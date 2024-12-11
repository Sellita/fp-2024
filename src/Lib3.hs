{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    Statements (..),
    Command (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    ) where

import Control.Monad.Trans.State.Strict (StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Concurrent ( Chan )
import Control.Concurrent.STM(STM, TVar, readTVar)
import qualified Lib2
import Control.Concurrent.Chan (writeChan, newChan)

import Control.Monad.Trans.Class(lift)
import qualified Data.Char as C 
import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (writeTVar)
import Control.Concurrent (readChan)

filename :: String
filename = "./data-lib3"

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
    v <- readChan chan
    _ <- case v of 
       Load c -> do
           a <- readFile filename
           putStrLn $ "loaded from file: " ++ a
           writeChan c a
       Save dataToSave _ -> do
          putStrLn $ "saving to file: " ++ dataToSave
          writeFile filename dataToSave
    storageOpLoop chan
--storageOpLoop _ = do
--  return $ error "Not implemented 1"

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
--parseCommand _ = Left "Not implemented 2"
parseCommand "" = Left "Can't parse command from empty string"

parseCommand input = 
    case runState (runExceptT parseLine) input of
        (Right "load", restLoad) -> Right (LoadCommand, restLoad)
        (Right "save", restSave) -> Right (SaveCommand, restSave)
        (Left err, _) -> Left err
        (Right "Begin", restBegin) -> case parseStatements restBegin
            of
                Right(statements, rest) -> Right (StatementCommand statements, rest)
                Left err -> Left err
        _ -> case parseStatements input
            of
                Right(statements, rest) -> Right (StatementCommand statements, rest)
                Left err -> Left err

-- maybe2 :: (a -> String -> c) -> Lib2.Parser a -> Lib2.Parser String -> Lib2.Parser c
-- maybe2 f a b = \input -> 
--     case a input of
--         Right (v1, r1) -> Right (f v1 r1, "")
--         Left e1 -> 
--             case b input of
--                 Right (v2, r2) -> Right (f "" r2, "")
--                 Left e2 -> Left e1
    -- case notEnter of
    --     Right (v1, r1) -> Right (f v1 r1, "")
    --     Left e1 -> 
    -- case a input of
    --     Right (v1, r1) ->
    --         case b r1 of
    --             Right (v2, r2) -> Right (f v1 "", r2)
    --             Left e2 -> Right (f v1 "", r1)
    --     Left e1 -> Left e1


parseLine :: Lib2.Parser String
parseLine = do 
    notEnter <- Lib2.parseWithRule isNotEnter
    _ <- Lib2.maybeParseWithRule isEnter
    return notEnter
                -- (Lib2.parseWithRule isNotEnter) 
                -- (Lib2.parseWithRule isEnter)
    

isNotLetter :: Char -> Bool
isNotLetter c = not $ C.isLetter c

isEnter :: Char -> Bool
isEnter c = c == '\n'

isNotEnter :: Char -> Bool
isNotEnter c = not $ isEnter c

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements "" = Left "Nothing to parse"
parseStatements input = 
    case splitToLines input of
        Right comandsArr -> 
            case comandsArr of
                [] -> Left "Nothing to parse"
                (h:t) -> 
                    case t of
                        [] -> parseQueryToStatement $ Lib2.parseQuery h
                        _ -> case last t of 
                            "End" -> case init comandsArr of
                                [] -> Left "End not found"
                                initArr -> parseBunchQureies $ parseQuerryMap initArr
                            _ -> Left "Bunch should end with End"
        Left err -> Left err

parseQuerryMap :: [String] -> Either String [Lib2.Query]
parseQuerryMap [] = Right []
parseQuerryMap ["End"] = Left "unexpected end"
parseQuerryMap (h:t) = case Lib2.parseQuery h of
    Right query -> 
        case parseQuerryMap t of
            Right queries -> Right $ [query] ++ queries
            Left err -> Left err
    Left err -> Left err

parseQueryToStatement :: Either String Lib2.Query -> Either String (Statements, String)
parseQueryToStatement (Left err) = Left err
parseQueryToStatement (Right query) = Right (Single query, "")

parseBunchQureies :: Either String [Lib2.Query] -> Either String (Statements, String)
parseBunchQureies (Left err) = Left err
parseBunchQureies (Right q) = Right (Batch q, "")



splitToLines :: String -> Either String [String]
splitToLines p = splitToLines' p []
    where
        splitToLines' :: String -> [String] -> Either String [String]
        splitToLines' p' acc = 
            case runState (runExceptT parseLine) p' of
                (Right v, "\n") -> Right (acc ++ [v])
                (Right v, "") -> Right (acc ++ [v])
                (Right v, res) -> splitToLines' res (acc ++ [v])
                (Left err, _) -> Left err

            


-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State garage) = carGarageToStatment garage


carGarageToStatment :: Lib2.CarGarage -> Statements
carGarageToStatment [] = Batch []
carGarageToStatment garage = carGarageToStatment' garage (Batch [])

carGarageToStatment' :: [Lib2.Car] -> Statements -> Statements
carGarageToStatment' [] statments = statments
carGarageToStatment' (car:rest) statments = 
    let 
        (a, b, c, d) = car
        carStatment = Lib2.Car a b c d
    in
        case statments of
            Batch s -> carGarageToStatment' rest $ Batch $ s ++ [carStatment]
            Single singleStatement -> Batch [singleStatement, carStatment]

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
--renderStatements _ = error "Not implemented 5"
renderStatements (Single s) = show s
renderStatements (Batch []) = ""
renderStatements (Batch st) = "Begin\n" ++ renderInnerStatements (Batch st) ++ "End"

renderInnerStatements :: Statements -> String
renderInnerStatements (Single s) = show s
renderInnerStatements (Batch []) = ""
renderInnerStatements (Batch (h:t)) = show h ++ "\n" ++ renderInnerStatements (Batch t)


-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition state LoadCommand ioChan = do
    wChan <- newChan 
    writeChan ioChan $ Load wChan
    res <- readChan wChan
    let c = parseCommand res
    case c of 
        Left err -> do
            return $ Left err
        Right (StatementCommand statements, _) -> do
            putStrLn $ "passing to transition statements: " ++ show statements
            stateTransition state (StatementCommand statements) ioChan
        Right _ -> do
            return $ Left "Load and save commands should'nt be written in file"
    
stateTransition state SaveCommand ioChan = do
    wChan <- newChan 
    st <- readTVarIO state
    putStrLn $ "save st: " ++ show st
    writeChan ioChan $ Save (renderStatements (marshallState st)) wChan
    return $ Right Nothing
stateTransition state (StatementCommand statements) _ = 
    case statements of
        Single s -> atomically $ do
            st <- readTVar state
            let newState = Lib2.stateTransition st s
            case newState of
                Left err -> return $ Left err
                Right (info, state') -> do
                    _ <- writeTVar state state'
                    return $ Right info
        Batch [] -> return $ Left "No comand found"
        Batch comands -> atomically $ do
            st <- readTVar state
            let newState = applyQuerries comands st
            case newState of
                Left err -> return $ Left err
                Right (msg, state') -> do
                    _ <- writeTVar state state'
                    return $ Right msg

applyQuerries :: [Lib2.Query] -> Lib2.State -> Either String (Maybe String, Lib2.State)

applyQuerries [] state = Right(Nothing, state)
applyQuerries (h:t) state = applyQuerries t $ extractStateFromEither $ Lib2.stateTransition state h

extractStateFromEither :: Either String (Maybe String, Lib2.State) -> Lib2.State
extractStateFromEither (Right (_, state)) = state
extractStateFromEither (Left err) = error err


--stateTransition _ _ ioChan = return $ Left "Not implemented 6"
