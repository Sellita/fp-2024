{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Control.Monad.Free (Free (..), liftF)
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)

import Data.ByteString
import Network.Wreq ( post, responseBody )
import Data.String.Conversions ( cs )
import Control.Lens
import qualified Lib2
import Control.Monad.IO.Class (liftIO)
import System.Console.Repline (ReplOpts(command))
import qualified Lib3
import Control.Concurrent (Chan)
import GHC.Conc (TVar)
import GHC.Conc.Sync
import Control.Concurrent.Chan

main :: IO ()
main = do
    t <- readCommand
    putStrLn t
    putStrLn "Done"


--------

sendCommand :: String -> IO String
sendCommand request = do
    let rawRequest = cs request :: ByteString
    putStrLn $ "Sending request:\n" ++ cs rawRequest
    resp <- post "http://localhost:3000" rawRequest
    return $ cs $ resp ^. responseBody

readCommand :: IO String
readCommand = do
    putStrLn "Enter command: "
    comandNumber <- readLn
    case comandNumber of
        1 -> do oneByOne program
        2 -> do 
            _ <- runStateT (allInOne program) []
            return "Batch command executed"
        3 -> do 
            _ <- runStateT (noRequestInterpretator program) []
            return "no request command executed"
        _ -> return "unknown command"

processSingleCommand :: String -> IO String
processSingleCommand str = do sendCommand str

processSingleCommandNoRequest :: String -> IO String
processSingleCommandNoRequest str = do 
    print str
    return str


processBatchCommandNoRequest :: [String] -> IO (Maybe String)
processBatchCommandNoRequest [] = return Nothing
processBatchCommandNoRequest [single] = Just <$> processSingleCommandNoRequest single
processBatchCommandNoRequest batch = Just <$> processSingleCommandNoRequest (renderStatements batch)

processBatchCommand :: [String] -> IO (Maybe String)
processBatchCommand [] = return Nothing
processBatchCommand [single] = Just <$> processSingleCommand single
processBatchCommand batch = Just <$> processSingleCommand (renderStatements batch)

renderStatements :: [String] -> String
renderStatements [] = ""
renderStatements [single] = single
renderStatements t = "Begin\n" ++ renderInnerStatements t ++ "\nEnd"

renderInnerStatements :: [String] -> String
renderInnerStatements [] = ""
renderInnerStatements [single] = single
renderInnerStatements  (h:t) = h ++ "\n" ++ renderInnerStatements t


----------------
data MyDomainAlgebra next =  Car String String Integer String (() -> next)
                          | RemoveCar Integer (() -> next)
                          | GetList (String -> next)
                          | Save (() -> next)
                          | Load (() -> next)
                          deriving Functor

type MyDomain = Free MyDomainAlgebra

car :: String -> String -> Integer -> String -> MyDomain ()
car brand model year collor = liftF $ Car brand model year collor id

removeCar :: Integer -> MyDomain ()
removeCar i = liftF $ RemoveCar i id

getList :: MyDomain String
getList = liftF $ GetList id

save :: MyDomain ()
save = liftF $ Save id

load :: MyDomain ()
load = liftF $ Load id

program :: MyDomain String
program = do
    load
    car "BMW" "I3" 1998 "Gray"
    car "Audi" "A4" 2003 "Green"
    car "Toyota" "Corrola" 2019 "Blue"        
    removeCar 1
    listOfCars  <- getList
    save
    return listOfCars

oneByOne :: MyDomain a -> IO a
oneByOne (Pure a) = return a
oneByOne (Free step) = do
    next <- runStep step
    oneByOne next
    where
        runStep :: MyDomainAlgebra a -> IO a
        runStep (Car brand model year collor next) = do
            v <- processSingleCommand $ show $ Lib2.Car brand model year collor
            putStrLn v
            return $ next ()
        runStep (RemoveCar i next) = do
            v <- processSingleCommand $ show $ Lib2.RemoveCar i
            putStrLn v
            return $ next ()
        runStep (GetList next) = do
            v <- processSingleCommand $ show Lib2.GetCarList
            putStrLn v
            return $ next v
        runStep (Load next) = do
            v <- processSingleCommand "load"
            putStrLn v
            return $ next ()
        runStep (Save next) = do
            v <- processSingleCommand "save"
            putStrLn v
            return $ next ()

allInOne :: MyDomain a -> StateT [String] IO a
allInOne (Pure a) = do
    bat <- get
    _ <- liftIO $ processBatchCommand bat
    return a
allInOne (Free step) = do
    next <- runStep step
    allInOne next
    where
        runStep :: MyDomainAlgebra a -> StateT [String] IO a
        runStep (Car brand model year collor next) = do
            arr <- get
            put (arr ++ [show $ Lib2.Car brand model year collor])
            return $ next ()
        runStep (RemoveCar i next) = do
            arr <- get
            put (arr ++ [show $ Lib2.RemoveCar i])
            return $ next ()
        runStep (GetList next) = do
            arr <- get
            _ <- liftIO $ processBatchCommand arr
            put []
            v <- liftIO $ processSingleCommand $ show Lib2.GetCarList
            liftIO $ putStrLn v
            return $ next (v)
        runStep (Load next) = do
            arr <- get
            _ <- liftIO $ processBatchCommand arr
            put []
            v <- liftIO $ processSingleCommand "load"
            liftIO $ putStrLn v
            return $ next ()
        runStep (Save next) = do
            arr <- get
            _ <- liftIO $ processBatchCommand arr
            put []
            v <- liftIO $ processSingleCommand "save"
            liftIO $ putStrLn v
            return $ next ()

noRequestInterpretator :: MyDomain a -> StateT [String] IO a
noRequestInterpretator (Pure a) = do
    bat <- get
    _ <- liftIO $ processBatchCommandNoRequest bat
    return a
noRequestInterpretator (Free step) = do
    next <- runStep step
    noRequestInterpretator next
    where
        runStep :: MyDomainAlgebra a -> StateT [String] IO a
        runStep (Car brand model year collor next) = do
            arr <- get
            put (arr ++ [show $ Lib2.Car brand model year collor])
            return $ next ()
        runStep (RemoveCar i next) = do
            arr <- get
            put (arr ++ [show $ Lib2.RemoveCar i])
            return $ next ()
        runStep (GetList next) = do
            arr <- get
            _ <- liftIO $ processBatchCommandNoRequest arr
            put []
            v <- liftIO $ processSingleCommandNoRequest $ show Lib2.GetCarList
            return $ next (v)
        runStep (Load next) = do
            arr <- get
            _ <- liftIO $ processBatchCommandNoRequest arr
            put []
            _ <- liftIO $ processSingleCommandNoRequest "load"
            return $ next ()
        runStep (Save next) = do
            arr <- get
            _ <- liftIO $ processBatchCommandNoRequest arr
            put []
            _ <- liftIO $ processSingleCommandNoRequest "save"
            return $ next ()