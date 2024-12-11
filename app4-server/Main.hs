{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent.Chan
import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions
import Web.Scotty

import qualified Lib3
import qualified Lib2
import GHC.Conc.Sync (newTVarIO)
import Control.Concurrent (forkIO)
import GHC.Conc (TVar)


main :: IO ()
main = do
  chan <- newChan :: IO (Chan Lib3.StorageOp)
  state <- newTVarIO Lib2.emptyState
  _ <- forkIO $ Lib3.storageOpLoop chan
  scotty 3000 $
    post "/" $ do
        b <- body
        liftIO $ putStrLn $ concat ["Request was: ", cs b] 
        result <- liftIO $ cmd state (cs b) chan
        text $ cs result


----------
maybeToString :: Maybe String -> String
maybeToString (Just s) = s
maybeToString Nothing = "OK"



cmd :: TVar Lib2.State -> String -> Chan Lib3.StorageOp -> IO String
cmd st str chan = do
  case Lib3.parseCommand str of
    Left e -> return e 
    Right (c, "") -> do
      tr <- Lib3.stateTransition st c chan
      case tr of
        Left e2 -> return e2
        Right m -> return $ maybeToString m
    Right (_, r) -> return $ "PARSE ERROR: string is not fully consumed - " ++ r