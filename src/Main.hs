{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import System.Environment(getArgs)

import Configuration
import DB
import Control.Monad.Trans
import Server
--import Offline

main :: IO ()
main = do
  args <- getArgs
  
  configM <- readConfig configurationPath
  config  <- 
    case configM of
      Left err -> do
        putStrLn err
        return defaultConfig
      Right c -> return c
      
  putStrLn "Current configuration:"
  print config
  
  case args of
    ("rebuild":_) -> do
      putStrLn "Starting DB rebuilding..."
      
      runGoStatM config $ do
        deleteDB
        liftIO $ putStrLn "Deleted DB."
        createDB
        liftIO $ putStrLn "Created DB."
        addFilesToDB
        liftIO $ putStrLn "DB rebuilding done!"
        server  
        
{-    ("archive":levelStr:_) -> do
      let level= read levelStr :: Int
      putStrLn $ "Building offline version upto level " ++ levelStr
      buildOffline level
      putStrLn $ "Offline files created."
-}
    _ -> runGoStatM config server