{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Configuration
import Control.Monad.Trans
import Server

main :: IO ()
main = do
  configM <- readConfig =<< configurationPath
  config  <- 
    case configM of
      Left err -> do
        putStrLn err
        return defaultConfig
      Right c -> return c
      
  putStrLn "Current configuration:"
  print config
  
  runGoStatM config server
    
{-
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
      
      runGoStatM config $ do
        rebuildDB
        server  
    _ -> runGoStatM config server
-}    
{-    ("archive":levelStr:_) -> do
      let level= read levelStr :: Int
      putStrLn $ "Building offline version upto level " ++ levelStr
      buildOffline level
      putStrLn $ "Offline files created."
-}
