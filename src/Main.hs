{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import System.Environment(getArgs)

import DB
import Server
import Offline

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ("rebuild":_) -> do
      putStrLn "Starting DB rebuilding..."
      deleteDB
      createDB
      addFilesToDB
      putStrLn "DB rebuilding done!"
      server  
    ("archive":levelStr:_) -> do
      let level= read levelStr :: Int
      putStrLn $ "Building offline version upto level " ++ levelStr
      buildOffline level
      putStrLn $ "Offline files created."
    _ -> server