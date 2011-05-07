{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import System.Environment(getArgs)

import DB
import Server

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
    ("archive":level:_) -> do
      let n = read level :: Int
      putStrLn $ "Building offline version upto level " ++ level
      putStrLn $ "Offline files created."
    _ -> server