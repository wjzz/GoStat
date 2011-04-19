
{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Control.Arrow
import Data.Function
import Data.List

import DB
import Server
import SgfBatching

main :: IO ()
main = do
  -- putStrLn "Starting DB rebuilding..."
  -- deleteDB
  -- createDB
  -- addFilesToDB
  -- putStrLn "DB rebuilding done!"
  server  


