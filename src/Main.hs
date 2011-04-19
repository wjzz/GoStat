
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
  putStrLn "Hello, World!"
  server
--  deleteDB
--  createDB
--  addFilesToDB
  --loadSGFs >>= \l -> print (sortBy (compare `on` snd) $ map (head &&& length) $ group $ sort $ map (head . moves) l)
  --loadSGFs >>= \l -> print (length l)

