{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Control.Arrow
import Data.Function
import Data.List

import SgfBatching

main :: IO ()
main = --putStrLn "Hello, World!"
  loadSGFs >>= \l -> print (sortBy (compare `on` snd) $ map (head &&& length) $ group $ sort $ map (head . moves) l)
