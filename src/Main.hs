{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Control.Arrow
import Data.Function
import Data.List

import Data.SGF.Parsing
import SgfBatching
import Transformations

main :: IO ()
main = --putStrLn "Hello, World!"
  loadSGFs >>= \l -> print (sortBy (compare `on` snd) $ map (head &&& length) $ group $ sort $ map (head . moves) l)
