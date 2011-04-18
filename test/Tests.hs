{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Test.Framework (defaultMain, Test)

import Data.SGF.Types.Tests
import Data.SGF.Parsing.Tests
import SgfBatching.Tests  
import Transformations.Tests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ data_sgf_types_tests,
          data_sgf_parsing_tests,
          sgfbatching_tests,
          transformations_tests
        ]