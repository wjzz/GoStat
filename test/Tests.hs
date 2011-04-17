{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit


main :: IO ()
main = defaultMain tests

tests = []
-- tests = [ tst_color
--         , tst_piece
--         ]