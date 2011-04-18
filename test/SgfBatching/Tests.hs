{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module SgfBatching.Tests where

--------------------------
--  Testing frameworks  --
--------------------------

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
--import Test.QuickCheck
--import Test.Framework.Providers.QuickCheck (testProperty)

import Data.Maybe
import System.IO.Unsafe

import Data.SGF.Types hiding (moves)
import Data.SGF.Parsing
import SgfBatching

-------------
--  Tests  --
-------------

test_just :: Assertion
test_just = Right True @?= (fmap (isJust . sgfToGameInfo undefined) sgf)

sgfbatching_tests :: Test.Framework.Test
sgfbatching_tests = testGroup "SgfBatching" 
                         [ testCase "sgfToGameInfo no parsing error" test_just
                         -- , testCase "parseSGF no of moves" test_len
                         -- , testCase "parseSGF black" test_black
                         -- , testCase "parseSGF white" test_white
                         -- , testCase "parseSGF winner" test_winner
                         ]

example_sgf :: String
example_sgf = unsafePerformIO $ readFile "/home/wjzz/Dropbox/Programy/Haskell/GoStat/test/example.sgf"

sgf :: Either String SGF
sgf = parseSGF example_sgf
