{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Data.SGF.Parsing.Tests where

--------------------------
--  Testing frameworks  --
--------------------------

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
--import Test.QuickCheck
--import Test.Framework.Providers.QuickCheck (testProperty)

import System.IO.Unsafe

import Data.SGF.Types
import Data.SGF.Parsing

-------------
--  Tests  --
-------------

-- instance Arbitrary Color where
--   arbitrary = do
--     c <- choose (0::Int,1)
--     return $ if c == 0 then Black else White
  
--   coarbitrary = error "color:coarbitrary: Not implemented"

isRight :: Either a b -> Bool
isRight (Right _ ) = True
isRight _          = False

test_right, test_len, test_black, test_white :: Assertion

test_right = unsafePerformIO (print sgf) `seq` (isRight sgf) @?= True

test_len = Right 46 @?= (length . moves) `fmap` sgf
   
test_black = Right "Andrew87" @?= black `fmap` sgf

test_white = Right "masec" @?= white `fmap` sgf



data_sgf_parsing_tests :: Test.Framework.Test
data_sgf_parsing_tests = testGroup "Data.SGF.Parsing" 
                         [ testCase "parseSGF no parsing error" test_right
                         , testCase "parseSGF no of moves" test_len
                         , testCase "parseSGF black" test_black
                         , testCase "parseSGF white" test_white
                         ]

example_sgf :: String
example_sgf = unsafePerformIO $ readFile "/home/wjzz/Dropbox/Programy/Haskell/GoStat/test/example.sgf"

sgf :: Either String SGF
sgf = parseSGF example_sgf

-- tst_color = testGroup "Board.Color" 
--             [ testProperty "otherColor1"  prop_otherColor
--             ]
