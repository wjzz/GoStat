{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Data.SGF.Parsing.Tests where

--------------------------
--  Testing frameworks  --
--------------------------

import Data.Char
import Data.Maybe

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck (testProperty)

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

test_right, test_len, test_black, test_white, test_winner :: Assertion

test_right  = unsafePerformIO (print sgf >> print (sgfSummary `fmap` sgf)) 
              `seq` (isRight sgf) @?= True
test_len    = Right 46 @?= (length . moves) `fmap` sgf
test_black  = Right "Andrew87" @?= black `fmap` sgf
test_white  = Right "masec" @?= white `fmap` sgf
test_winner = Right (Just "masec" ) @?= getWinnerName `fmap` sgf

-- with this definition we will only get char from 'a' to 'i'
instance Arbitrary Char where
  arbitrary = do
    c <- choose (0::Int, 8)
    return $ chr (ord 'a' + c)

-- test if we always return a Just given two Justs
property_maybeMove_isJust :: (Char, Char) -> Bool
property_maybeMove_isJust (f,s) = {-(f `elem` ['a'..'i'] && s `elem` ['a'..'i']) ==> -} 
  isJust $ maybeMove (Just f, Just s)

inBounds :: Int -> Bool
inBounds n = 1 <= n && n <= 9

moveInBounds :: Move -> Bool
moveInBounds (a,b) = inBounds a && inBounds b

-- test if we always return a move within bounds
property_maybeMove_inBounds :: (Char, Char) -> Bool
property_maybeMove_inBounds (f,s) = moveInBounds $ fromJust $ maybeMove (Just f, Just s)

-- test if the corner cases are handled correctly
test_maybeMove1, test_maybeMove2, test_maybeMove3, test_maybeMove4 :: Assertion
test_maybeMove1 = Just (1,1) @?= maybeMove (Just 'i', Just 'a')
test_maybeMove2 = Just (1,9) @?= maybeMove (Just 'i', Just 'i')
test_maybeMove3 = Just (9,1) @?= maybeMove (Just 'a', Just 'a')
test_maybeMove4 = Just (9,9) @?= maybeMove (Just 'a', Just 'i')

data_sgf_parsing_tests :: Test.Framework.Test
data_sgf_parsing_tests = testGroup "Data.SGF.Parsing" 
                         [ testCase "parseSGF no parsing error" test_right
                         , testCase "parseSGF no of moves" test_len
                         , testCase "parseSGF black" test_black
                         , testCase "parseSGF white" test_white
                         , testCase "parseSGF winner" test_winner
                         , testProperty "maybeMove inbounds"   property_maybeMove_isJust
                         , testProperty "maybeMove inbounds 2" property_maybeMove_inBounds
                         , testCase "maybeMove Corner 1" test_maybeMove1
                         , testCase "maybeMove Corner 2" test_maybeMove2
                         , testCase "maybeMove Corner 3" test_maybeMove3                           
                         , testCase "maybeMove Corner 4" test_maybeMove4
                         ]

example_sgf :: String
example_sgf = unsafePerformIO $ readFile "/home/wjzz/Dropbox/Programy/Haskell/GoStat/test/example.sgf"

sgf :: Either String SGF
sgf = parseSGF example_sgf

-- tst_color = testGroup "Board.Color" 
--             [ testProperty "otherColor1"  prop_otherColor
--             ]
