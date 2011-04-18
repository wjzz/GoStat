{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Transformations.Tests where

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
import Transformations

-------------
--  Tests  --
-------------

isRight :: Either a b -> Bool
isRight (Right _ ) = True
isRight _          = False

--test_right, test_len, test_black, test_white, test_winner :: Assertion


-- -- test if we always return a Just given two Justs
-- property_maybeMove_isJust :: (Char, Char) -> Bool
-- property_maybeMove_isJust (f,s) = {-(f `elem` ['a'..'i'] && s `elem` ['a'..'i']) ==> -} 
--   isJust $ maybeMove (Just f, Just s)

inBounds :: Int -> Bool
inBounds n = 1 <= n && n <= 9

moveInBounds :: Move -> Bool
moveInBounds (a,b) = inBounds a && inBounds b

-- -- test if we always return a move within bounds
-- property_maybeMove_inBounds :: (Char, Char) -> Bool
-- property_maybeMove_inBounds (f,s) = moveInBounds $ fromJust $ maybeMove (Just f, Just s)

-- -- test if the corner cases are handled correctly
-- test_maybeMove1, test_maybeMove2, test_maybeMove3, test_maybeMove4 :: Assertion
-- test_maybeMove1 = Just (1,1) @?= maybeMove (Just 'i', Just 'a')
-- test_maybeMove2 = Just (1,9) @?= maybeMove (Just 'i', Just 'i')
-- test_maybeMove3 = Just (9,1) @?= maybeMove (Just 'a', Just 'a')
-- test_maybeMove4 = Just (9,9) @?= maybeMove (Just 'a', Just 'i')

test_isInside_center :: Assertion

test_isInside_center = (unsafePerformIO printTriangles) `seq` triangles @=? findTriangles (5,5)

normalize :: Move -> Move
normalize (a,b) = (norm a, norm b) where
  norm a = let ab = abs a `mod` 9 
           in if ab == 0 then 9 else ab

property_normalize_in_bounds :: Move -> Bool
property_normalize_in_bounds = moveInBounds . normalize

property_non_center_isInside_most_two :: Move -> Property
property_non_center_isInside_most_two m = normalize m /= (5,5) ==> 2 >= length (findTriangles (normalize m))

property_only_diagonal_isInside_two :: Move -> Property
property_only_diagonal_isInside_two m = 
  (x+y==10 || x==y || x == 5 || y == 5) ==> 2 <= length (findTriangles (x,y)) where
  (x,y) = normalize m

property_only_diagonal_isInside_two2 :: Move -> Property
property_only_diagonal_isInside_two2 m = 
  not (x+y==10 || x==y || x == 5 || y == 5) ==> 1 == length (findTriangles (x,y)) where
  (x,y) = normalize m
  
test_all_points_have_triangle :: Assertion
test_all_points_have_triangle = True @=? all (not . null) [findTriangles (x,y) | x <- [1..9] , y <- [1..9]]

property_main_diagonal_has_two :: Move -> Property
property_main_diagonal_has_two m = isOnMainDiagonal (normalize m) ==> 2 <= length (findTriangles (normalize m))

printTriangles :: IO ()
printTriangles = mapM_ printRow [1..9]

printRow :: Int -> IO ()
printRow y = do
  mapM_ printCell $ zip (reverse [1..9]) (repeat y)
  putStrLn "" where
    printCell p = putStr $ show (findTriangle p) ++ " "

transformations_tests :: Test.Framework.Test
transformations_tests = testGroup "Transformations" 
                         [ testCase "center is inside all" test_isInside_center
                         , testProperty "normalize inbounds"        property_normalize_in_bounds
                         , testProperty "non center inside max 2"   property_non_center_isInside_most_two
                         , testProperty "diagonal inside >= 2"      property_only_diagonal_isInside_two 
                         , testProperty "main diagonal inside >= 2" property_main_diagonal_has_two 
                         , testProperty "non diagonal inside = 1"   property_only_diagonal_isInside_two2
                         , testCase "every point has a triangle"    test_all_points_have_triangle
                         -- , testCase "parseSGF no of moves" test_len
                         -- , testCase "parseSGF black" test_black
                         -- , testCase "parseSGF white" test_white
                         -- , testCase "parseSGF winner" test_winner

                         -- , testProperty "maybeMove inbounds 2" property_maybeMove_inBounds
                         ]

