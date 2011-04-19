{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Transformations.Tests where

--------------------------
--  Testing frameworks  --
--------------------------

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck (testProperty)

import System.IO.Unsafe

import Data.SGF.Types
import Transformations

-------------
--  Tests  --
-------------

--------------------
--  some helpers  --
--------------------

isRight :: Either a b -> Bool
isRight (Right _ ) = True
isRight _          = False

inBounds :: Int -> Bool
inBounds n = 1 <= n && n <= 9

moveInBounds :: Move -> Bool
moveInBounds (a,b) = inBounds a && inBounds b

-----------------
--  normalize  --
----------------

normalize :: Move -> Move
normalize (a,b) = (norm a, norm b) where
  norm x = let ab = abs x `mod` 9 
           in if ab == 0 then 9 else ab

property_normalize_in_bounds :: Move -> Bool
property_normalize_in_bounds = moveInBounds . normalize

--------------------------------
--  isInside & findTriangles  --
--------------------------------

test_isInside_center :: Assertion
test_isInside_center = (unsafePerformIO printTriangles) `seq` triangles @=? findTriangles (5,5)

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

isInside_tests :: [Test.Framework.Test]
isInside_tests = [ testCase "center is inside all"          test_isInside_center
                 , testProperty "normalize inbounds"        property_normalize_in_bounds
                 , testProperty "non center inside max 2"   property_non_center_isInside_most_two
                 , testProperty "diagonal inside >= 2"      property_only_diagonal_isInside_two 
                 , testProperty "main diagonal inside >= 2" property_main_diagonal_has_two 
                 , testProperty "non diagonal inside = 1"   property_only_diagonal_isInside_two2
                 , testCase "every point has a triangle"    test_all_points_have_triangle
                 ]

----------------------------------------
--  findTriangle for the whole board  --
----------------------------------------

printTriangles :: IO ()
printTriangles = mapM_ printRow [1..9]

printRow :: Int -> IO ()
printRow y = do
  mapM_ printCell $ zip (reverse [1..9]) (repeat y)
  putStrLn "" where
    printCell p = putStr $ show (findTriangle p) ++ " "
    
-----------------------------------
--  isOn/Above/Below predicates  --
-----------------------------------

property_isOnHorizontal_trichtomy :: Move -> Bool
property_isOnHorizontal_trichtomy p = 1 == length (filter ($ m) preds) where
  m = normalize p
  preds = [isOnHorizontal, isAboveHorizontal, isBelowHorizontal]
  
property_isOnMainDiagonal_trichtomy :: Move -> Bool
property_isOnMainDiagonal_trichtomy p = 1 == length (filter ($ m) preds) where
  m = normalize p
  preds = [isOnMainDiagonal, isAboveMainDiagonal, isBelowMainDiagonal]
  

isOnAboveBelow_tests :: [Test.Framework.Test]  
isOnAboveBelow_tests = [ testProperty "horizontal trichtomy"   property_isOnHorizontal_trichtomy
                       , testProperty "mainDiagonal trichtomy" property_isOnMainDiagonal_trichtomy
                       ]

---------------------------------
--  horizontal transformation  --
---------------------------------

property_horizontal_fixpoints :: Int -> Bool
property_horizontal_fixpoints x = horizontal m == m where
  m = normalize (x, 5)
  
property_horizontal_non_fixpoints :: Move -> Property
property_horizontal_non_fixpoints p = y /= 5 ==> horizontal m /= m where
  m@(_,y) = normalize p

property_horizontal_involutive :: Move -> Bool
property_horizontal_involutive p = m == horizontal (horizontal m) where
  m = normalize p
  
test_horizontal_edge :: Assertion
test_horizontal_edge = (1,1) @?= horizontal (1,9)

property_horizontal_inbounds :: Move -> Bool
property_horizontal_inbounds m = moveInBounds (horizontal (normalize m))

horizontal_tests :: [Test.Framework.Test]
horizontal_tests = [ testProperty "horizontal fixpoints"      property_horizontal_fixpoints
                   , testProperty "horizontal non fixpoints"  property_horizontal_non_fixpoints
                   , testProperty "horizontal involutive"     property_horizontal_involutive
                   , testCase     "horizontal example"        test_horizontal_edge
                   , testProperty "horizontal inbounds"       property_horizontal_inbounds
                   ]

-------------------------------
--  rotate90 transformation  --
-------------------------------

test_rotate90_fixpoint :: Assertion
test_rotate90_fixpoint = (5,5) @?= rotate90 (5,5)

property_rotate90_nonfixpoint :: Move -> Property
property_rotate90_nonfixpoint p = m /= (5,5) ==> m /= rotate90 m where
  m = normalize p
  
property_rotate90_4_times :: Move -> Bool
property_rotate90_4_times p = m == rotate90 (rotate90 (rotate90 (rotate90 m))) where
  m = normalize p
  
-- a single rotation changes the triangle by 2
property_rotate90_triangle :: Move -> Property
property_rotate90_triangle p = before > 2 ==> before `mod` 8 == after `mod` 8 where
  before = findTriangle m
  after  = findTriangle (rotate90 m) + 2
  m = normalize p

rotate90_tests :: [Test.Framework.Test]
rotate90_tests = [ testCase     "rotate90 fixpoint"         test_rotate90_fixpoint  
                 , testProperty "rotate90 non fixpoint"     property_rotate90_nonfixpoint
                 , testProperty "rotate90 4 times "         property_rotate90_4_times
                 , testProperty "rotate90 and triangle "    property_rotate90_triangle
                 ]  

--------------------------
--  mainDiagonalMirror  --
--------------------------

property_mainDiagonalMirror_fixpoints :: Move -> Property
property_mainDiagonalMirror_fixpoints p = isOnMainDiagonal m ==> m == (mainDiagonalMirror m) where
  m = normalize p

property_mainDiagonalMirror_above :: Move -> Property
property_mainDiagonalMirror_above p = isAboveMainDiagonal m ==> isBelowMainDiagonal (mainDiagonalMirror m) where
  m = normalize p

property_mainDiagonalMirror_below :: Move -> Property
property_mainDiagonalMirror_below p = isBelowMainDiagonal m ==> isAboveMainDiagonal (mainDiagonalMirror m) where
  m = normalize p

property_mainDiagonalMirror_involutive :: Move -> Bool
property_mainDiagonalMirror_involutive p = m ==  mainDiagonalMirror (mainDiagonalMirror m) where
  m = normalize p

mainDiagonalMirror_tests :: [Test.Framework.Test]
mainDiagonalMirror_tests = [ testProperty "mainDiagonalMirror fixpoints"  property_mainDiagonalMirror_fixpoints
                           , testProperty "mainDiagonalMirror above"      property_mainDiagonalMirror_above
                           , testProperty "mainDiagonalMirror below"      property_mainDiagonalMirror_below
                           , testProperty "mainDiagonalMirror involutive" property_mainDiagonalMirror_involutive
                           ]

-------------------------
--  getTransformation  --
-------------------------

property_getTransformation_gives_first :: Move -> Bool
property_getTransformation_gives_first p = 1 == findTriangle (f m) where
  m = normalize p
  f = getTransformation m

getTransformation_tests :: [Test.Framework.Test]
getTransformation_tests = [ testProperty "getTransformation gives first" property_getTransformation_gives_first
                          ]
----------------------
--  normalizeMoves  --
----------------------

property_normalizeMoves_head :: [Move] -> Property
property_normalizeMoves_head ps = not (null ms) ==> 1 == findTriangle (head (normalizeMoves ms)) where
  ms = map normalize ps

property_normalizeMoves_length :: [Move] -> Bool
property_normalizeMoves_length ps = length ms == length (normalizeMoves ms) where
  ms = map normalize ps

property_normalizeMoves_symmetry_diag :: [Move] -> Property
property_normalizeMoves_symmetry_diag ps = not (null ms) && not (null start) && not (null rest) 
                                           ==> isBelowMainDiagonal (head rest) where
  ms = map normalize ps
  (start, rest) = span isOnMainDiagonal $ normalizeMoves ms

property_normalizeMoves_symmetry_horiz :: [Move] -> Property
property_normalizeMoves_symmetry_horiz ps = not (null ms) && not (null start) && not (null rest) 
                                           ==> isBelowHorizontal (head rest) where
  ms = (5,5) : map normalize ps
  (start, rest) = span isOnHorizontal $ normalizeMoves ms


property_normalizeMoves_center_case :: [Move] -> Property
property_normalizeMoves_center_case ps = not (null ps) ==> 1 == findTriangle second where
  ms = (5,5) : map normalize ps
  (_:second:_) = normalizeMoves ms

normalizeMoves_tests :: [Test.Framework.Test]
normalizeMoves_tests = [ testProperty "normalizeMoves head triangle"     property_normalizeMoves_head
                       , testProperty "normalizeMoves length presev"     property_normalizeMoves_length
                       , testProperty "normalizeMoves diagonal symmetry handling" property_normalizeMoves_symmetry_diag
                       , testProperty "normalizeMoves horizont symmetry handling" property_normalizeMoves_symmetry_horiz
                       , testProperty "normalizeMoves start:55 handling" property_normalizeMoves_center_case
                       ]

-----------------
--  All tests  --
-----------------

transformations_tests :: Test.Framework.Test
transformations_tests = testGroup "Transformations" $ concat [ isInside_tests
                                                             , isOnAboveBelow_tests
                                                             , horizontal_tests
                                                             , rotate90_tests
                                                             , getTransformation_tests
                                                             , mainDiagonalMirror_tests
                                                             , normalizeMoves_tests
                                                             ]