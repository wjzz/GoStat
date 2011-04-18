{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Transformations where

import Data.SGF.Types

{- A coordinates & triangles map.

  91           51            11
    x   [7]    x     [8]   x     
      x        x        x 
   [6]   x     x   x       [1]
           x   x  x
  95 x x x x x 55 x x x x x   15             
            x  x  x 
    [5]  x     x    x  [2]
       x       x       x  
    x   [4]    x  [3]     x
  99           59            19

-}



-- | The number of a triangle.
-- | Has to be between 1 and 8
type Triangle = Int


-- | Checks if a given point is located in the given triangle
isInside :: Move -> Triangle -> Bool
isInside (x,y) 1 = x <= y && y <= 5 --
isInside (x,y) 2 = x <= y && y >= 5 && x+y <= 10 --
isInside (x,y) 3 = x <= y && x+y >= 10 && x <= 5 --
isInside (x,y) 4 = x <= y && x+y >= 10 && x >= 5 --
isInside (x,y) 5 = x >= y && y >= 5 --
isInside (x,y) 6 = x >= y && y <= 5 && x+y >= 10 --
isInside (x,y) 7 = x >= y && x+y <= 10 && x >= 5 -- 
isInside (x,y) 8 = x >= y && x <= 5 --
isInside _ _ = error "triangle has to be between 1 and 8"

triangles :: [Triangle]
triangles = [1..8]

findTriangles :: Move -> [Triangle]
findTriangles m = filter (isInside m) triangles

-- | Returns the number of the triangle a point is located in.
-- | If the point is on the boundary, then the triangle with the least number is returned
findTriangle :: Move -> Triangle
findTriangle = minimum . findTriangles

-- | Checks if the given move is on the main diagonal
-- The main diagonal:
-- ..X
-- .X.
-- X..
isOnMainDiagonal :: Move -> Bool
isOnMainDiagonal (x,y) = x == y

-- |Performs a horizontal mirror transformation
horizontal :: Move -> Move
horizontal (x,y) = (x, 10 - y)

-- |Performs a 90 degrees rotation (counter clock-wise)
rotate90 :: Move -> Move
rotate90 (x,y) = (10-y, x)

-- |Returns a transformation that will make any move in the given triangle
-- |to appear in the first triangle
transformIntoFirst :: Triangle -> (Move -> Move)
transformIntoFirst n 
  | n < 0 || n > 8 = error "triangle has to be between 1 and 8"
  | n == 1    = id
  | n == 2    = horizontal
  | otherwise = transformIntoFirst (n-2) . rotate90

getTransformation :: Move -> (Move -> Move)
getTransformation = transformIntoFirst . findTriangle

