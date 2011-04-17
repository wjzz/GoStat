{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Board.Color where

--------------------------
--  Testing frameworks  --
--------------------------

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.QuickCheck

--------------------------
--  The Color datatype  --
--------------------------

-- |Represents the color of a piece/player.
-- We follow the convention of calling Sente black and Gote white.
data Color = Black | White
             deriving (Eq, Show)

-- |Returns the one letter (lowercase) name of the color.
--
-- @
--   shortName Black == \"b\"
--   shortName White == \"w\"
-- @
shortName :: Color -> String
shortName Black = "b"
shortName White = "w"

-- |Returns the \"opposite\" color, that is the other color.
otherColor :: Color -> Color
otherColor Black = White
otherColor White = Black

-- |Checks if the given color is Black.
isBlack :: Color -> Bool
isBlack = (== Black)

-- |Checks if the given color is White.
isWhite :: Color -> Bool
isWhite = (== White)

------------------------------
--  Pretty printing colors  --
------------------------------

prettyColor :: Color -> String
prettyColor = shortName

---------------
--  Testing  --
---------------

instance Arbitrary Color where
  arbitrary = do
    c <- choose (0::Int,1)
    return $ if c == 0 then Black else White
  
  coarbitrary = error "color:coarbitrary: Not implemented"
  

prop_otherColor c = 
  c == (otherColor (otherColor c))

prop_otherColor2 c =
  c /= otherColor c

prop_isFunctions c =
  isWhite c || isBlack c
  
prop_isFunctions2 c =
  not $ isWhite c && isBlack c

-- the whole package of color tests
tst_color = testGroup "Board.Color" 
            [ testProperty "otherColor1"  prop_otherColor
            , testProperty "otherColor2"  prop_otherColor2
            , testProperty "isBlackWhite" prop_isFunctions
            , testProperty "isBlackWhite" prop_isFunctions2              
            ]
