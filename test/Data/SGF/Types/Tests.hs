{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Data.SGF.Types.Tests where

--------------------------
--  Testing frameworks  --
--------------------------

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck (testProperty)
import Test.Framework.Providers.HUnit (testCase)
import Test.QuickCheck
import Test.HUnit

--import Data.SGF.Types

-------------
--  Tests  --
-------------

-- instance Arbitrary Color where
--   arbitrary = do
--     c <- choose (0::Int,1)
--     return $ if c == 0 then Black else White
  
--   coarbitrary = error "color:coarbitrary: Not implemented"

data_sgf_types_tests :: Test.Framework.Test
data_sgf_types_tests = testGroup "Data.SGF.Types" []


-- tst_color = testGroup "Board.Color" 
--             [ testProperty "otherColor1"  prop_otherColor
--             ]
