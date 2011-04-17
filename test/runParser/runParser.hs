{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Control.Monad.Instances
import Data.SGF.Types
import Data.SGF.Parsing

main :: IO ()
main = interact (show . fmap (fromMeta . metaData) . parseSGF)
