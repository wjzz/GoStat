{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Control.Monad.Instances
import Data.SGF.Types
import Data.SGF.Parsing

displaySGF :: String -> String
displaySGF input = 
  case parseSGF input of
    Left err -> err ++ "\n"
    Right sgf -> sgfSummary sgf ++ "\n"

main :: IO ()
main = interact displaySGF
