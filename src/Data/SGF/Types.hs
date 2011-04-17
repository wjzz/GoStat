{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Data.SGF.Types where

import Data.List(isPrefixOf)
import Text.Printf

type Move = (Int, Int)
type Moves = [Move]

data Header = Black | White | Ruleset | Application | 
              Handicap | BlackRank | WhiteRank | BoardSize | Komi | Time | Date | Result | Unknown
              deriving (Show, Eq)

headerFromString :: String -> Header
headerFromString "PW" = White
headerFromString "PB" = Black
headerFromString "RU" = Ruleset
headerFromString "AP" = Application
headerFromString "SZ" = BoardSize
headerFromString "KM" = Komi
headerFromString "TM" = Time
headerFromString "DT" = Date
headerFromString "RE" = Result
headerFromString "HA" = Handicap
headerFromString "BR" = BlackRank
headerFromString "WR" = WhiteRank
headerFromString _    = Unknown


newtype MetaData = MetaData { fromMeta :: [(Header, String)] }
                   deriving (Show)
                            

type PlayerName = String

data Result = Unfinished | Draw | BlackWin PlayerName | WhiteWin PlayerName
              deriving Show
                       
getResult :: SGF -> Result
getResult sgf@(SGF (MetaData meta) _) = 
  case lookup Result meta of
    Nothing  -> Unfinished
    Just res -> parseResult res 
      where
      parseResult result
        | "B+" `isPrefixOf` result = BlackWin (black sgf)
        | "W+" `isPrefixOf` result = WhiteWin (white sgf)
        | otherwise                = Unfinished

getWinnerName :: SGF -> Maybe PlayerName
getWinnerName sgf = 
  case getResult sgf of
    Unfinished -> Nothing
    Draw       -> Nothing
    BlackWin p -> Just p
    WhiteWin p -> Just p
    
getMeta :: String -> SGF -> String
getMeta str sgf = 
  case (map snd $ filter ((==str) . show . fst) (fromMeta $ metaData sgf)) of
    [] -> ""
    (x:_) -> x
       

black, white, blackRank, whiteRank :: SGF -> String
black = getMeta "Black"
white = getMeta "White"
blackRank = getMeta "BlackRank"
whiteRank = getMeta "WhiteRank"


emptyMetaData :: MetaData
emptyMetaData = MetaData []


data SGF = SGF { metaData :: MetaData
               , moves ::  Moves
               }
           deriving Show

sgfSummary :: SGF -> String
sgfSummary sgf = printf "%s [%s] vs. %s [%s]. " (black sgf) (blackRank sgf) (white sgf) (whiteRank sgf)
                 ++ (case getWinnerName sgf of
                        Just "masec" -> "Won"
                        Just _ -> "Lost"
                        Nothing -> "No result.")
