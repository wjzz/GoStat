{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Data.SGF.Types where

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
                            

getMeta :: String -> SGF -> String
getMeta str sgf = head $ map snd $ filter ((==str) . show . fst) (fromMeta $ metaData sgf)

black, white :: SGF -> String
black = getMeta "Black"
white = getMeta "White"

emptyMetaData :: MetaData
emptyMetaData = MetaData []


data SGF = SGF { metaData :: MetaData
               , moves ::  Moves
               }
           deriving Show
