{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Data.SGF.Types ( Move
                      , Header
                      , SGF (..)
                      , MetaData (..)
                      , Winner (..)
                      , Result (..)
                      , isWithHandicap
                      , getBlack  
                      , getWhite
                      , getBlackRank
                      , getWhiteRank
                      , getResult
                      , getWinner
                      , getWinnerName
                      , headerFromString
                      , getMovesStr
                      , sgfSummary
                      , sgfTestSummary
                      , moveStrToCoordinates
                      , moveToCoordinates
                      ) where

import Control.Applicative
import Data.Char
import Data.List  (isPrefixOf)
import Data.Maybe (isJust)
import Text.Printf

type Move  = (Int, Int)
type Moves = [Move]

data Header = BlackName | WhiteName | Ruleset | Application | 
              Handicap | BlackRank | WhiteRank | BoardSize | Komi | Time | Date | Result | Unknown
              deriving (Show, Eq)

headerFromString :: String -> Header
headerFromString "PW" = WhiteName
headerFromString "PB" = BlackName
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

data Winner = Black | White
              deriving (Show, Eq, Ord)
                       
data Result = Unfinished | Draw | Win Winner PlayerName
              deriving Show


getResult :: SGF -> Result
getResult sgf@(SGF (MetaData meta) _) = 
  case lookup Result meta of
    Nothing  -> Unfinished
    Just res -> parseResult res 
      where
      parseResult result
        | "B+" `isPrefixOf` result = Win Black (getBlack sgf)
        | "W+" `isPrefixOf` result = Win White (getWhite sgf)
        | otherwise                = Unfinished


getWinnerInfo :: SGF -> Maybe (Winner, PlayerName)
getWinnerInfo sgf = 
  case getResult sgf of
    Unfinished -> Nothing
    Draw       -> Nothing
    Win w p    -> Just (w, p)

getWinner :: SGF -> Maybe Winner
getWinner sgf = fst <$> getWinnerInfo sgf

getWinnerName :: SGF -> Maybe PlayerName
getWinnerName sgf = snd <$> getWinnerInfo sgf

isWithHandicap :: SGF -> Bool
isWithHandicap = isJust . lookup Handicap . fromMeta . metaData

getMeta :: String -> SGF -> String
getMeta str sgf = 
  case (map snd $ filter ((==str) . show . fst) (fromMeta $ metaData sgf)) of
    [] -> ""
    (x:_) -> x
       

getBlack, getWhite, getBlackRank, getWhiteRank, date :: SGF -> String
getBlack     = getMeta "BlackName"
getWhite     = getMeta "WhiteName"
getBlackRank = getMeta "BlackRank"
getWhiteRank = getMeta "WhiteRank"
date         = getMeta "Date"

{-
emptyMetaData :: MetaData
emptyMetaData = MetaData []
-}

data SGF = SGF { metaData :: MetaData
               , moves ::  Moves
               }
           deriving Show

sgfTestSummary :: SGF -> String
sgfTestSummary sgf = printf "%s [%s] vs. %s [%s]. " (getBlack sgf) (getBlackRank sgf) (getWhite sgf) (getWhiteRank sgf)
                     ++ (case getWinnerName sgf of
                            Just "masec" -> "Won"
                            Just _       -> "Lost"
                            Nothing      -> "No result.")

sgfSummary :: SGF -> (String, String, Result, String)
sgfSummary sgf = ( showPlayer (getBlack sgf) (getBlackRank sgf) 
                 , showPlayer (getWhite sgf) (getWhiteRank sgf)
                 , getResult sgf
                 , date sgf ) where
  showPlayer n l 
    | null l    = n
    | otherwise = printf "%s [%s]" n l

-- 11 -> i9
-- 91 -> a9
moveToCoordinates :: Move -> String
moveToCoordinates (x,y) = first : second where
  first  = ['A'..'I'] !! (9 - x)
  second = show (10 - y)
  
moveStrToCoordinates :: String -> String
moveStrToCoordinates "00"     = "PASS"
moveStrToCoordinates ""       = "END"
moveStrToCoordinates [d1, d2] = curry moveToCoordinates (digitToInt d1) (digitToInt d2)
moveStrToCoordinates s        = "ERROR: [" ++ s ++ "]"

movesToText :: Moves -> String
movesToText ms = concatMap (\(x,y) -> show x ++ show y) ms

getMovesStr :: SGF -> String
getMovesStr sgf = movesToText $ moves sgf

