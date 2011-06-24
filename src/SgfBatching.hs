{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module SgfBatching where

import Data.SGF.Types
import Data.SGF.Parsing
import Transformations

import Control.Monad
import System.FilePath.Find hiding (fileName)

--import System.IO.Strict as Strict

-- | Returns a lazy list of all files in a given directory (and in it's subdirectories)
getFileNames :: FilePath -> IO [FilePath]
getFileNames topDir = find always (extension ==? ".sgf") topDir

getSGFs :: [FilePath] -> IO [FilePath]
getSGFs dirs = concat `fmap` mapM getFileNames dirs

fileToSGF :: String -> Maybe (FilePath, SGF)
fileToSGF input = do
  case parseSGF input of
    Left _ -> Nothing
    Right v -> Just (input, v)


-- | A data type containing only the most important information about a go game.
-- | We only care about games that have a winner, so no draws or unfinished games.
data GameInfo = GameInfo { sgfFileName  :: FilePath
                         , winner       :: Winner
                         , moves        :: [Move]
                         , blackName    :: String
                         , whiteName    :: String
                         , blackRank    :: String
                         , whiteRank    :: String
                         }
                deriving Show
                
-- |Converts a GameInfo data so that it can be written in a db
gameInfoToDB :: GameInfo -> (FilePath, Char, String, String, String, String, String) -- b,w,brank,wrank
gameInfoToDB (GameInfo sgf win mvs bName wName bRank wRank) = (sgf, winToChar win, movesToString mvs, bName, wName, bRank, wRank) where
  winToChar Black = 'b'
  winToChar White = 'w'
  
  movesToString = concatMap (\(a,b) -> show a ++ show b) 

sgfToGameInfo :: FilePath -> SGF -> Maybe GameInfo
sgfToGameInfo fileName sgf = do
  win <- getWinner sgf
  
  let mvs = Data.SGF.Types.moves sgf
  guard (length mvs > 5)
  guard (not (isWithHandicap sgf))
  return $ GameInfo fileName win (normalizeMoves mvs) (getBlack sgf) (getWhite sgf) (getBlackRank sgf) (getWhiteRank sgf)
