{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module SgfBatching where

import Data.SGF.Types
import Data.SGF.Parsing

import Control.Applicative
import Control.Monad
import Data.Maybe
import System.FilePath.Find hiding (fileName)

import System.IO.Strict as Strict

-- | Returns a lazy list of all files in a given directory (and in it's subdirectories)
getFileNames :: FilePath -> IO [FilePath]
getFileNames topDir = find always (extension ==? ".sgf") topDir

sgfDirectory :: FilePath
sgfDirectory = "/home/wjzz/Dropbox/Programy/Haskell/GoStat/data/"

getSGFs :: IO [FilePath]
getSGFs = getFileNames sgfDirectory


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
                         }
                deriving Show
                
sgfToGameInfo :: FilePath -> SGF -> Maybe GameInfo
sgfToGameInfo fileName sgf = do
  win <- getWinner sgf
  --  guard -- has at least 20 moves
  return $ GameInfo fileName win (Data.SGF.Types.moves sgf)


loadSGFs :: IO [GameInfo]
loadSGFs = do
  files <- getSGFs
  inputs <- mapM Strict.readFile files
  let gameInfos = map (fileToSGF >=> uncurry sgfToGameInfo) inputs
  
  return $ catMaybes gameInfos