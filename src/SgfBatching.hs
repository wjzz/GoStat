
{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module SgfBatching where

import Data.SGF.Types
import Data.SGF.Parsing
import Transformations

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
                
-- |Converts a GameInfo data so that it can be written in a db
gameInfoToDB :: GameInfo -> (FilePath, Char, String)
gameInfoToDB (GameInfo sgf win mvs) = (sgf, winToChar win, movesToString mvs) where
  winToChar Black = 'b'
  winToChar White = 'w'
  
  movesToString = concatMap (\(a,b) -> show a ++ show b) 

sgfToGameInfo :: FilePath -> SGF -> Maybe GameInfo
sgfToGameInfo fileName sgf = do
  win <- getWinner sgf
  
  let mvs = Data.SGF.Types.moves sgf
  guard (length mvs > 5)
  guard (not (isWithHandicap sgf))
  return $ GameInfo fileName win (normalizeMoves mvs)
{-
aux :: FilePath -> IO Int
aux file = do
  l <- length `fmap` Prelude.readFile file
  l `seq`  return l

aux2 :: FilePath -> IO (Maybe GameInfo)
aux2 file = do
  l <- (fileToSGF >=> uncurry sgfToGameInfo) `fmap` Strict.readFile file
  return l

loadSGFs :: IO [GameInfo]
loadSGFs = do
  files <- getSGFs
  gameInfos <- mapM aux files
  --gameInfos <- {-map (fileToSGF >=> uncurry sgfToGameInfo) `fmap`-} mapM aux {-Strict.readFile-} files
  --value <- sum `fmap` mapM aux files
  --print value
  --let gameInfos = map (fileToSGF >=> uncurry sgfToGameInfo) inputs
  
  return $ map undefined gameInfos
  --return $ catMaybes gameInfos
  --return $ [] -- map undefined files
-}