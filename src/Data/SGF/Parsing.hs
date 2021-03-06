{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Data.SGF.Parsing ( parseSGF
                        , maybeMove 
                        , module Data.SGF.Types
                        ) where

import Data.SGF.Types

import Data.Char (ord)
import Data.Maybe
import Text.ParserCombinators.Parsec hiding (runParser)

---------------------------
--  Top level functions  --
---------------------------

parseSGF :: String -> Either String SGF
parseSGF input = 
  let 
    input' = filter (/= '\n') input
  in
   case parse sgfParser "" input' of
     Left e  -> Left $ show e
     Right v -> Right v

maybeMove :: (Maybe Char, Maybe Char) -> Maybe Move
maybeMove (m1, m2) = do
  f <- m1
  s <- m2
  return $ (9 - (ord f - ord 'a'), ord s - ord 'a' + 1)

------------------------
--  Helper functions  --
------------------------

sgfParser :: Parser SGF
sgfParser = do
  char '('
  char ';'  
  meta <- metaDataParser
  char ';'
  mvs <- moveParser `sepBy` (char ';')
  --char ')'
  return $ SGF { metaData = meta, moves = map maybeMoveToMove (dropFinalPasses mvs) }
  
symb :: Parser Char
symb = alphaNum <|> space  <|> oneOf "[].-:/\\()~,+-_"
  
-- Parses key[value] pairs in the metadata
singleMeta :: Parser (Header, String)
singleMeta = do
  key <- many1 letter
  char '['
  value <- many ((char '\\' >> char ']') <|> alphaNum <|> space <|> oneOf ".-:/()~,+-_[")
  char ']'
  return $ (headerFromString key, value)

metaDataParser :: Parser MetaData
metaDataParser = do
  meta <- many singleMeta
  return $ MetaData meta

dropFinalPasses :: [Maybe Move] -> [Maybe Move]
dropFinalPasses ls = reverse $ dropWhile isNothing $ reverse ls

maybeMoveToMove :: Maybe Move -> Move
maybeMoveToMove Nothing = (0,0)
maybeMoveToMove (Just x) = x

moveParser :: Parser (Maybe Move)
moveParser = do
  char 'B' <|> char 'W'
  char '['
  m1 <- optionMaybe letter
  m2 <- optionMaybe letter
  char ']'
  many symb
  return $ maybeMove (m1, m2)