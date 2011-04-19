{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module DB where

import SgfBatching

import Control.Monad
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL(connectPostgreSQL)
import System.IO.Strict as Strict

createDB :: IO ()
createDB = do
  conn <- connectPostgreSQL ""
  run conn "CREATE TABLE go_stat_data (id SERIAL PRIMARY KEY, winner CHAR NOT NULL, moves VARCHAR(666) NOT NULL, game_id VARCHAR(255))" []
  commit conn
  disconnect conn

deleteDB :: IO ()
deleteDB = do
  conn <- connectPostgreSQL ""
  run conn "DROP TABLE go_stat_data" []
  commit conn
  disconnect conn

-- addDataToDB :: IO ()
-- addDataToDB = do
--   conn <- connectPostgreSQL ""
--   -- stmt <- prepare conn "INSERT INTO go_stat_data VALUES (?)"
--   -- executeMany stmt $ map (\x -> [toSql x]) [1..(10::Integer)]
--   -- run conn "INSERT INTO go_stat_data (winner, moves) VALUES (?, ?)" [toSql 'b', toSql "1234342"]
--   commit conn
--   disconnect conn

addFilesToDB :: IO ()
addFilesToDB = do
  conn <- connectPostgreSQL ""
  putStrLn "connected to DB..."
  
  files <- getSGFs
  
  stmt <- prepare conn "INSERT INTO go_stat_data (winner, moves, game_id) VALUES (?,?,?)"
  
  forM_ (zip files [(1::Int)..]) $ \(file, index) -> do
    gameInfo <- (fileToSGF >=> uncurry sgfToGameInfo) `fmap` Strict.readFile file
    case gameInfo of
      Nothing -> return ()
      Just gi -> do
        let (_, win, mvs) = gameInfoToDB gi
        --print (length mvs)
        if index `mod` 1000 == 0
           then putStrLn $ "done " ++ show index
           else return ()
        execute stmt [toSql win, toSql mvs, toSql file]
        return ()
        
  
  commit conn
  disconnect conn
  putStrLn "closed connection with DB"
  
queryCountDB :: IO Int
queryCountDB = do
  conn <- connectPostgreSQL ""
  answer <- quickQuery' conn "SELECT count(*) FROM go_stat_data" []
  disconnect conn
  
  case answer of
    [[sqlInt]] -> return $ fromSql sqlInt
    _          -> return 0


-- |Returns a statistic in the form (move, total_played, black won, white won)
queryStatsDB :: IO [(String, Int, Int, Int)]
queryStatsDB = do
  conn <- connectPostgreSQL ""
  total <- quickQuery' conn "SELECT SUBSTR(moves, 1,2) as st, count(*) FROM go_stat_data GROUP BY st" []
  black <- quickQuery' conn "SELECT SUBSTR(moves, 1,2) as st, count(*) FROM go_stat_data WHERE winner = ? GROUP BY st " [toSql 'b']
  
  disconnect conn
  
  return $ map (count black) total where
    count black [moves, totalCount] = (fromSql moves, tc, b, w) where
      black_count :: Maybe Int
      black_count = fmap fromSql $ lookup moves $ map (\[a,b] -> (a,b)) black
      
      tc = fromSql totalCount
      b = fromMaybe 0 black_count
      w = tc - b
    count _ _ = ("",0,0,0)
