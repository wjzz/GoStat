{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module DB where

import SgfBatching

import Control.Monad
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
  
  forM_ (zip files [1..]) $ \(file, id) -> do
    gameInfo <- (fileToSGF >=> uncurry sgfToGameInfo) `fmap` Strict.readFile file
    case gameInfo of
      Nothing -> return ()
      Just gi -> do
        let (_, win, mvs) = gameInfoToDB gi
        --print (length mvs)
        if id `mod` 1000 == 0
           then putStrLn $ "done " ++ show id
           else return ()
        execute stmt [toSql win, toSql mvs, toSql file]
        return ()
        
  
  commit conn
  disconnect conn
  putStrLn "closed connection with DB"