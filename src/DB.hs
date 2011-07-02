{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module DB ( createDB 
          , deleteDB
          , addFilesToDB
          , queryCountDB
          , queryStatsDB
          , queryCurrStatsDB
          , queryGamesListDB
          , queryFindGameById
          , rebuildDB
          ) where

import SgfBatching hiding (moves)

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Database.HDBC.Sqlite3    (connectSqlite3)
import System.IO.Strict as Strict
import Text.Printf

import Configuration

withConnection :: (ConnWrapper -> IO a) -> GoStatM a
withConnection dbAction = do
  db   <- dbServer <$> getConfig
  conn <- (case db of
              PostgreSQL     -> liftIO $ ConnWrapper <$> connectPostgreSQL ""
              Sqlite3 dbPath -> liftIO $ ConnWrapper <$> connectSqlite3 dbPath)
          
  result <- liftIO $ dbAction conn
  liftIO $ disconnect conn
  return result

createTableQuery :: String -> String 
createTableQuery gameId = 
  printf "CREATE TABLE go_stat_data (id %s PRIMARY KEY, %s, %s, %s, %s, %s, %s, %s)" gameId winner moves path b_name w_name b_rank w_rank where
    winner = "winner CHAR NOT NULL"
    moves  = "moves VARCHAR(700) NOT NULL"
    path   = "path VARCHAR(255) NOT NULL"
    b_name = "b_name VARCHAR(30) NOT NULL"
    w_name = "w_name VARCHAR(30) NOT NULL"
    b_rank = "b_rank VARCHAR(10)"
    w_rank = "w_rank VARCHAR(10)"

createDB :: GoStatM ()
createDB = do
  db <- dbServer <$> getConfig
  
  let createTable = case db of
        PostgreSQL-> createTableQuery "SERIAL"
        Sqlite3 _ -> createTableQuery "INTEGER"

  withConnection $ \(ConnWrapper conn) -> do
    run conn createTable []
    commit conn

deleteDB :: GoStatM ()
deleteDB = withConnection $ \(ConnWrapper conn) -> do
    catch (handleSqlError (do
              run conn "DROP TABLE go_stat_data" []
              commit conn))
      (const (return ()))

addFilesToDB :: MVar (Maybe Int) -> GoStatM ()
addFilesToDB mint = do
  dirs <- gameDirs <$> getConfig
  withConnection (\(ConnWrapper conn) -> do 
  putStrLn "connected to DB..."
  
  len <- length <$> getSGFs dirs
  putStrLn $ printf "%d games to analyze." len
  
  files <- getSGFs dirs
  
  stmt <- prepare conn "INSERT INTO go_stat_data (winner, moves, path, b_name, w_name, b_rank, w_rank) VALUES (?,?,?,?,?,?,?)"
  
  forM_ (zip files [(1::Int)..]) $ \(file, index) -> do
    gameInfo <- (fileToSGF >=> uncurry sgfToGameInfo) `fmap` Strict.readFile file
    
    case gameInfo of
      Nothing -> return ()
      Just gi -> do
        let (_, win, mvs, bName, wName, bRank, wRank) = gameInfoToDB gi
        execute stmt [toSql win, toSql mvs, toSql file, toSql bName, toSql wName, toSql bRank, toSql wRank]
        return ()
        
    if index `mod` 100 == 0
      then do let perc = ((100 * index) `div` len)
              --putStrLn $ printf "done %d (%2d%%)" index perc
              swapMVar mint $ Just perc
              commit conn
      else return ()

  commit conn)
  liftIO $ swapMVar mint Nothing
  liftIO $ putStrLn "closed connection to DB"
  
queryCountDB :: GoStatM Int
queryCountDB = do
  answer <- withConnection $ \(ConnWrapper conn) -> do
    quickQuery' conn "SELECT count(*) FROM go_stat_data" []
  
  case answer of
    [[sqlInt]] -> return $ fromSql sqlInt
    _          -> return 0


-- |Returns a statistic in the form (move, total_played, black won, white won) about all possible continuations
-- |of the current position found the db
queryStatsDB :: String -> GoStatM [(String, Int, Int, Int)]
queryStatsDB movesSoFar = do
  (total, black) <- withConnection $ \(ConnWrapper conn) -> do
    total <- quickQuery' conn total_query []
    black <- quickQuery' conn black_query [toSql 'b']
    return (total, black)
  
  return $ map (count black) total where
    start_index = 1 + length movesSoFar
    pattern     = '\'': movesSoFar ++ "%'"
    
    query_template = "SELECT SUBSTR(moves, %d, 2) as st, count(*) FROM go_stat_data %s GROUP BY st" 
    total_query = printf query_template start_index (if null movesSoFar then "" else "WHERE moves LIKE " ++ pattern)
    black_query = printf query_template start_index ("WHERE WINNER = ?" ++ 
                                                     (if null movesSoFar then "" else "AND moves LIKE" ++ pattern))
        
    count black [moves, totalCount] = (fromSql moves, tc, bWin, wWin) where
      black_count :: Maybe Int
      black_count = fmap fromSql $ lookup moves $ map (\[a,b] -> (a,b)) black
      
      tc = fromSql totalCount
      bWin = fromMaybe 0 black_count
      wWin = tc - bWin
    count _ _ = ("",0,0,0)

-- |Returns a statistic in the form (total_played, black won, white won) about the current position
queryCurrStatsDB :: String -> GoStatM (Int, Int, Int)
queryCurrStatsDB movesSoFar = do
  (total, black) <- withConnection $ \(ConnWrapper conn) -> 
    do  
      total <- quickQuery' conn total_query []
      black <- quickQuery' conn black_query [toSql 'b']
      return (total, black)
  
  return $ count total black where 
    count total' black' = (t, b, w) where
      t = get total'
      b = get black'
      w = t - b
      get = fromSql . head . head    
    pattern     = '\'': movesSoFar ++ "%'"
    
    query_template = "SELECT COUNT(*) FROM go_stat_data %s" 
    total_query = printf query_template ("WHERE moves LIKE "                ++ pattern)
    black_query = printf query_template ("WHERE WINNER = ? AND moves LIKE " ++ pattern)

-- |Returns a list of full data of games with the given position, but not more that the given limit
queryGamesListDB :: String -> Int -> GoStatM [(Int, FilePath, String, String, String, String, String)]
queryGamesListDB movesSoFar limit = do
  result <- withConnection $ \(ConnWrapper conn) -> 
     quickQuery' conn query [toSql limit]

  return $ map convertFromSql result where
    convertFromSql (gameId : path : winner : b_name : w_name : b_rank : w_rank : _) = 
      (fromSql gameId, fromSql path, fromSql winner, fromSql b_name, fromSql w_name, fromSql b_rank, fromSql w_rank)
    
    pattern = '\'': movesSoFar ++ "%'"
    query   = printf "SELECT id, path, winner, b_name, w_name, b_rank, w_rank FROM go_stat_data %s LIMIT ?" 
                                 (if null movesSoFar then "" else "WHERE moves LIKE " ++ pattern)
    
queryFindGameById :: Int -> GoStatM (Maybe (String, FilePath))
queryFindGameById gameId = do
  result <- withConnection $ \(ConnWrapper conn) -> 
    quickQuery' conn "SELECT moves, path FROM go_stat_data WHERE id = ?" [toSql gameId]
  
  return $ case result of
    ((path:moves:_):_) -> Just (fromSql path, fromSql moves)
    _                  -> Nothing

rebuildDB :: MVar (Maybe Int) -> GoStatM ()
rebuildDB mint = do
  liftIO $ putStrLn "Starting DB rebuilding..."
  deleteDB
  liftIO $ putStrLn "Deleted DB."
  createDB
  liftIO $ putStrLn "Created DB."
  addFilesToDB mint
  liftIO $ putStrLn "DB rebuilding done!"
