{-# OPTIONS -Wall -fno-warn-unused-do-bind #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module DB where

import SgfBatching hiding (moves)

import Control.Applicative
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

createDB :: GoStatM ()
createDB = do
  db <- dbServer <$> getConfig
  
  let createTable = case db of
        PostgreSQL-> "CREATE TABLE go_stat_data (id SERIAL  PRIMARY KEY, winner CHAR NOT NULL, moves VARCHAR(700) NOT NULL, game_id VARCHAR(255))"
        Sqlite3 _ -> "CREATE TABLE go_stat_data (id INTEGER PRIMARY KEY, winner CHAR NOT NULL, moves VARCHAR(700) NOT NULL, game_id VARCHAR(255))"

  withConnection $ \(ConnWrapper conn) -> do
    run conn createTable []
    commit conn

deleteDB :: GoStatM ()
deleteDB = withConnection $ \(ConnWrapper conn) -> do
    catch (do
              run conn "DROP TABLE go_stat_data" []
              commit conn)
      (const (return ()))

addFilesToDB :: GoStatM ()
addFilesToDB = do
  dirs <- gameDirs <$> getConfig
  withConnection (\(ConnWrapper conn) -> do 
  putStrLn "connected to DB..."
  
  len <- length <$> getSGFs dirs
  putStrLn $ printf "%d games to analyze." len
  
  files <- getSGFs dirs
  
  stmt <- prepare conn "INSERT INTO go_stat_data (winner, moves, game_id) VALUES (?,?,?)"
  
  forM_ (zip files [(1::Int)..]) $ \(file, index) -> do
    gameInfo <- (fileToSGF >=> uncurry sgfToGameInfo) `fmap` Strict.readFile file
    case gameInfo of
      Nothing -> return ()
      Just gi -> do
        let (_, win, mvs) = gameInfoToDB gi
        --print (length mvs)
        execute stmt [toSql win, toSql mvs, toSql file]
        return ()
    if index `mod` 1000 == 0
      then putStrLn $ printf "done %d (%2d%%)" index ((100 * index) `div` len)
      else return ()

  
  commit conn)
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
queryGamesListDB :: String -> Int -> GoStatM [(Int, String)]
queryGamesListDB movesSoFar limit = do
  result <- withConnection $ \(ConnWrapper conn) -> 
     quickQuery' conn query [toSql limit]

  return $ map (\(idd:game_id:_) -> (fromSql idd, fromSql game_id)) result where
    pattern = '\'': movesSoFar ++ "%'"
    query = printf "SELECT id, game_id FROM go_stat_data %s LIMIT ?" (if null movesSoFar then "" else "WHERE moves LIKE " ++ pattern)
    
queryFindGameById :: Int -> GoStatM (Maybe (String, FilePath))
queryFindGameById gameId = do
  result <- withConnection $ \(ConnWrapper conn) -> 
    quickQuery' conn "SELECT moves, game_id FROM go_stat_data WHERE id = ?" [toSql gameId]
  
  return $ case result of
    ((path:moves:_):_) -> Just (fromSql path, fromSql moves)
    _                 -> Nothing


{-
createDB :: IO ()
createDB = do
  conn <- connectPostgreSQL ""
  run conn "CREATE TABLE go_stat_data (id SERIAL PRIMARY KEY, winner CHAR NOT NULL, moves VARCHAR(700) NOT NULL, game_id VARCHAR(255))" []
  commit conn
  disconnect conn

deleteDB :: IO ()
deleteDB = do
  conn <- connectPostgreSQL ""
  run conn "DROP TABLE go_stat_data" []
  commit conn
  disconnect conn

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


-- |Returns a statistic in the form (move, total_played, black won, white won) about all possible continuations
-- |of the current position found the db
queryStatsDB :: String -> IO [(String, Int, Int, Int)]
queryStatsDB movesSoFar = do
  conn <- connectPostgreSQL ""
    
  total <- quickQuery' conn total_query []
  black <- quickQuery' conn black_query [toSql 'b']
  
  disconnect conn
  
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
queryCurrStatsDB :: String -> IO (Int, Int, Int)
queryCurrStatsDB movesSoFar = do
  conn <- connectPostgreSQL ""
    
  total <- quickQuery' conn total_query []
  black <- quickQuery' conn black_query [toSql 'b']  
  
  disconnect conn
  
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
queryGamesListDB :: String -> Int -> IO [(Int, String)]
queryGamesListDB movesSoFar limit = do
  conn <- connectPostgreSQL ""

  result <- (quickQuery' conn query [toSql limit]) 
  
  disconnect conn

  return $ map (\(idd:game_id:_) -> (fromSql idd, fromSql game_id)) result where
    pattern = '\'': movesSoFar ++ "%'"
    query = printf "SELECT id, game_id FROM go_stat_data %s LIMIT ?" (if null movesSoFar then "" else "WHERE moves LIKE " ++ pattern)
    
queryFindGameById :: Int -> IO (Maybe (String, FilePath))
queryFindGameById gameId = do
  conn <- connectPostgreSQL ""

  result <- (quickQuery' conn "SELECT moves, game_id FROM go_stat_data WHERE id = ?" [toSql gameId]) 
  
  disconnect conn
  
  return $ case result of
    ((path:moves:_):_) -> Just (fromSql path, fromSql moves)
    _                 -> Nothing
-}