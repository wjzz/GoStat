{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Configuration where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Text.Printf

---------------------------------------
--  Types defining possible options  --
---------------------------------------

data DbServer = PostgreSQL 
              | Sqlite3 FilePath -- path to the .db file
              deriving Show
                       
data Configuration = Configuration { dbServer :: DbServer 
                                   , gameDirs :: [FilePath]
                                   }
                     deriving Show

------------------------------------
--  Parsing a configuration file  --
------------------------------------

-- an inverse to intersperse

split :: String -> Char -> [String]
split []   sep = []
split line sep = 
  case break (== sep) line of
    (_, [])         -> [line]
    (before, after) -> before : split (tail after) sep

parseConfiguration :: String -> Either String Configuration
parseConfiguration contents = foldM extendConfig defaultConfig ls where
  ls = lines contents
  
  extendConfig :: Configuration -> String -> Either String Configuration
  extendConfig config line 
    | null line              = Right config      -- ignore empty lines
    | "--" `isPrefixOf` line = Right config      -- comment
    | null header            = Left "empty header"
    | null rest              = Left "no color"
    | header == "dbserver" = 
      case values of
        ["postgresql"]  -> Right $ config { dbServer = PostgreSQL }
        ["sqlite3",dir] -> Right $ config { dbServer = Sqlite3 dir }
        _               -> Left $ "wrong dbserver: " ++ show values
        
    | header == "gamedirs" =
        case values of
          [] -> Left "gamedirs cannot be empty!"
          
          -- we extend the gameDirs, so each declaration can be on a seperate line if needed          
          _  -> Right $ config { gameDirs = nub (gameDirs config ++ values) } 
          
    | otherwise = Left $ "wrong clause: " ++ line  where
      
    (header, rest) = break (== ':') line
    values         = split (tail rest) ';'

--------------------------------------
--  Unparsing a configuration file  --
--------------------------------------

showDbServer :: DbServer -> String
showDbServer PostgreSQL     = "postgresql;"
showDbServer (Sqlite3 path) = printf "sqlite3;%s;" path

showConfiguration :: Configuration -> String
showConfiguration (Configuration db gameDirs) = 
  unlines [ "dbserver:" ++ showDbServer db
          , "gamedirs:" ++ (concatMap (flip (++) ";") gameDirs)
          ]

-----------------------------------------------
--  Loading and saving a configuration file  --
-----------------------------------------------

configurationPath :: FilePath
configurationPath = "/home/wjzz/Dropbox/Programy/Haskell/GoStat/CONFIG"

readConfig :: FilePath -> IO (Either String Configuration)
readConfig path = parseConfiguration `fmap` readFile path

writeConfig :: Configuration -> FilePath -> IO ()
writeConfig config path = writeFile path $ showConfiguration config

---------------------------
--  Default config file  --
---------------------------

sgfDirectory :: FilePath
sgfDirectory = "/home/wjzz/Dropbox/Programy/Haskell/GoStat/data/2011/1"

sqliteDir :: FilePath
sqliteDir = "/home/wjzz/Dropbox/Programy/Haskell/GoStat/db/games.db"

defaultConfig :: Configuration
defaultConfig = Configuration { dbServer = Sqlite3 sqliteDir -- PostgreSQL
                              , gameDirs = [sgfDirectory]
                              }

------------------------
--  The GoStat monad  --
------------------------

type GoStatM a = ReaderT Configuration IO a

runGoStatM :: Configuration -> GoStatM a -> IO a
runGoStatM = flip runReaderT

getConfig :: GoStatM Configuration
getConfig = ask