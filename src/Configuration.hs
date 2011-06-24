{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Configuration where

import Control.Monad
import Control.Monad.Reader

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

-----------------------------------------------
--  Loading and saving a configuration file  --
-----------------------------------------------

readConfig :: IO Configuration
readConfig = undefined

writeConfig :: Configuration -> IO ()
writeConfig = undefined

---------------------------
--  Default config file  --
---------------------------

sgfDirectory :: FilePath
sgfDirectory = "/home/wjzz/Dropbox/Programy/Haskell/GoStat/data/2011/1"

sqliteDir :: FilePath
sqliteDir = "/home/wjzz/Dropbox/Programy/Haskell/GoStat/db/games.db"

defaultConfig :: Configuration
defaultConfig = Configuration { dbServer = PostgreSQL
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