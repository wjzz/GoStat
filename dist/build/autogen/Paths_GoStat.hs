module Paths_GoStat (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/wjzz/.cabal/bin"
libdir     = "/home/wjzz/.cabal/lib/GoStat-0.1/ghc-7.0.2"
datadir    = "/home/wjzz/.cabal/share/GoStat-0.1"
libexecdir = "/home/wjzz/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "GoStat_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "GoStat_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "GoStat_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "GoStat_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
