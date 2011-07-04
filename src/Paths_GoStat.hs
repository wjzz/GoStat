module Paths_GoStat where

getDataFileName :: FilePath -> IO FilePath
getDataFileName "CONFIG" = return "LOCAL_CONFIG"
getDataFileName f = return f