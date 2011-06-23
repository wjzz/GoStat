{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Server where

import Data.SGF.Parsing
import DB
import Pages
import Lang

import Control.Monad
import Control.Monad.Trans
import Happstack.Server hiding (body, path)
import Text.Printf

---------------------------------------------------
--  The top-level server and routing procedures  --
---------------------------------------------------

server :: IO ()
server = do
  putStrLn "Listening on port 8000..."
  simpleHTTP nullConf $ router
  
router :: ServerPart Response
router = msum [ dir "movebrowser" moveBrowserC
              , dir "games"       gamesC
              , dir "game"        gameDetailsC
              , dir "public" $ serveDirectory EnableBrowsing [] "public"
              , dir "sgf"    $ serveDirectory EnableBrowsing [] "data"                                
              , mainPageC
              ]
         
---------------------
--  The main page  --
---------------------

mainPageC :: ServerPart Response
mainPageC = do
  ok $ toResponse $ mainPage onLineConfig
  
------------------------
--  The game browser  --
------------------------

gamesC :: ServerPart Response
gamesC = do
  (count, currentStats, movesSoFar, lang) <- fetchStats
  
  limit <- (read `fmap` look "limit") `mplus` return 200
  games <- liftIO $ queryGamesListDB movesSoFar limit
  
  let relativeGames = map makeRelative games
  
  ok $ toResponse $ gamesPage relativeGames count currentStats movesSoFar (onLineConfig { language = lang })

-----------------------------
--  The game details page  --
-----------------------------

gameDetailsC :: ServerPart Response
gameDetailsC = do
  (count, _currentStats, _movesSoFar, lang) <- fetchStats
  gamePathM <- ((\x -> [x]) `fmap` look "path") `mplus` return []
  contents <- liftIO $ mapM (readFile . makeAbsolute) gamePathM
  
  let sgf = 
        case contents of
          [file] -> either (const Nothing) Just (parseSGF file)
          _      -> Nothing
          
  let path = case gamePathM of
        [filePath] -> Just filePath
        _          -> Nothing        

  ok $ toResponse $ gameDetailsPage count sgf path (onLineConfig { language = lang })

----------------------------
--  The moveBrowser page  --
----------------------------

moveBrowserC :: ServerPart Response
moveBrowserC = do
  (count, currentStats, movesSoFar, lang) <- fetchStats 
  moves                                   <- liftIO $ queryStatsDB movesSoFar  
  
  ok $ toResponse $ moveBrowser count currentStats moves movesSoFar (onLineConfig { language = lang })
  
---------------------------
--  A fetching shortcut  --
---------------------------

fetchStats :: ServerPart (Int, (Int, Int, Int), String, Messages)
fetchStats = do
  count      <- liftIO $ queryCountDB
  movesSoFar <- look "moves" `mplus` (return [])
  langStr    <- look "lang"  `mplus` (return "pl")
  currStats  <- liftIO $ queryCurrStatsDB movesSoFar
  
  let lang = 
        case langStr of
          "pl" -> pl
          _    -> eng  
          
  return (count, currStats, movesSoFar, lang)

------------------------------
--  Make the path relative  --
------------------------------

--TODO 
-- use IO and find this directory dynamically, on the run

absolutePathToGameDir :: FilePath
absolutePathToGameDir = "/home/wjzz/Dropbox/Programy/Haskell/GoStat/data/"

makeRelative :: FilePath -> FilePath
makeRelative = drop (length absolutePathToGameDir)

makeAbsolute :: FilePath -> FilePath
makeAbsolute s = absolutePathToGameDir ++ s

-----------------------------------------------
--  The configuration of the online version  --
-----------------------------------------------

onLineConfig :: Configuration
onLineConfig = Configuration { mainPageUrl        = "/"
                             , moveBrowserMainUrl = "/movebrowser"
                             , moveBrowserMakeUrl = urlMaker
                             , gameBrowserMakeUrl = gameBrowserUrlMaker
                             , gameDetailsMakeUrl = gameDetailsUrlMaker
                             , gameDownloadLink   = sgfDownloadUrlMaker
                             , imagesMakeUrl      = imageUrlMaker
                             , cssUrl             = "/public/style.css"
                             , jsUrls             = ["/public/jquery.js", "/public/highlight.js", "/public/eidogo/player/js/all.compressed.js"]
                             , language           = eng
                             }

urlMaker :: Language -> String -> String
urlMaker langN movesList = printf "/movebrowser?lang=%s&moves=%s" langN movesList

imageUrlMaker :: String -> String
imageUrlMaker s = "/public/img/" ++ s 

gameBrowserUrlMaker :: Language -> MovesSoFar -> String
gameBrowserUrlMaker lang moves = printf "/games?lang=%s&moves=%s" lang moves

gameDetailsUrlMaker :: Language -> FilePath -> String
gameDetailsUrlMaker lang path = printf "/game?lang=%s&path=%s" lang path

sgfDownloadUrlMaker :: FilePath -> String
sgfDownloadUrlMaker path = printf "/sgf/%s" path