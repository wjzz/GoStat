{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Server where

import Data.SGF.Parsing
import DB
import Pages
import Lang
import Configuration

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Happstack.Server hiding (body, path)
import Text.Printf

---------------------------------------------------
--  The top-level server and routing procedures  --
---------------------------------------------------

server :: GoStatM ()
server = do
  liftIO $ putStrLn "Listening on port 8000..."
  config <- getConfig
  liftIO $ simpleHTTP nullConf $ router config
  
router :: Configuration -> ServerPart Response
router config = msum [ dir "movebrowser" $ moveBrowserC config
                     , dir "games"       $ gameBrowserC config
                     , dir "game"        $ gameDetailsC config
                     , dir "public"      $ serveDirectory EnableBrowsing [] "public"
                     , dir "sgf"         $ serveDirectory EnableBrowsing [] "data"                                
                     , mainPageC config
                     ]
         
---------------------
--  The main page  --
---------------------

mainPageC :: Configuration -> ServerPart Response
mainPageC config = do
  ok $ toResponse $ mainPage onLineConfig
  
------------------------
--  The game browser  --
------------------------

gameBrowserC :: Configuration -> ServerPart Response
gameBrowserC config = do undefined
                         {-
  (count, currentStats, movesSoFar, lang) <- fetchStats
  
  limit <- (read `fmap` look "limit") `mplus` return 200
  games <- liftIO $ queryGamesListDB movesSoFar limit
  
  let idsWithRelativeGames = map (second makeRelative) games
  
  ok $ toResponse $ gameBrowserPage idsWithRelativeGames count currentStats movesSoFar (onLineConfig { language = lang })
-}
-----------------------------
--  The game details page  --
-----------------------------

gameDetailsC :: Configuration -> ServerPart Response
gameDetailsC config = do undefined
  {-
  (count, _currentStats, _movesSoFar, lang) <- fetchStats

  gameIdM <- ((Just . read) `fmap` look "id") `mplus` return Nothing
  
  case gameIdM of
    Nothing  -> errorPage "No id given"
    Just gameId -> do
      qResult <- liftIO $ queryFindGameById gameId
      
      case qResult of
        Nothing -> errorPage "Wrong id, game not found"
        Just (movesSoFar, absPath) -> do
          contents <- liftIO $ readFile absPath
          
          case parseSGF contents of
            Left _ -> errorPage "Problem reading or parsing the given sgf"
            Right sgf -> 
              ok $ toResponse $ gameDetailsPage count gameId sgf (makeRelative absPath) 
                                                movesSoFar (onLineConfig { language = lang })
-}
-- TODO
-- create a real error page
  
errorPage :: String -> a
errorPage s = error s

----------------------------
--  The moveBrowser page  --
----------------------------

moveBrowserC :: Configuration -> ServerPart Response
moveBrowserC config = do undefined
  {-
  (count, currentStats, movesSoFar, lang) <- fetchStats 
  moves                                   <- liftIO $ queryStatsDB movesSoFar  
  
  ok $ toResponse $ moveBrowser count currentStats moves movesSoFar (onLineConfig { language = lang })
-}
  
  
---------------------------
--  A fetching shortcut  --
---------------------------

fetchStats :: ServerPart (Int, (Int, Int, Int), String, Messages)
fetchStats = do undefined
                {-
  count      <- liftIO $ queryCountDB
  movesSoFar <- look "moves" `mplus` (return [])
  langStr    <- look "lang"  `mplus` (return "pl")
  currStats  <- liftIO $ queryCurrStatsDB movesSoFar
  
  let lang = 
        case langStr of
          "pl" -> pl
          _    -> eng  
          
  return (count, currStats, movesSoFar, lang)
-}
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

onLineConfig :: UrlBuilders
onLineConfig = UrlBuilders { mainPageUrl        = "/"
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

gameDetailsUrlMaker :: Language -> Int -> String
gameDetailsUrlMaker lang idd = printf "/game?lang=%s&id=%d" lang idd

sgfDownloadUrlMaker :: FilePath -> String
sgfDownloadUrlMaker path = printf "/sgf/%s" path