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
import Control.Concurrent
import Control.Concurrent.MVar
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
                     , dir "sgf"         $ uriRest (sgfBrowserC config)
                     , dir "rebuild"     $ rebuildC config
                     , mainPageC config
                     ]
         
---------------------
--  The main page  --
---------------------

mainPageC :: Configuration -> ServerPart Response
mainPageC config = do
  ok $ toResponse $ mainPage onLineBuilders
  
----------------------------
--  A readability helper  --
----------------------------

withConfig :: Configuration -> GoStatM a -> ServerPart a
withConfig config action = liftIO $ runGoStatM config action

------------------------
--  The game browser  --
------------------------

gameBrowserC :: Configuration -> ServerPart Response
gameBrowserC config = do 
  (count, currentStats, movesSoFar, lang) <- fetchStats config  
  limit <- (read `fmap` look "limit") `mplus` return 200
  games <- withConfig config $ queryGamesListDB movesSoFar limit
  
  ok $ toResponse $ gameBrowserPage games count currentStats movesSoFar (onLineBuilders { language = lang })

-----------------------------
--  The game details page  --
-----------------------------

gameDetailsC :: Configuration -> ServerPart Response
gameDetailsC config = do 
  (count, _currentStats, _movesSoFar, lang) <- fetchStats config

  gameIdM <- ((Just . read) `fmap` look "id") `mplus` return Nothing
  
  case gameIdM of
    Nothing      -> errorPage "No id given"
    Just gameId -> do
      qResult <- withConfig config $ queryFindGameById gameId
      
      case qResult of
        Nothing -> errorPage "Wrong id, game not found"
        Just (movesSoFar, relPath) -> do
          contents <- liftIO $ readFile relPath
          
          case parseSGF contents of
            Left _ -> errorPage "Problem reading or parsing the given sgf"
            Right sgf -> 
              ok $ toResponse $ gameDetailsPage count gameId sgf relPath
                                                movesSoFar (onLineBuilders { language = lang })

-- TODO
-- create a real error page
  
errorPage :: String -> a
errorPage s = error s

----------------------------
--  The moveBrowser page  --
----------------------------

moveBrowserC :: Configuration -> ServerPart Response
moveBrowserC config = do 
  (count, currentStats, movesSoFar, lang) <- fetchStats config
  moves <- withConfig config $ queryStatsDB movesSoFar  
  
  ok $ toResponse $ moveBrowser count currentStats moves movesSoFar (onLineBuilders { language = lang })
  
------------------------------
--  The rebuild controller  --
------------------------------
  
rebuildC :: Configuration -> ServerPart Response
rebuildC config = do 
  liftIO $ putStrLn "Will rebuild the db..."
  liftIO $ forkIO $ (runGoStatM config rebuildDB)
  mainPageC config

  
------------------
--  SgfServing  --
------------------

sgfBrowserC :: Configuration -> FilePath -> ServerPart Response
sgfBrowserC config path = do
  file <- liftIO $ readFile path
  ok $ toResponse file

---------------------------
--  A fetching shortcut  --
---------------------------

getParams :: ServerPart (String, Messages)
getParams = do
  movesSoFar <- look "moves" `mplus` (return [])
  langStr    <- look "lang"  `mplus` (return "pl")

  let lang = 
        case langStr of
          "pl" -> pl
          _    -> eng
  return (movesSoFar, lang)

fetchStatsWorker :: String -> GoStatM (Int, (Int, Int, Int))
fetchStatsWorker movesSoFar = do 
  count     <- queryCountDB
  currStats <- queryCurrStatsDB movesSoFar
  return (count, currStats)
  
fetchStats :: Configuration -> ServerPart (Int, (Int, Int, Int), String, Messages)
fetchStats config = do
  (movesSoFar, lang)    <- getParams
  (count, currentStats) <- withConfig config $ fetchStatsWorker movesSoFar
  
  return (count, currentStats, movesSoFar, lang)

-----------------------------------------------
--  The configuration of the online version  --
-----------------------------------------------

onLineBuilders :: UrlBuilders
onLineBuilders = UrlBuilders { mainPageUrl        = "/"
                             , moveBrowserMainUrl = "/movebrowser"
                             , moveBrowserMakeUrl = urlMaker
                             , gameBrowserMakeUrl = gameBrowserUrlMaker
                             , gameDetailsMakeUrl = gameDetailsUrlMaker
                             , gameDownloadLink   = sgfDownloadUrlMaker
                             , imagesMakeUrl      = imageUrlMaker
                             , rebuildUrl         = rebuildUrlMaker
                             , cssUrl             = "/public/style.css"
                             , jsUrls             = ["/public/jquery.js", "/public/highlight.js", "/public/eidogo/player/js/all.compressed.js"]
                             , language           = pl
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
sgfDownloadUrlMaker path = printf "/sgf%s" path

rebuildUrlMaker :: Language -> String
rebuildUrlMaker lang = printf "/rebuild?lang=%s" lang
