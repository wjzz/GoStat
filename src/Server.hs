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
  (count, movesSoFar, lang) <- fetchStats
  
  limit <- (read `fmap` look "limit") `mplus` return 200
  games <- liftIO $ queryGamesListDB movesSoFar limit
  
  ok $ toResponse $ gamesPage games count movesSoFar (onLineConfig { language = lang })

-----------------------------
--  The game details page  --
-----------------------------

gameDetailsC :: ServerPart Response
gameDetailsC = do
  (count, _movesSoFar, lang) <- fetchStats
  gamePathM <- ((\x -> [x]) `fmap` look "path") `mplus` return []
  contents <- liftIO $ mapM (readFile) gamePathM
  
  let sgf = do
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
  (count, movesSoFar, lang) <- fetchStats 
  moves                     <- liftIO $ queryStatsDB movesSoFar  
  
  ok $ toResponse $ moveBrowser count moves movesSoFar (onLineConfig { language = lang })
  
---------------------------
--  A fetching shortcut  --
---------------------------

fetchStats :: ServerPart (Int, String, Messages)
fetchStats = do
  count      <- liftIO $ queryCountDB  
  movesSoFar <- look "moves" `mplus` (return [])
  langStr    <- look "lang"  `mplus` (return "pl")
  
  let lang = 
        case langStr of
          "pl" -> pl
          _    -> eng  
          
  return (count, movesSoFar, lang)

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
sgfDownloadUrlMaker path = printf "/sgf/%s" (drop 48 path)