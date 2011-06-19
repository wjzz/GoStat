{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Server where

import DB
import Pages
import Lang

import Control.Monad
import Control.Monad.Trans
import Happstack.Server hiding (body)

---------------------------------------------------
--  The top-level server and routing procedures  --
---------------------------------------------------

server :: IO ()
server = do
  putStrLn "Listening on port 8000..."
  simpleHTTP nullConf $ router
  
router :: ServerPart Response
router = msum [ dir "movebrowser" moveBrowserC
              , dir "public" $ serveDirectory EnableBrowsing [] "public"
              , mainPageC
              ]
         
---------------------
--  The main page  --
---------------------

mainPageC :: ServerPart Response
mainPageC = do
  ok $ toResponse $ mainPage onLineConfig

----------------------------
--  The moveBrowser page  --
----------------------------

moveBrowserC :: ServerPart Response
moveBrowserC = do
  count      <- liftIO $ queryCountDB  
  movesSoFar <- look "moves" `mplus` (return [])
  langStr    <- look "lang"  `mplus` (return "pl")
  
  let lang = 
        case langStr of
          "pl" -> pl
          _    -> eng
          
  moves <- liftIO $ queryStatsDB movesSoFar  
  ok $ toResponse $ moveBrowser count moves movesSoFar (onLineConfig { language = lang })

-----------------------------------------------
--  The configuration of the online version  --
-----------------------------------------------

onLineConfig :: Configuration
onLineConfig = Configuration { mainPageUrl        = "/"
                             , moveBrowserMainUrl = "/movebrowser"
                             , moveBrowserMakeUrl = urlMaker
                             , imagesMakeUrl      = imageUrlMaker
                             , cssUrl             = "/public/style.css"
                             , jsUrls             = ["/public/jquery.js", "/public/highlight.js"]
                             , language           = eng
                             }

urlMaker :: Language -> String -> String
urlMaker langN movesList = "/movebrowser?lang=" ++ langN ++ "&moves=" ++ movesList

imageUrlMaker :: String -> String
imageUrlMaker s = "/public/img/" ++ s 