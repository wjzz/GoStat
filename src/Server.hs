{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Server where

import DB
import Pages

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
  count <- liftIO $ queryCountDB  
  ok $ toResponse $ mainPage count

----------------------------
--  The moveBrowser page  --
----------------------------

moveBrowserC :: ServerPart Response
moveBrowserC = do
  movesSoFar <- look "moves" `mplus` (return [])
  moves <- liftIO $ queryStatsDB movesSoFar  
  ok $ toResponse $ moveBrowser moves movesSoFar
