{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Server where

import DB

import Control.Monad
import Control.Monad.Trans
import Happstack.Server hiding (body)
import Text.Printf
import Text.XHtml hiding (dir)

server :: IO ()
server = do
  putStrLn "Listening on port 8000..."
  simpleHTTP nullConf $ router
  
router :: ServerPart Response
router = msum [ dir "movebrowser" moveBrowserC
              , mainPageC
              ]
         
mainPageC :: ServerPart Response
mainPageC = do
  -- fetch count from the DB
  count <- liftIO $ queryCountDB
  
  ok $ toResponse $ mainPage count

mainPage :: Int -> Html
mainPage count = pHeader +++ pBody where
  pHeader = header << thetitle << "Welcome to Go 9x9 statistics!"
  
  pBody = body $ concatHtml [ pGameCount 
                            , hr
                            , pLinkToMoveBrowser
                            ]
  
  pGameCount = primHtml $ printf "We currently have <b>%d</b> games in the database." count
  
  pLinkToMoveBrowser = anchor ! [href "/movebrowser"] << h3 << "Go to move browser"


moveBrowserC :: ServerPart Response
moveBrowserC = ok $ toResponse moveBrowser

moveBrowser :: Html
moveBrowser = pHeader +++ pBody where
  pHeader = header << thetitle << "Welcome to Go 9x9 statistics!"
  
  pBody = body $ concatHtml [ pHomePageLink
                            , hr
                            , primHtml "Move browser will be here."
                            ]
  
  pHomePageLink = anchor ! [href "/"] << h3 << "Back to main page"