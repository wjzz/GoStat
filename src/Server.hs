{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Server where

import DB

import Control.Monad
import Control.Monad.Trans
import Data.Function
import Data.List
import Happstack.Server hiding (body)
import Text.Printf
import Text.XHtml hiding (dir)

server :: IO ()
server = do
  putStrLn "Listening on port 8000..."
  simpleHTTP nullConf $ router
  
router :: ServerPart Response
router = msum [ dir "movebrowser" moveBrowserC
              , dir "public" $ serveDirectory EnableBrowsing [] "public"
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
moveBrowserC = do
  movesSoFar <- look "moves" `mplus` (return [])
  moves <- liftIO $ queryStatsDB movesSoFar  
  ok $ toResponse $ moveBrowser moves movesSoFar

moveBrowser :: [(String, Int, Int, Int)] -> String -> Html
moveBrowser moves movesSoFar = pHeader +++ pBody where
  pHeader = header << ((thetitle << "Welcome to Go 9x9 statistics!") 
                       +++ (thelink ! [href "public/style.css"] ! [thetype "text/css"] ! [rel "stylesheet"] << noHtml))
  
  pBody = body $ concatHtml [ pHomePageLink
                            , hr
                            , primHtml "Move browser will be here."
                            , hr
                            , pMovesList
                            ]
  
  pHomePageLink = anchor ! [href "/"] << h3 << "Back to main page"
  
  pMovesList = concatHtml [ pMoveHeader , pMovesSoFar, leftTable, rightTable ]
               
  leftTable  = dI "leftTable"  << (pH1 +++ pMoves movesTotal)
  rightTable = dI "rightTable" << (pH2 +++ pMoves movesPercentage)
  
  insertSeps (a:b:rest) = a:b:'-' : insertSeps rest
  insertSeps _ = []

  pMoveHeader = h2 << "Available moves:"
  pMovesSoFar = h4 << primHtml ("Moves played so far: " ++ insertSeps movesSoFar)
  pH1         = h3 << "Moves by total count:"
  pH2         = h3 << "Moves by black win percentage:"
  
  movesTotal      = reverse $ sortBy (compare `on` (\(_,t,_,_) -> t)) moves
  movesPercentage = reverse $ sortBy (compare `on` (\(_,t,b,_) -> (1000*b) `div` t)) moves
  
  pMoves mvs = table << (tHeader +++ concatHtml (map makeRow mvs))
  
  tHeader = concatHtml [ th << "Move sequence"
                       , th << "Total played"
                       , td << "Black wins"
                       , td << "White wins"
                       , td << "Black winning %"                         
                       ]
  
  makeRow (move, count, black, white) = tr << concatHtml [ td << anchor ! [href url] << move
                                                         , td << show count
                                                         , td << show black
                                                         , td << show white
                                                         , td << percentage
                                                         ] where
    percentage = show ((100 * black) `div` count) ++ "%"
    url = "/movebrowser?moves=" ++ movesSoFar ++ move
    
--------------------------------------
--  HTML building-oriented helpers  --
--------------------------------------

dI :: String -> Html -> Html
dI x = thediv ! [identifier x]

dC :: String -> Html -> Html
dC x = thediv ! [theclass x]
