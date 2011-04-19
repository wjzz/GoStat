{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Server where

import DB

import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.Function
import Data.List
import Happstack.Server hiding (body)
import Text.Printf
import Text.XHtml hiding (dir)

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

----------------------------------------
--  Functions for drawing a go board  --
----------------------------------------
  
type Point = (Int, Int)

{-  91 11
    99 19 -}

-- |Returns the name of the image that should be used in the given point (assuming it's an empty intersection)
imageFromPoint :: Point -> String
imageFromPoint (1,1) = "ur.gif"
imageFromPoint (9,1) = "ul.gif"
imageFromPoint (9,9) = "dl.gif"
imageFromPoint (1,9) = "dr.gif"
imageFromPoint (_,1) = "u.gif"
imageFromPoint (_,9) = "d.gif"
imageFromPoint (1,_) = "er.gif"
imageFromPoint (9,_) = "el.gif"
imageFromPoint _     = "e.gif"


data Color = B | W

otherColor :: Color -> Color
otherColor B = W
otherColor W = B

imageFromColor :: Color -> String
imageFromColor B = "b.gif"
imageFromColor W = "w.gif"


parseMoves :: String -> [(Point, Color)]
parseMoves = parseMoves' B

parseMoves' :: Color -> String -> [(Point, Color)]
parseMoves' color (x:y:rest) = ((digitToInt x, digitToInt y), color) : parseMoves' (otherColor color) rest
parseMoves' _ _              = []

getImage :: Point -> String -> String
getImage p str = 
  case lookup p (parseMoves str) of
    Nothing    -> imageFromPoint p
    Just color -> imageFromColor color

getIntersect :: Bool -> [Point] -> Point -> String -> Html
getIntersect False _     p str = image ! [src ("/public/img/" ++ getImage p str)]
getIntersect True  candidates p str 
  | p `elem` candidates = primHtml "x"
  | otherwise           = getIntersect False candidates p str

board :: Bool -> [String] -> String -> Html
board displayCand moves movesSoFar = dI "boardTable" $  tbl where
  tbl = table ! [border 0] ! [cellspacing 0] ! [cellpadding 0] << (bHeader +++ concatHtml (map row [1..9]))
  bHeader = tr << map (\n -> td << primHtml [n]) ['a'..'i']
  row j = tr << (concatHtml (map field (reverse [1..9])) +++ td << primHtml (show (10-j) ++ ".")) where
    field i = td << anchor ! [href url] << getIntersect displayCand candMoves (i,j) movesSoFar where
      url = "/movebrowser?moves=" ++ movesSoFar ++ show i ++ show j
      candMoves = map (\[a,b] -> (digitToInt a, digitToInt b)) moves

----------------------------
--  The moveBrowser page  --
----------------------------

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
                            , anchor ! [href "/movebrowser"] << h2 << "Reset moves"
                            , hr
                            , pMovesList
                            ]
  
  pHomePageLink = anchor ! [href "/"] << h3 << "Back to main page"
  
  candidates = map (\(a,_,_,_) -> a) moves
  
  pMovesList = concatHtml [ pMoveHeader
                          , board True candidates movesSoFar
                          , leftTable
                          , rightTable ]
               
  leftTable  = dI "leftTable"  << (pH1 +++ pMoves movesTotal)
  rightTable = dI "rightTable" << (pH2 +++ pMoves movesPercentage)
  
  insertSeps (a:b:rest) = a:b:'-' : insertSeps rest
  insertSeps _ = []

  pMoveHeader = h2 << "Available moves:"
  pH1         = h3 << "Moves by total count:"
  pH2         = h3 << "Moves by win percentage:"
  
  movesTotal      = reverse $ sortBy (compare `on` (\(_,t,_,_) -> t)) moves
  movesPercentage = reverse $ sortBy (compare `on` selector) moves
  
  selector (_,t,b,w) = 
    let c = if blackTurn then b else w
    in (1000*c) `div` t
  
  pMoves mvs = table << (tHeader +++ concatHtml (map makeRow mvs))
  
  blackTurn = length movesSoFar `mod` 4 == 0
  
  tHeader = concatHtml [ th << "Move"
                       , th << "Total played"
                       , td << "Black wins"
                       , td << "White wins"
                       , td << ((if blackTurn then "Black" else "White") ++ " winning %")
                       ]
  
  makeRow (move, count, black, white) = tr << concatHtml [ td << anchor ! [href url] << move
                                                         , td << show count
                                                         , td << show black
                                                         , td << show white
                                                         , td << percentage
                                                         ] where
    percentage = show ((100 * current) `div` count) ++ "%"
    current = if blackTurn then black else white
    url = "/movebrowser?moves=" ++ movesSoFar ++ move
    
--------------------------------------
--  HTML building-oriented helpers  --
--------------------------------------

dI :: String -> Html -> Html
dI x = thediv ! [identifier x]

dC :: String -> Html -> Html
dC x = thediv ! [theclass x]
