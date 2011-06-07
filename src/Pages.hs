{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Pages where

import Data.Char
import Data.Function
import Data.List
import Text.Printf
import Text.XHtml hiding (dir, color, black, white)

----------------------------------------------------
--  A type for storing the basic data about urls  --
----------------------------------------------------

data Configuration = Configuration { mainPageUrl :: String
                                   , moveBrowserMainUrl :: String
                                   , moveBrowserMakeUrl :: String -> String
                                   , imagesMakeUrl :: String -> String
                                   , cssUrl :: String }

---------------------
--  The main page  --
---------------------

mainPage :: Configuration -> Int -> Html
mainPage config count = pHeader +++ pBody where
  pHeader = header << thetitle << "Welcome to Go 9x9 statistics!"
  
  pBody = body $ concatHtml [ pGameCount 
                            , hr
                            , pLinkToMoveBrowser
                            ]
  
  pGameCount = primHtml $ printf "We currently have <b>%d</b> games in the database." count
  
  pLinkToMoveBrowser = anchor ! [href (moveBrowserMainUrl config)] << h3 << "Go to move browser"


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
getImage point str = 
  case lookup point (parseMoves str) of
    Nothing    -> imageFromPoint point
    Just color -> imageFromColor color

getIntersect :: Configuration -> Bool -> [Point] -> Point -> String -> Html
getIntersect config False _     point str = image ! [src (imagesMakeUrl config $ (getImage point str))]
getIntersect config True  candidates point str 
  | point `elem` candidates = primHtml "x"
  | otherwise           = getIntersect config False candidates point str

board :: Configuration -> Bool -> [String] -> String -> Html
board config displayCand moves movesSoFar = dI "boardTable" $  tbl where
  tbl = table ! [border 0] ! [cellspacing 0] ! [cellpadding 0] << (bHeader +++ concatHtml (map row [1..9]))
  bHeader = tr << map (\n -> td << primHtml [n]) ['a'..'i']
  row j = tr << (concatHtml (map field (reverse [1..9])) +++ td << primHtml (show (10-j) ++ ".")) where
    field i = td << anchor ! [href url] << getIntersect config displayCand candMoves (i,j) movesSoFar where
      url = (moveBrowserMakeUrl config) $ movesSoFar ++ show i ++ show j
      candMoves = map (\[a,b] -> (digitToInt a, digitToInt b)) moves'
      moves' = filter ((==2) . length) moves

-----------------------------
--  The move browser page  --
-----------------------------

moveBrowser :: [(String, Int, Int, Int)] -> String -> Configuration -> Html
moveBrowser moves movesSoFar config = pHeader +++ pBody where
  pHeader = header << ((thetitle << "Welcome to Go 9x9 statistics!") 
                       +++ (thelink ! [href (cssUrl config)] ! [thetype "text/css"] ! [rel "stylesheet"] << noHtml))
  
  pBody = body $ concatHtml [ pHomePageLink
                            , hr
                            , anchor ! [href (moveBrowserMakeUrl config [])] << h2 << "Reset moves"
                            , hr
                            , pMovesList
                            ]
  
  pHomePageLink = anchor ! [href (mainPageUrl config)] << h3 << "Back to main page"
  
  candidates = map (\(a,_,_,_) -> a) moves
  
  pMovesList = concatHtml [ pMoveHeader
                          , board config True candidates movesSoFar
                          , leftTable
                          , rightTable ]
               
  leftTable  = dI "leftTable"  << (pH1 +++ pMoves movesTotal)
  rightTable = dI "rightTable" << (pH2 +++ pMoves movesPercentage)

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
    url = (moveBrowserMakeUrl config) (movesSoFar ++ move)

    
--------------------------------------
--  HTML building-oriented helpers  --
--------------------------------------

dI :: String -> Html -> Html
dI x = thediv ! [identifier x]

dC :: String -> Html -> Html
dC x = thediv ! [theclass x]