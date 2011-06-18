{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Pages where

import Data.SGF.Types
import Data.Char
import Data.Function
import Data.List
--import Text.Printf
import Text.XHtml hiding (dir, color, black, white, lang)

import Lang (Language, Message, allLanguages)
import qualified Lang as L

----------------------------------------------------
--  A type for storing the basic data about urls  --
----------------------------------------------------

data Configuration = Configuration { mainPageUrl        :: String
                                   , moveBrowserMainUrl :: String
                                   , moveBrowserMakeUrl :: Language -> String -> String
                                   , imagesMakeUrl      :: String -> String
                                   , cssUrl             :: String 
                                   , language           :: Message
                                   }

---------------------
--  The main page  --
---------------------

mainPage :: Configuration -> Html
mainPage config = pHeader +++ pBody where
  lang = language config
  
  pHeader = header << ((thetitle << L.title lang) 
                       +++ (thelink ! [href (cssUrl config)] ! [thetype "text/css"] ! [rel "stylesheet"] << noHtml))

  -- TODO should this use moveBrowserMainUrl langStr ?
  makeFlag langStr = anchor ! [href (moveBrowserMakeUrl config langStr []) ] 
                     << image ! [width "180" , height "120" , src (imagesMakeUrl config (langStr ++ "_flag.gif"))]
  
  pBody = body $ welcome +++ (concatHtml $ intersperse (primHtml " ") $ map makeFlag allLanguages)
  
  welcome = h1 << "Witaj w programie do go!"
  
  pLinkToMoveBrowser = anchor ! [href (moveBrowserMainUrl config)] << h3 << L.goToMovesBrowser lang


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
board config displayCand moves movesSoFar = dI "boardTable" $ tbl where
  tbl = table ! [border 0] ! [cellspacing 0] ! [cellpadding 0] << (bHeader +++ concatHtml (map row [1..9]))
  bHeader = tr << map (\n -> td << primHtml [n]) ['A'..'I']
  row j = tr << (concatHtml (map field (reverse [1..9])) +++ td << primHtml (show (10-j))) where
    field i = td << anchor ! [href url] << getIntersect config displayCand candMoves (i,j) movesSoFar where
      url       = moveBrowserMakeUrl config langName $ movesSoFar ++ show i ++ show j
      langName  = L.langName (language config)
      candMoves = map (\[a,b] -> (digitToInt a, digitToInt b)) moves'
      moves'    = filter ((==2) . length) moves

-----------------------------
--  The move browser page  --
-----------------------------

moveBrowser :: Int -> [(String, Int, Int, Int)] -> String -> Configuration -> Html
moveBrowser count moves movesSoFar config = pHeader +++ pBody where
  pGameCount = primHtml $ L.gamesInDb lang count
  
  lang = language config
  
  pHeader = header << ((thetitle << L.title lang) 
                       +++ (thelink ! [href (cssUrl config)] ! [thetype "text/css"] ! [rel "stylesheet"] << noHtml))
  
  pBody = body $ concatHtml [ pGameCount
                            , hr
                            , dI "boardInfo" $ boardDiv +++ infoDiv
                            , dI "tables"    $ leftTable +++ rightTable 
                            ]
          
  -- main parts of pBody
  
  pHomePageLink = anchor ! [href (mainPageUrl config)] << h4 << L.backToMain lang
          
  infoDiv = dI "infoBox" $ concatHtml [ movesSoFarField 
                                      , playerToMove
                                      , resetMovesField
                                      , pHomePageLink
                                      ]
            
  boardDiv   = board config True candidates movesSoFar
  
  pMovesList = thediv $ concatHtml [ pMoveHeader
                                     --, board config True candidates movesSoFar
                                   , leftTable
                                   , rightTable ]
               
  -- smaller parts

  langName = L.langName (language config)
  resetMovesField = anchor ! [href (moveBrowserMakeUrl config langName [])] << h2 << L.resetMoves lang
  
  noMoves         = length movesSoFar `div` 2
  movesSoFarField = h4 << (L.numberOfMoves lang ++ show noMoves)  
  
  playerToMoveStr = if blacksTurn then L.black lang else L.white lang
  playerToMove    = h4 << (L.playerToMove lang ++ playerToMoveStr)  
  
  candidates = map (\(a,_,_,_) -> a) moves
  

               
  leftTable  = dI "leftTable"  << (pH1 +++ pMoves movesTotal)
  rightTable = dI "rightTable" << (pH2 +++ pMoves movesPercentage)

  pMoveHeader = h2 << L.availableMoves       lang
  pH1         = h4 << L.movesByTotalCount    lang
  pH2         = h4 << L.movesByWinPercentage lang
  
  movesTotal      = reverse $ sortBy (compare `on` (\(_,t,_,_) -> t)) moves
  movesPercentage = reverse $ sortBy (compare `on` selector) moves
  
  selector (_,t,b,w) = 
    let c = if blacksTurn then b else w
    in (1000*c) `div` t
  
  pMoves mvs = table << (tHeader +++ concatHtml (map makeRow mvs))
  
  blacksTurn = length movesSoFar `mod` 4 == 0
  
  tHeader = concatHtml [ th << L.move lang
                       , th << L.totalPlayed lang
                       , td << L.blackWins lang
                       , td << L.whiteWins lang
                       , td << (if blacksTurn then L.blackWinningPerc lang else L.whiteWinningPerc lang)
                       ]
  
  makeRow (move, count, black, white) = tr << concatHtml [ td << anchor ! [href url] << (moveStrToCoordinates move)
                                                         , td << show count
                                                         , td << show black
                                                         , td << show white
                                                         , td << percentage
                                                         ] where
    percentage = show ((100 * current) `div` count) ++ "%"
    current    = if blacksTurn then black else white
    url        = moveBrowserMakeUrl config langName $ movesSoFar ++ move

    
--------------------------------------
--  HTML building-oriented helpers  --
--------------------------------------

dI :: String -> Html -> Html
dI x = thediv ! [identifier x]

dC :: String -> Html -> Html
dC x = thediv ! [theclass x]
