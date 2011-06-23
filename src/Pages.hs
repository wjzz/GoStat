{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Pages where

import Data.SGF.Types
import Data.Char
import Data.Function
import Data.List
import Text.Printf
import Text.XHtml hiding (dir, color, black, white, lang)

import Lang (Language, Messages, allLanguages, capitalize)
import qualified Lang as L

----------------------------------------------------
--  A type for storing the basic data about urls  --
----------------------------------------------------

type MovesSoFar = String

data Configuration = Configuration { mainPageUrl        :: String
                                   , moveBrowserMainUrl :: String
                                   , moveBrowserMakeUrl :: Language -> MovesSoFar -> String
                                   , gameBrowserMakeUrl :: Language -> MovesSoFar -> String
                                   , gameDetailsMakeUrl :: Language -> FilePath   -> String
                                   , gameDownloadLink   :: FilePath -> String
                                   , imagesMakeUrl      :: String   -> String
                                   , cssUrl             :: String
                                   , jsUrls             :: [String]
                                   , language           :: Messages
                                   }

-----------------------
--  A common header  --
-----------------------

htmlHeader :: Configuration -> Html
htmlHeader config = header << ((thetitle << L.title lang) 
                                 +++ (thelink ! [href (cssUrl config)] ! [thetype "text/css"] ! [rel "stylesheet"] << noHtml)
                                 +++ script ! [thetype "text/javascript"] << eidogoConfig
                                 +++ concatHtml (map buildScript $ jsUrls config)) 
  where
    buildScript url = script ! [thetype "text/javascript",src url] << noHtml --
    lang            = language config
    
    eidogoConfig = primHtml " \
  \  eidogoConfig = { \
  \      theme:          \"standard\", \
  \      mode:           \"view\", \
  \      showComments:    true, \ 
  \      showPlayerInfo:  false, \
  \      showGameInfo:    false, \
  \      showTools:       false, \
  \      showOptions:     true, \
  \      showNavTree:     false, \
  \      markCurrent:     true, \
  \      markVariations:  false, \ 
  \      markNext:        false, \
  \      problemMode:     false, \
  \      enableShortcuts: false \
  \  };"

---------------------------------
--  Header used in most pages  --
---------------------------------

globalHeader :: Int -> Configuration -> (Language -> String) -> Html
globalHeader count config makeUrl = concatHtml [ flags 
                                               , br 
                                               , br
                                               , primHtml $ L.gamesInDb lang count
                                               , pHomePageLink 
                                               ] where
  
  pHomePageLink = h4 << anchor ! [href (moveBrowserMainUrl config)] << L.backToMain lang
  lang          = language config
  
  makeFlag langStr = anchor ! [href (makeUrl langStr) ] 
                     << (thespan ! [theclass "flag"] $ (image ! [width "36" , height "24" , 
                                                                 src (imagesMakeUrl config (langStr ++ "_flag.gif"))]))
  
  flags = concatHtml $ intersperse (primHtml " ") $ map makeFlag allLanguages


---------------------
--  The main page  --
---------------------

mainPage :: Configuration -> Html
mainPage config = pHeader +++ pBody where
  lang    = language config
  pHeader = htmlHeader config

  -- TODO should this use moveBrowserMainUrl langStr ?
  makeFlag langStr = anchor ! [href (moveBrowserMakeUrl config langStr []) ] 
                     << image ! [width "180" , height "120" , src (imagesMakeUrl config (langStr ++ "_flag.gif"))]
  
  pBody = body $ welcome +++ (concatHtml $ intersperse (primHtml " ") $ map makeFlag allLanguages)
  
  welcome = h1 << "Witaj w programie do go!"
  
  pLinkToMoveBrowser = anchor ! [href (moveBrowserMainUrl config)] << h3 << L.goToMovesBrowser lang

------------------------------
--  The games browser page  --
------------------------------

gamesPage :: [FilePath] -> Int -> (Int, Int, Int) -> String -> Configuration -> Html
gamesPage games count (allGames, bWin, wWin) movesSoFar config = pHeader +++ pBody where
  lang       = language config
  pHeader    = htmlHeader config
  
  pBody = body $ concatHtml [ globalHeader count config (\l -> gameBrowserMakeUrl config l movesSoFar)
                            , hr
                            , navigation
                            , hr
                            , concatHtml $ intersperse br $ map makeLink games
                            ]          
  
  navigation = concatHtml [ currentPosition , br , br
                          , numberOfGames , br , br
                          , currentPositionWinningChance , br , br
                          , numberOfShownGames
                          ]
  blacksTurn = length movesSoFar `mod` 4 == 0          
  percentage = (100 * current) `div` allGames where
    current = if blacksTurn then bWin else wWin
    
  currentPosition              = anchor ! [ href mBrowserUrl] << primHtml (L.showCurrentPosition lang) where
    mBrowserUrl = moveBrowserMakeUrl config (L.langName lang) movesSoFar
  numberOfGames                = primHtml $ printf "%s %d"   (L.numberOfGames lang) allGames  
  currentPositionWinningChance = primHtml $ printf "%s %d%%" (L.chanceOfWinning lang) percentage
  numberOfShownGames           = primHtml $ L.noOfShownGames lang
          
  makeLink game = anchor ! [href url] << primHtml game where
    url = gameDetailsMakeUrl config (L.langName lang) game

-----------------------------
--  The game details page  --
-----------------------------

gameDetailsPage :: Int -> Maybe SGF -> Maybe FilePath -> Configuration -> Html
gameDetailsPage _ _           Nothing     config = primHtml $ printf "No game specifed."
gameDetailsPage _ Nothing     (Just path) config = primHtml $ printf "Game %s not found. An internal error might have occured" path
gameDetailsPage count (Just game) (Just path) config = pHeader +++ pBody where
  lang       = language config
  pHeader    = htmlHeader config
  
  pBody = body $ concatHtml [ globalHeader count config (\l -> gameDetailsMakeUrl config l path)
                            , hr
                            , gameInContext
                            , hr
                            , dI "gameLeft" $ concatHtml [ gameSummary 
                                                         , br
                                                         , downloadGame 
                                                         , br
                                                         , br
                                                         , finalPosition 
                                                         , italics $ primHtml(L.finalPosition lang)
                                                         ]
                            , dI "gameRight" eidogo
                            ]
          
  gameInContext = anchor ! [href mBrowserUrl] << (L.showInContext lang) where
    mBrowserUrl = moveBrowserMakeUrl config (L.langName lang) movesSoFar

  eidogo = dC "eidogo-player-auto" ! [strAttr "sgf" (gameDownloadLink config path)] << noHtml

  downloadGame = anchor ! [href (gameDownloadLink config path)] << (L.downloadSgf lang) 
          
  movesSoFar = getMovesStr game
                 
  finalPosition = board config True [] movesSoFar
  
  (blk, wht, res, dt) = sgfSummary game
  
  result = 
    case res of
      Unfinished  -> L.noResult lang
      Win Black _ -> "B+"
      Win White _ -> "W+"
  
  gameSummary = dI "gameSummary" $ table << bData
  
  labels = [capitalize (L.black lang), capitalize (L.white lang), L.result lang, L.date lang]
  values = [blk, wht, result, dt]

  bData   = map (\(l,v) -> tr << ((td << (l ++ ":")) +++ (td << v))) (zip labels values)

-----------------------------
--  The move browser page  --
-----------------------------

moveBrowser :: Int -> (Int, Int, Int) -> [(String, Int, Int, Int)] -> String -> Configuration -> Html
moveBrowser count (allGames, bWin, wWin) moves movesSoFar config = pHeader +++ pBody where
  lang       = language config
  pHeader    = htmlHeader config
  
  pBody = body $ concatHtml [ globalHeader count config (\l -> moveBrowserMakeUrl config l movesSoFar)
                            , hr
                            , currentStatistics
                            , hr
                            , dI "boardInfo" $ boardDiv  +++ infoDiv
                            , dI "tables"    $ leftTable +++ rightTable 
                            ]
          
  -- main parts of pBody
  
  pHomePageLink = anchor ! [href (mainPageUrl config)] << h4 << L.backToMain lang
          
  infoDiv = dI "infoBox" $ concatHtml [ movesSoFarField 
                                      , playerToMove
                                      , takeBackLink
                                      , resetMovesField
                                      , pHomePageLink
                                      ]
            
  boardDiv   = board config True candidates movesSoFar
  
  pMovesList = thediv $ concatHtml [ pMoveHeader
                                     --, board config True candidates movesSoFar
                                   , leftTable
                                   , rightTable ]
               
  -- smaller parts
  
  currentStatistics = concatHtml [ currentPositionWinningChance
                                 , br , br
                                 , numberOfGames
                                 , br , br
                                 , matchingGames
                                 ]

  percentage                   = (100 * current) `div` allGames where
    current = if blacksTurn then bWin else wWin
  currentPositionWinningChance = primHtml $ printf "%s %d%%" (L.chanceOfWinning lang) percentage
  numberOfGames                = primHtml $ printf "%s %d"   (L.numberOfGames lang) allGames
  matchingGames                = anchor ! [href gamesUrl] << primHtml (L.matchingGamesList lang) where
    gamesUrl = gameBrowserMakeUrl config langName movesSoFar

  langName = L.langName (language config)
  takeBackLink 
    | null movesSoFar = noHtml 
    | otherwise       = anchor ! [href (moveBrowserMakeUrl config langName (init (init movesSoFar)))] 
                        << h3 << L.takeBackMove lang
  
  resetMovesField 
    | null movesSoFar = noHtml
    | otherwise       = anchor ! [href (moveBrowserMakeUrl config langName [])] << h3 << L.resetMoves lang
  
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
                       , th << L.blackWins lang
                       , th << L.whiteWins lang
                       , th << (if blacksTurn then L.blackWinningPerc lang else L.whiteWinningPerc lang)
                       ]
  
  makeRow (move, count, black, white) = tr ! [ identifier trid] << concatHtml [ td << moveField
                                                                              , td << countField
                                                                              , td << show black
                                                                              , td << show white
                                                                              , td << percentage
                                                                              ] where
    percentage     = show ((100 * current) `div` count) ++ "%"
    current        = if blacksTurn then black else white
    url            = moveBrowserMakeUrl config langName $ movesSoFar ++ move

    countField = anchor ! [href gamesUrl ] << show count where
      gamesUrl = gameBrowserMakeUrl config langName $ movesSoFar ++ move

    moveField  
      | null move = anchor ! [href gameDetailsUrl] << L.gameOver lang
      | otherwise = anchor ! [href url]            << thespan ! attrs << (moveStrToCoordinates move) where
        gameDetailsUrl = gameBrowserMakeUrl config langName movesSoFar
        attrs      = [ identifier idd
                     , strAttr "onMouseover" (printf "lstMouseOver(\"%s\",\"%s\")" imgUrl move)
                     , strAttr "onMouseout"  (printf "lstMouseOut(\"%s\")"  move)
                     ]
    imgUrl     = imagesMakeUrl config $ if blacksTurn then moveFromColor B else moveFromColor W
    idd        = "lst" ++ move
    trid       = "tr"  ++ move

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


moveFromColor :: Color -> String
moveFromColor B = "bm.gif"
moveFromColor W = "wm.gif"

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

getIntersect :: String -> String -> Configuration -> Bool -> [Point] -> Point -> String -> Html
getIntersect _ _        config False _     point str = image ! [src (imagesMakeUrl config $ (getImage point str))]
getIntersect imgUrl url config True  candidates point@(i,j) str 
  | point `elem` candidates = 
    let 
      move = show i ++ show j
      attrs = [ identifier $ "brd" ++ move
              , strAttr "onMouseover" (printf "brdMouseOver(\"%s\", \"%s\")" imgUrl move)
              , strAttr "onMouseout"  (printf "brdMouseOut(%s)"  move)
              ]
    in  
     anchor ! [href url] << thespan ! attrs << primHtml "x"                              
  | otherwise = getIntersect imgUrl url config False candidates point str

board :: Configuration -> Bool -> [String] -> String -> Html
board config displayCand moves movesSoFar = dI "boardTable" $ tbl where
  tbl = table ! [border 0] ! [cellspacing 0] ! [cellpadding 0] << (bHeader +++ concatHtml (map row [1..9]))
  
  bHeader = tr << map (\n -> td << primHtml [n]) ['A'..'I']
  
  row j = tr << (concatHtml (map field (reverse [1..9])) +++ td << primHtml (show (10-j))) where
    field i = td << getIntersect imgUrl url config displayCand candMoves (i,j) movesSoFar where
      imgUrl    = imagesMakeUrl config (if length movesSoFar `mod` 4 == 0 then moveFromColor B else moveFromColor W)
      url       = moveBrowserMakeUrl config langName $ movesSoFar ++ show i ++ show j
      langName  = L.langName (language config)
      candMoves = map (\[a,b] -> (digitToInt a, digitToInt b)) moves'
      moves'    = filter ((==2) . length) moves

    
--------------------------------------
--  HTML building-oriented helpers  --
--------------------------------------

dI :: String -> Html -> Html
dI x = thediv ! [identifier x]

dC :: String -> Html -> Html
dC x = thediv ! [theclass x]
