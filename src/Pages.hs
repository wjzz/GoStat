{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Pages where

import Data.Char
import Data.Function
import Data.List
import Text.Printf
import Text.XHtml hiding (dir, color, black, white, lang, link)

import Configuration
import Data.SGF.Types hiding (moves)
import Lang (Language, Messages, allLanguages, capitalize)
import qualified Lang as L

----------------------------------------------------
--  A type for storing the basic data about urls  --
----------------------------------------------------

type MovesSoFar = String

data UrlBuilders = UrlBuilders { mainPageUrl        :: Language -> String
                               , moveBrowserMainUrl :: Language -> String
                               , moveBrowserMakeUrl :: Language -> MovesSoFar -> String
                               , gameBrowserMakeUrl :: Language -> MovesSoFar -> String
                               , gameDetailsMakeUrl :: Language -> Int        -> String
                               , configureUrl       :: Language -> String
                               , rebuildUrl         :: Language -> String
                               
                               , gameDownloadLink   :: FilePath -> String
                               , imagesMakeUrl      :: String   -> String
                               , cssUrl             :: String
                               , jsUrls             :: [String]
                               , language           :: Messages
                               }

-----------------------
--  A common header  --
-----------------------

htmlHeader :: UrlBuilders -> Html
htmlHeader urlBuilder = header << ((thetitle << L.title lang) 
                                 +++ (thelink ! [href (cssUrl urlBuilder)] ! [thetype "text/css"] ! [rel "stylesheet"] << noHtml)
                                 +++ script ! [thetype "text/javascript"] << eidogoConfig
                                 +++ concatHtml (map buildScript $ jsUrls urlBuilder)) 
  where
    buildScript url = script ! [thetype "text/javascript",src url] << noHtml --
    lang            = language urlBuilder
        
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

globalHeader :: Int -> UrlBuilders -> (Language -> String) -> Html
globalHeader count urlBuilder makeUrl = dI "menu" (linksMenu +++ flagsMenu) where
  linksMenu = dI "links" (unordList [pStartPageLink, pHomePageLink])
  flagsMenu = dI "flags" (unordList $ map makeFlag allLanguages)
  
  pHomePageLink = anchor ! [href (moveBrowserMainUrl urlBuilder lName)] << L.backToMain lang
  
  pStartPageLink = anchor ! [href (mainPageUrl urlBuilder lName)] << L.startPage lang
  lang          = language urlBuilder
  lName         = L.langName lang
  
  makeFlag langStr = anchor ! [href (makeUrl langStr) ] 
                     << (thespan ! [theclass "flag"] $ (image ! [width "36" , height "24" , 
                                                                 src (imagesMakeUrl urlBuilder (langStr ++ "_flag.gif"))]))
  
  flags = dI "flags" $ concatHtml $ intersperse (primHtml " ") $ map makeFlag allLanguages


---------------------
--  The main page  --
---------------------

mainPage :: UrlBuilders -> Html
mainPage urlBuilder = pHeader +++ pBody where
  lang    = language urlBuilder
  pHeader = htmlHeader urlBuilder

  makeFlag langStr = anchor ! [href (mainPageUrl urlBuilder langStr) ] 
                     << image ! [width "180" , height "120" , src (imagesMakeUrl urlBuilder (langStr ++ "_flag.gif"))]
  
  pBody = body $ concatHtml [ welcome 
                            , flags
                            , hr
                            , anchor ! [href (moveBrowserMakeUrl urlBuilder (L.langName lang) [])] << L.goToMovesBrowser lang
                            , br , br
                            , anchor ! [href (configureUrl urlBuilder (L.langName lang))] << L.config lang
                            , br , br
                            , anchor ! [htmlAttr "href" rebuildJS] << L.rebuild lang
                            ]
          
  rebuildJS = primHtml $ printf "javascript:rebuildConfirm('%s','%s')" (L.confirm lang) (rebuildUrl urlBuilder (L.langName lang)) 
  rebuild =  (rebuildUrl urlBuilder (L.langName lang))
  
  flags = (concatHtml $ intersperse (primHtml " ") $ map makeFlag allLanguages)
  
  welcome = h1 << (L.welcome lang)
  
  --pLinkToMoveBrowser = anchor ! [href (moveBrowserMainUrl urlBuilder)] << h3 << L.goToMovesBrowser lang

------------------------------
--  The games browser page  --
------------------------------

gameBrowserPage :: [(Int, FilePath, String, String, String, String, String)] -> Int -> (Int, Int, Int) -> String -> UrlBuilders -> Html
gameBrowserPage gameInfos count (allGames, bWin, wWin) movesSoFar urlBuilder = pHeader +++ pBody where
  lang       = language urlBuilder
  pHeader    = htmlHeader urlBuilder
  
  pBody = body $ concatHtml [ globalHeader count urlBuilder (\l -> gameBrowserMakeUrl urlBuilder l movesSoFar)
                            , navigation
                            , hr
                            , dI "gameListTable" gameList
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
    mBrowserUrl = moveBrowserMakeUrl urlBuilder (L.langName lang) movesSoFar
  numberOfGames                = primHtml $ printf "%s %d"   (L.numberOfGames lang) allGames  
  currentPositionWinningChance = primHtml $ printf "%s %d%%" (L.chanceOfWinning lang) percentage
  numberOfShownGames           = primHtml $ printf "%s %d"   (L.noOfShownGames lang) (length gameInfos)
          
  makeLink (gameId, gamePath, winner, bName, wName, bRank, wRank) = 
    tr $ concatHtml $ map td (map primHtml  [show gameId, bName, bRank, wName, wRank, winner] ++ [link]) where
      link = anchor ! [href url] << primHtml gamePath
      url  = gameDetailsMakeUrl urlBuilder (L.langName lang) gameId
  
  gameList = table << (tHeader +++ (concatHtml $ map makeLink gameInfos))
  tHeader = tr $ concatHtml $ map (\l -> th << l) labels
  labels  = map (flip ($) lang) [L.number, L.black, L.blackRank, L.white, L.whiteRank, L.winner, L.link] 

-----------------------------
--  The game details page  --
-----------------------------

type GameId = Int

gameDetailsPage :: Int -> GameId -> SGF -> FilePath -> MovesSoFar -> UrlBuilders -> Html
gameDetailsPage count gameId game path movesSoFar urlBuilder = pHeader +++ pBody where
  lang       = language urlBuilder
  pHeader    = htmlHeader urlBuilder
  
  pBody = body $ concatHtml [ globalHeader count urlBuilder (\l -> gameDetailsMakeUrl urlBuilder l gameId)
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
    mBrowserUrl = moveBrowserMakeUrl urlBuilder (L.langName lang) movesSoFar

  eidogo = dC "eidogo-player-auto" ! [strAttr "sgf" (gameDownloadLink urlBuilder path)] << noHtml

  downloadGame = anchor ! [href (gameDownloadLink urlBuilder path)] << (L.downloadSgf lang) 
          
  --movesSoFar = getMovesStr game
                 
  finalPosition = board urlBuilder True [] movesSoFar
  
  (blk, wht, res, dt) = sgfSummary game
  
  result = 
    case res of
      Unfinished  -> L.noResult lang
      Win Black _ -> "B+"
      Win White _ -> "W+"
      Draw        -> error "Found a draw: impossible case!"
  
  gameSummary = dI "gameSummary" $ table << bData
  
  labels = [capitalize (L.black lang), capitalize (L.white lang), L.result lang, L.date lang]
  values = [blk, wht, result, dt]

  bData   = map (\(l,v) -> tr << ((td << (l ++ ":")) +++ (td << v))) (zip labels values)

-------------------------------
--  The configuration forms  --
-------------------------------

configForm :: Int -> Configuration -> UrlBuilders -> Html
configForm count configuration urlBuilder = pHeader +++ pBody where
  pHeader    = htmlHeader urlBuilder
  lang       = language urlBuilder
  
  pBody = body $ concatHtml [ globalHeader count urlBuilder (configureUrl urlBuilder)
                            , h1 << L.configurationForm lang
                            , hr
                            , cForm
                            ]
          
  cForm = form ! [method "POST"] $ concatHtml [ primHtml dbLabel
                                              , br
                                              , dbSelectForm
                                              , br
                                              , br
                                              , primHtml sqliteLabel
                                              , br
                                              , sqlitePath
                                              , br
                                              , br
                                              , primHtml dirsLabel
                                              , br
                                              , dirsForm
                                              , br
                                              , submitButton
                                              ]
  dbLabel      = L.databaseLabel lang
  dbSelectForm = select ! [name "dbServer"] $ concatHtml $ map makeOption ["PostgreSQL", "SQLite3"] where
        
                 
  currentDb = 
    case dbServer configuration of
      PostgreSQL -> "postgresql"
      Sqlite3 _  -> "sqlite3"
                 
  makeOption dbName = option ! [value lowered] ! attrs $ primHtml dbName where
    lowered = map toLower dbName
    attrs   = if lowered == currentDb then [selected] else []
  

  sqliteLabel = L.sqlite3Location lang
  sqlitePath  = primHtml $ printf "<textarea name=\"sqlitePath\" cols=\"100\" rows=\"1\">%s</textarea>" dbPath
  
  dbPath = 
    case dbServer configuration of
      PostgreSQL -> ""
      Sqlite3 p -> p
    
  
  dirsLabel = L.sgfDirectories lang
  dirsForm = primHtml $ printf "<textarea name=\"dirs\" cols=\"100\" rows=\"10\">%s</textarea>" (unlines (gameDirs configuration))
  
  submitButton = submit "action" (L.submitChanges lang)
  

-----------------------------
--  The move browser page  --
-----------------------------

moveBrowser :: Int -> (Int, Int, Int) -> [(String, Int, Int, Int)] -> String -> UrlBuilders -> Html
moveBrowser count (allGames, bWin, wWin) moves movesSoFar urlBuilder = pHeader +++ pBody where
  lang       = language urlBuilder
  pHeader    = htmlHeader urlBuilder
  
  pBody = body $ concatHtml [ globalHeader count urlBuilder (\l -> moveBrowserMakeUrl urlBuilder l movesSoFar)
                            , currentStatistics
                            , hr
                            , dI "boardInfo" $ boardDiv  +++ infoDiv
                            , dI "tables"    $ leftTable +++ rightTable 
                            ]
          
  -- main parts of pBody
          
  infoDiv = dI "infoBox" $ concatHtml [ movesSoFarField 
                                      , playerToMove
                                      , takeBackLink
                                      , resetMovesField
                                      ]
            
  boardDiv   = board urlBuilder True candidates movesSoFar
                 
  -- smaller parts
  
  currentStatistics = concatHtml [ primHtml $ L.gamesInDb lang count
                                 , br , br
                                 , currentPositionWinningChance
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
    gamesUrl = gameBrowserMakeUrl urlBuilder langName movesSoFar

  langName = L.langName (language urlBuilder)
  takeBackLink 
    | null movesSoFar = noHtml 
    | otherwise       = anchor ! [href (moveBrowserMakeUrl urlBuilder langName (init (init movesSoFar)))] 
                        << h3 << L.takeBackMove lang
  
  resetMovesField 
    | null movesSoFar = noHtml
    | otherwise       = anchor ! [href (moveBrowserMakeUrl urlBuilder langName [])] << h3 << L.resetMoves lang
  
  noMoves         = length movesSoFar `div` 2
  movesSoFarField = h4 << (L.numberOfMoves lang ++ show noMoves)  
  
  playerToMoveStr = if blacksTurn then L.black lang else L.white lang
  playerToMove    = h4 << (L.playerToMove lang ++ playerToMoveStr)  
  
  candidates = map (\(a,_,_,_) -> a) moves
  

               
  leftTable  = dI "leftTable"  << (pH1 +++ pMoves movesTotal)
  rightTable = dI "rightTable" << (pH2 +++ pMoves movesPercentage)

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
  
  makeRow (move, cnt, blk, wht) = tr ! [ identifier trid] << concatHtml [ td << moveField
                                                                              , td << countField
                                                                              , td << show blk
                                                                              , td << show wht
                                                                              , td << localPercentage
                                                                              ] where
    localPercentage = show ((100 * current) `div` cnt) ++ "%"
    current         = if blacksTurn then blk else wht
    url             = moveBrowserMakeUrl urlBuilder langName $ movesSoFar ++ move

    countField = anchor ! [href gamesUrl ] << show cnt where
      gamesUrl = gameBrowserMakeUrl urlBuilder langName $ movesSoFar ++ move

    moveField  
      | null move = anchor ! [href gameDetailsUrl] << L.gameOver lang
      | otherwise = anchor ! [href url]            << thespan ! attrs << (moveStrToCoordinates move) where
        gameDetailsUrl = gameBrowserMakeUrl urlBuilder langName movesSoFar
        attrs      = [ identifier idd
                     , strAttr "onMouseover" (printf "lstMouseOver(\"%s\",\"%s\")" imgUrl move)
                     , strAttr "onMouseout"  (printf "lstMouseOut(\"%s\")"  move)
                     ]
    imgUrl     = imagesMakeUrl urlBuilder $ if blacksTurn then moveFromColor B else moveFromColor W
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

getIntersect :: String -> String -> UrlBuilders -> Bool -> [Point] -> Point -> String -> Html
getIntersect _ _        urlBuilder False _     point str = image ! [src (imagesMakeUrl urlBuilder $ (getImage point str))]
getIntersect imgUrl url urlBuilder True  candidates point@(i,j) str 
  | point `elem` candidates = 
    let 
      move = show i ++ show j
      attrs = [ identifier $ "brd" ++ move
              , strAttr "onMouseover" (printf "brdMouseOver(\"%s\", \"%s\")" imgUrl move)
              , strAttr "onMouseout"  (printf "brdMouseOut(%s)"  move)
              ]
    in  
     anchor ! [href url] << thespan ! attrs << primHtml "x"                              
  | otherwise = getIntersect imgUrl url urlBuilder False candidates point str

board :: UrlBuilders -> Bool -> [String] -> String -> Html
board urlBuilder displayCand moves movesSoFar = dI "boardTable" $ tbl where
  tbl = table ! [border 0] ! [cellspacing 0] ! [cellpadding 0] << (bHeader +++ concatHtml (map row [1..9]))
  
  bHeader = tr << map (\n -> td << primHtml [n]) ['A'..'I']
  
  row j = tr << (concatHtml (map field (reverse [1..9])) +++ td << primHtml (show (10-j))) where
    field i = td << getIntersect imgUrl url urlBuilder displayCand candMoves (i,j) movesSoFar where
      imgUrl    = imagesMakeUrl urlBuilder (if length movesSoFar `mod` 4 == 0 then moveFromColor B else moveFromColor W)
      url       = moveBrowserMakeUrl urlBuilder langName $ movesSoFar ++ show i ++ show j
      langName  = L.langName (language urlBuilder)
      candMoves = map (\[a,b] -> (digitToInt a, digitToInt b)) moves'
      moves'    = filter ((==2) . length) moves

    
--------------------------------------
--  HTML building-oriented helpers  --
--------------------------------------

dI :: String -> Html -> Html
dI x = thediv ! [identifier x]

dC :: String -> Html -> Html
dC x = thediv ! [theclass x]
