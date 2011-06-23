{-# OPTIONS -Wall #-}
{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Lang where

import Data.Char

type Language = String

data Messages = Messages { langName             :: Language
                         , title                :: String
                         , gamesInDb            :: Int -> String
                         , goToMovesBrowser     :: String
                         , black                :: String
                         , white                :: String
                         , backToMain           :: String
                         , numberOfMoves        :: String
                         , playerToMove         :: String
                         , takeBackMove         :: String
                         , resetMoves           :: String
                         , availableMoves       :: String
                         , movesByTotalCount    :: String
                         , movesByWinPercentage :: String
                         , move                 :: String
                         , totalPlayed          :: String
                         , blackWins            :: String
                         , whiteWins            :: String
                         , whiteWinningPerc     :: String
                         , blackWinningPerc     :: String
                         , gameOver             :: String
                         , noResult             :: String
                         , result               :: String
                         , date                 :: String
                         , downloadSgf          :: String
--                         , show
                         }

eng :: Messages
eng = Messages { langName             = "eng"
               , title                = "Welcome to Go 9x9 statistics!" 
               , gamesInDb            = \n -> "We currently have <b>" ++ show n ++ "</b> games in the database." 
               , goToMovesBrowser     = "Go to move browser"
               , black                = "black" 
               , white                = "white" 
               , backToMain           = "Back to main page" 
               , numberOfMoves        = "Number of moves so far: "
               , playerToMove         = "Player to move: "
               , takeBackMove         = "Take back last move"
               , resetMoves           = "Reset moves" 
               , availableMoves       = "Available moves:"
               , movesByTotalCount    = "Moves by total count:" 
               , movesByWinPercentage = "Moves by win percentage:" 
               , move                 = "Move" 
               , totalPlayed          = "Total played" 
               , blackWins            = "Black wins" 
               , whiteWins            = "White wins"
               , whiteWinningPerc     = "White winning %"
               , blackWinningPerc     = "Black winning %"
               , gameOver             = "End"
               , noResult             = "No result"
               , result               = "Result"
               , date                 = "Date"
               , downloadSgf          = "Download game record (.sgf)"
               }

pl :: Messages
pl =  Messages { langName             = "pl"
               , title                = "Statystyki gier go na planszy 9x9"
               , gamesInDb            = \n -> "Liczba zapisów gier w bazie: <b>" ++ show n ++ "</b>." 
               , goToMovesBrowser     = "Przeglądarka ruchów"
               , black                = "czarny" 
               , white                = "biały" 
               , backToMain           = "Powrót do strony głównej" 
               , numberOfMoves        = "Ruchów do tej pory: "
               , playerToMove         = "Ruch ma gracz: "
               , takeBackMove         = "Cofnij ostatni ruch"
               , resetMoves           = "Od nowa" 
               , availableMoves       = "Dostępne ruchy:"
               , movesByTotalCount    = "Sortowanie wg częstości:" 
               , movesByWinPercentage = "Sortowanie wg skuteczności:" 
               , move                 = "Ruch" 
               , totalPlayed          = "Łącznie gier" 
               , blackWins            = "Wygranych czarnego" 
               , whiteWins            = "Wygranych białego"
               , whiteWinningPerc     = "Procent wygr. białego"
               , blackWinningPerc     = "Procent wygr. czarnego"
               , gameOver             = "Koniec"
               , noResult             = "Nierozstrzygnięte"
               , result               = "Wynik"
               , date                 = "Data"
               , downloadSgf          = "Pobierz zapis gry (.sgf)"
               }
      
allLanguages :: [Language]
allLanguages = ["pl", "eng"]

capitalize :: String -> String
capitalize []     = []
capitalize (c:cs) = toUpper c : cs