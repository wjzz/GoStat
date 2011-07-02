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
                         , finalPosition        :: String
                         , showInContext        :: String
                         , showCurrentPosition  :: String
                         , numberOfGames        :: String
                         , chanceOfWinning      :: String
                         , noOfShownGames       :: String
                         , matchingGamesList    :: String
                         , welcome              :: String
                         , config               :: String
                         , rebuild              :: String                           
                         , startPage :: String
                         , number :: String
                         , blackRank :: String
                         , whiteRank :: String
                         , winner :: String
                         , link :: String
                         , configurationForm :: String
                         , databaseLabel :: String
                         , sqlite3Location :: String
                         , sgfDirectories :: String
                         , submitChanges :: String
                         }

eng :: Messages
eng = Messages { langName             = "eng"
               , title                = "Welcome to Go 9x9 statistics!" 
               , gamesInDb            = \n -> "We currently have <b>" ++ show n ++ "</b> games in the database." 
               , goToMovesBrowser     = "Go to move browser"
               , black                = "black" 
               , white                = "white" 
               , backToMain           = "Move browser" 
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
               , finalPosition        = "Final position"
               , showInContext        = "Show game in context"
               , showCurrentPosition  = "Show current position"
               , numberOfGames        = "Number of games with this position: "
               , chanceOfWinning      = "Winning probability of the current player: "
               , noOfShownGames       = "Number of listed games: "
               , matchingGamesList    = "List of matching games"
               , welcome = "Welcome to GoStat"
               , rebuild = "Rebuild the database"
               , config = "Configure the application (game dirs and database)"
               , startPage = "Start page"
               , number = "no"
               , blackRank = "black rank"
               , whiteRank = "white rank"
               , winner = "winner"
               , link = "link"
               , configurationForm = "Configuration form"
               , databaseLabel = "Database that you want to use:"        
               , sqlite3Location = "Location of the sqlite3 database (*.db)"
               , sgfDirectories = "Directories with SGF files you want to analyze (one path each row):"
               , submitChanges = "Submit changes"
               }

pl :: Messages
pl =  Messages { langName             = "pl"
               , title                = "Statystyki gier go na planszy 9x9"
               , gamesInDb            = \n -> "Liczba zapisów gier w bazie: <b>" ++ show n ++ "</b>." 
               , goToMovesBrowser     = "Przeglądarka ruchów"
               , black                = "czarny" 
               , white                = "biały" 
               , backToMain           = "Przeglądarka ruchów" 
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
               , finalPosition        = "Pozycja końcowa"
               , showInContext        = "Pokaż grę w kontekście"
               , showCurrentPosition  = "Pokaż bieżącą pozycję"
               , numberOfGames        = "Wystąpień bieżącej pozycji: "
               , chanceOfWinning      = "Szansa wygranej dla bieżącego gracza: "
               , noOfShownGames       = "Liczba wyświetlonych gier: "
               , matchingGamesList    = "Lista pasujących gier"
               , welcome = "Witaj w programie GoStat"
               , rebuild = "Przebudowa bazy danych"
               , config = "Konfiguracja"
               , startPage = "Strona startowa"
               , number = "nr"
               , blackRank = "rank. czarnego"
               , whiteRank = "rank. białego"
               , winner = "zwycięzca"
               , link = "odnośnik"
               , configurationForm = "Formularz konfiguracyjny"
               , databaseLabel = "Wybierz bazę danych:"
               , sqlite3Location = "(Dotyczy sqlite3) lokalizacja pliku z bazą danych (ścieżka względna do pliku *.db)"
               , sgfDirectories = "Katalogi z plikami .sgf (jeden katalog na wiersz) "
               , submitChanges = "Zapisz ustawienia"

               }
      
allLanguages :: [Language]
allLanguages = ["pl", "eng"]

capitalize :: String -> String
capitalize []     = []
capitalize (c:cs) = toUpper c : cs