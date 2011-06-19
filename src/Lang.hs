{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Lang where

type Language = String

data Message = Message { langName :: Language
                       , title :: String
                       , gamesInDb :: Int -> String
                       , goToMovesBrowser :: String
                       , black :: String
                       , white :: String
                       , backToMain :: String
                       , numberOfMoves :: String
                       , playerToMove :: String
                       , takeBackMove :: String
                       , resetMoves :: String
                       , availableMoves :: String
                       , movesByTotalCount :: String
                       , movesByWinPercentage :: String
                       , move :: String
                       , totalPlayed :: String
                       , blackWins :: String
                       , whiteWins :: String
                       , whiteWinningPerc :: String
                       , blackWinningPerc :: String
                       }

eng :: Message
eng = Message { langName             = "eng"
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
              }

pl :: Message
pl =  Message { langName             = "pl"
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
              }

allLanguages :: [Language]
allLanguages = ["pl", "eng"]