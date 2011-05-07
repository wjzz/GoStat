{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Offline where

import Pages
import DB
import Text.XHtml (renderHtml)

path :: String
path = "/home/wjzz/GoStatOffline/"

type Level = Int

buildOffline :: Level -> IO ()
buildOffline level = do 
  putStrLn "Building offline version..."
  saveMainPage
  buildMoveBrowser level ""

saveMainPage :: IO ()
saveMainPage = do
  count <- queryCountDB
  let html = mainPage offLineConfig count
  writeFile (path ++ "index.htm") (renderHtml html)
  
buildMoveBrowser :: Level -> String -> IO ()
buildMoveBrowser 0 _ = return ()
buildMoveBrowser n movesSoFar = do
  moves <- queryStatsDB movesSoFar
  let html = moveBrowser moves movesSoFar offLineConfig
  writeFile (path ++ "moves/" ++ (mbUrl movesSoFar)) (renderHtml html) 
  let options = map (\(m,_,_,_) -> m) moves
  mapM_ (\move -> buildMoveBrowser (n-1) (movesSoFar ++ move)) options

offLineConfig :: Configuration
offLineConfig = Configuration { mainPageUrl        = "../index.htm"
                              , moveBrowserMainUrl = "moves/move.htm"
                              , moveBrowserMakeUrl = mbUrl
                              , cssUrl             = "../style.css"
                              , imagesMakeUrl      = imageUrl
                              }

mbUrl :: String -> String
mbUrl movesSoFar = "move" ++ movesSoFar ++ ".htm"

imageUrl :: String -> String
imageUrl image = "../img/" ++ image