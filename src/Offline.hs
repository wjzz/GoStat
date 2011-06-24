{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Offline where

import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

import Lang
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
  buildMoveBrowser level 0 ""
  waitForChildren

saveMainPage :: IO ()
saveMainPage = do
  let html = mainPage offLineConfig
  writeFile (path ++ "index.htm") (renderHtml html)
  
buildMoveBrowser :: Level -> Level -> String -> IO ()
buildMoveBrowser 0 _ _ = return ()
buildMoveBrowser n currentLevel movesSoFar = do
  moves <- queryStatsDB movesSoFar
  count <- queryCountDB
  let html = moveBrowser count moves movesSoFar offLineConfig
  let langN = "en"
  writeFile (path ++ "moves/" ++ (mbUrl langN movesSoFar)) (renderHtml html) 
  let options = map (\(m,_,_,_) -> m) moves
  mapM_ (\move -> aux $ buildMoveBrowser (n-1) (currentLevel+1) (movesSoFar ++ move)) options where
    aux m
      | currentLevel < 1 = forkChild m >> return ()
      | otherwise        = m

offLineConfig :: UrlBuilders
offLineConfig = UrlBuilders { mainPageUrl        = "../index.htm"
                              , moveBrowserMainUrl = "moves/move.htm"
                              , moveBrowserMakeUrl = mbUrl
                              , cssUrl             = "../style.css"
                              , jsUrls             = ["../jquery.js", "../highlight.hs"]
                              , imagesMakeUrl      = imageUrl
                              , language           = eng
                              }

mbUrl :: Language -> String -> String
mbUrl langN movesSoFar = error "This needs to updated!" -- "move" ++ movesSoFar ++ ".htm"

imageUrl :: String -> String
imageUrl image = "../img/" ++ image

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])
    
waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkIO (io `finally` putMVar mvar ())

-- main =
--   later waitForChildren $