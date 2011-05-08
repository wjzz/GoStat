{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Offline where

import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

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
  count <- queryCountDB
  let html = mainPage offLineConfig count
  writeFile (path ++ "index.htm") (renderHtml html)
  
buildMoveBrowser :: Level -> Level -> String -> IO ()
buildMoveBrowser 0 _ _ = return ()
buildMoveBrowser n currentLevel movesSoFar = do
  moves <- queryStatsDB movesSoFar
  let html = moveBrowser moves movesSoFar offLineConfig
  writeFile (path ++ "moves/" ++ (mbUrl movesSoFar)) (renderHtml html) 
  let options = map (\(m,_,_,_) -> m) moves
  mapM_ (\move -> aux $ buildMoveBrowser (n-1) (currentLevel+1) (movesSoFar ++ move)) options where
    aux m
      | currentLevel < 1 = forkChild m >> return ()
      | otherwise        = m

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