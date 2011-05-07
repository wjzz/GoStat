{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Offline where

import Pages
import DB
import Text.XHtml (renderHtml)

type Level = Int

buildOffline :: Level -> IO ()
buildOffline _ = do 
  putStrLn "Building offline version..."
  saveMainPage

-- remember to copy the .css file

path :: String
path = "/home/wjzz/GoStatOffline"

saveMainPage :: IO ()
saveMainPage = do
  count <- queryCountDB
  let html = mainPage offLineConfig count
  writeFile (path ++ "/index.htm") (renderHtml html)
  
offLineConfig :: Configuration
offLineConfig = Configuration { mainPageUrl        = "/index.htm"
                              , moveBrowserMainUrl = "/moves/move.htm"
                              , moveBrowserMakeUrl = id
                              , cssUrl             = "/style.css"
                              , imagesMakeUrl      = id
                              }
