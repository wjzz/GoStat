{-# OPTIONS -Wall #-}

{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Main where

import Configuration
import Control.Monad.Trans
import Server

main :: IO ()
main = do
  configM <- readConfig =<< configurationPath
  config  <- 
    case configM of
      Left err -> do
        putStrLn err
        return defaultConfig
      Right c -> return c
  
  runGoStatM config server

