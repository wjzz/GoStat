{-
  @author: Wojciech Jedynak (wjedynak@gmail.com)
-}
module Offline where

import Pages

type Level = Int

buildOffline :: Level -> IO ()
buildOffline _ = putStrLn "Building offline version..."

-- remember to copy the .css file