module Main where

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.Conc (getNumCapabilities, numCapabilities, pseq)
import Lib

main = do
  let eps = 0.0001
  putStrLn "Sequential"
  start <- getCurrentTime
  print $ integral (0, 800 * 3.14159) (\x -> x * sin x) False eps
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn "number of cores: 1"

  putStrLn "Parallel"
  start <- getCurrentTime
  print $ integral (0, 800 * 3.14159) (\x -> x * sin x) True eps
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn $ "number of cores: " ++ show numCapabilities

--  print =<< getNumCapabilities
