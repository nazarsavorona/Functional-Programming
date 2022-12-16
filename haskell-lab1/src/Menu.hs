module Menu
  ( menu,
    pause,
    Choices
  )
where

import System.Exit (ExitCode (ExitFailure))
import System.Process

type Choices = [(Int, (String, IO ()))]

menu :: String -> Choices -> IO ()
menu title choices = do
  _ <- clear
  putStrLn $ "- " ++ title  ++ " -"
  putStrLn . unlines $ map concatNums choices
  choice <- getLine

  let x = read choice :: Int
  let len = listLength choices

  case validate choice choices of
    Just n -> do
      if x == len
        then do
          _ <- clear
          return ()
        else do
          execute choices x
          menu title choices
    Nothing -> do
      putStrLn "Please try again"
      menu title choices
  where
    concatNums (i, (s, _)) = show i ++ ". " ++ s

validate :: String -> Choices -> Maybe Int
validate s choices = isValid (reads s)
  where
    isValid [] = Nothing
    isValid ((n, _) : _)
      | outOfBounds n = Nothing
      | otherwise = Just n
    outOfBounds n = (n < 1) || (n > length choices)

execute :: Choices -> Int -> IO ()
execute choices n = do
  _ <- clear
  doExec $ filter (\(i, _) -> i == n) choices
  where
    doExec ((_, (_, f)) : _) = f

listLength :: [a] -> Int
listLength = listLength' 0
  where
    listLength' a [] = a
    listLength' a (_ : xs) = listLength' (a + 1) xs

clear :: IO ExitCode
clear = system "cls"

pause :: IO ExitCode
pause = system "pause"
