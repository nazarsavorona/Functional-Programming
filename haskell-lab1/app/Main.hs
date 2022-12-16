module Main (main) where

import Address
import Menu
import Newspaper
import Person
import Position
import Staff
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  menu "Main Menu" mainChoices

mainChoices :: Choices
mainChoices =
  zip
    [1 ..]
    [ ("Address", menu "Address Menu" addressChoices),
      ("Newspaper", menu "Newspaper Menu" newspaperChoices),
      ("Person", menu "Person Menu" personChoices),
      ("Position", menu "Position Menu" positionChoices),
      ("Staff", menu "Staff Menu" staffChoices),
      ("Quit", return ())
    ]
