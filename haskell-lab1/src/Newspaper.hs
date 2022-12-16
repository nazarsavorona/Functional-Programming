{-# LANGUAGE OverloadedStrings #-}

module Newspaper
  ( getNewspaper,
    createNewspaper,
    updateNewspaper,
    deleteNewspaper,
    newspaperChoices,
  )
where

import Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Menu

data Newspaper = Newspaper {id :: Int, about :: String, staff_id :: Int}
  deriving (Show)

instance FromRow Newspaper where
  fromRow = Newspaper <$> field <*> field <*> field

getNewspapers :: Connection -> IO [Newspaper]
getNewspapers conn = query conn "SELECT * FROM newspaper" ()

getNewspaper :: Connection -> Int -> IO [Newspaper]
getNewspaper conn cid = query conn "SELECT * FROM newspaper WHERE id = ?" (Only cid)

createNewspaper :: Connection -> String -> Int -> IO [Newspaper]
createNewspaper conn about staff_id = do
  query conn "INSERT INTO newspaper (about, staff_id) VALUES (?, ?) RETURNING *" (about, staff_id)

updateNewspaper :: Connection -> Int -> String -> Int -> IO [Newspaper]
updateNewspaper conn cid about staff_id = do
  query conn "UPDATE newspaper SET about = ?, staff_id = ? WHERE id = ? RETURNING *" (about, staff_id, cid)

deleteNewspaper :: Connection -> Int -> IO Bool
deleteNewspaper conn cid = do
  n <- execute conn "DELETE FROM newspaper WHERE id = ?" (Only cid)
  return $ n > 0

newspaperChoices :: Choices
newspaperChoices =
  zip
    [1 ..]
    [ ("Print newspapers", handleGetNewspapers),
      ("Create newspaper", handleCreateNewspaper),
      ("Update newspaper", handleUpdateNewspaper),
      ("Delete newspaper", handleDeleteNewspaper),
      ("Go back", return ())
    ]

handleGetNewspapers :: IO ()
handleGetNewspapers =
  do
    putStrLn "Newspaper List"
    conn <- connect localPG
    newspapers <- getNewspapers conn
    mapM_ print newspapers
    pause
    return ()

handleCreateNewspaper :: IO ()
handleCreateNewspaper = do
  putStrLn "New Newspaper"
  putStr "About: "
  about <- getLine
  putStr "Staff id: "
  staffId <- getLine

  let parsedStaffId = read staffId :: Int

  conn <- connect localPG
  newspaper <- createNewspaper conn about parsedStaffId
  putStrLn $ show newspaper ++ " created!"
  pause
  return ()

handleUpdateNewspaper :: IO ()
handleUpdateNewspaper = do
  putStrLn "Update Newspaper"

  putStr "ID: "
  id <- getLine
  putStr "About: "
  about <- getLine
  putStr "Staff id: "
  staffId <- getLine

  let parsedId = read id :: Int
  let parsedStaffId = read staffId :: Int

  conn <- connect localPG
  newspaper <- updateNewspaper conn parsedId about parsedStaffId
  putStrLn $ show newspaper ++ " updated!"
  pause
  return ()

handleDeleteNewspaper :: IO ()
handleDeleteNewspaper = do
  putStrLn "Delete Newspaper"

  putStr "ID: "
  id <- getLine
  let parsedId = read id :: Int

  conn <- connect localPG
  newspaper <- deleteNewspaper conn parsedId
  putStrLn "Deleted!"
  pause
  return ()
