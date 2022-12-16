{-# LANGUAGE OverloadedStrings #-}

module Person
  ( getPerson,
    createPerson,
    updatePerson,
    deletePerson,
    personChoices,
  )
where

import Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Menu

data Person = Person {id :: Int, name :: String, phone :: String, address_id :: Int}
  deriving (Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field

getPersons :: Connection -> IO [Person]
getPersons conn = query conn "SELECT * FROM person" ()

getPerson :: Connection -> Int -> IO [Person]
getPerson conn cid = query conn "SELECT * FROM person WHERE id = ?" (Only cid)

createPerson :: Connection -> String -> String -> Int -> IO [Person]
createPerson conn name phone address_id = do
  query conn "INSERT INTO person (name, phone, address_id) VALUES (?, ?, ?) RETURNING *" (name, phone, address_id)

updatePerson :: Connection -> Int -> String -> String -> Int -> IO [Person]
updatePerson conn cid name phone address_id = do
  query conn "UPDATE person SET name = ?, phone = ?, address_id = ? WHERE id = ? RETURNING *" (name, phone, address_id, cid)

deletePerson :: Connection -> Int -> IO Bool
deletePerson conn cid = do
  n <- execute conn "DELETE FROM person WHERE id = ?" (Only cid)
  return $ n > 0

personChoices :: Choices
personChoices =
  zip
    [1 ..]
    [ ("Print persons", handleGetPersons),
      ("Create person", handleCreatePerson),
      ("Update person", handleUpdatePerson),
      ("Delete person", handleDeletePerson),
      ("Go back", return ())
    ]

handleGetPersons :: IO ()
handleGetPersons =
  do
    putStrLn "Person List"
    conn <- connect localPG
    persons <- getPersons conn
    mapM_ print persons
    pause
    return ()

handleCreatePerson :: IO ()
handleCreatePerson = do
  putStrLn "New Person"
  putStr "Name: "
  name <- getLine
  putStr "Phone: "
  phone <- getLine
  putStr "Addresss id: "
  addressId <- getLine

  let parsedAddressId = read addressId :: Int

  conn <- connect localPG
  person <- createPerson conn name phone parsedAddressId
  putStrLn $ show person ++ " created!"
  pause
  return ()

handleUpdatePerson :: IO ()
handleUpdatePerson = do
  putStrLn "Update Person"

  putStr "ID: "
  id <- getLine
  putStr "Name: "
  name <- getLine
  putStr "Phone: "
  phone <- getLine
  putStr "Addresss id: "
  addressId <- getLine

  let parsedId = read id :: Int
  let parsedAddressId = read addressId :: Int

  conn <- connect localPG
  person <- updatePerson conn parsedId name phone parsedAddressId
  putStrLn $ show person ++ " updated!"
  pause
  return ()

handleDeletePerson :: IO ()
handleDeletePerson = do
  putStrLn "Delete Person"

  putStr "ID: "
  id <- getLine
  let parsedId = read id :: Int

  conn <- connect localPG
  person <- deletePerson conn parsedId
  putStrLn "Deleted!"
  pause
  return ()
