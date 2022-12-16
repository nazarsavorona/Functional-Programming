{-# LANGUAGE OverloadedStrings #-}

module Address
  ( getAddress,
    createAddress,
    updateAddress,
    deleteAddress,
    addressChoices,
    getAddresses,
  )
where

import Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Menu

data Address = Address
  { id :: Int,
    street :: String,
    city :: String
  }
  deriving (Show)

instance FromRow Address where
  fromRow = Address <$> field <*> field <*> field

getAddresses :: Connection -> IO [Address]
getAddresses conn = query conn "SELECT * FROM address" ()

getAddress :: Connection -> Int -> IO [Address]
getAddress conn cid = query conn "SELECT * FROM address WHERE id = ?" (Only cid)

createAddress :: Connection -> String -> String -> IO [Address]
createAddress conn street city = do
  query conn "INSERT INTO address (street, city) VALUES (?, ?) RETURNING *" (street, city)

updateAddress :: Connection -> Int -> String -> String -> IO [Address]
updateAddress conn cid street city = do
  query conn "UPDATE address SET street = ?, city = ? WHERE id = ? RETURNING *" (street, city, cid)

deleteAddress :: Connection -> Int -> IO Bool
deleteAddress conn cid = do
  n <- execute conn "DELETE FROM address WHERE id = ?" (Only cid)
  return $ n > 0

addressChoices :: Choices
addressChoices =
  zip
    [1 ..]
    [ ("Print addresses", handleGetAddresses),
      ("Create address", handleCreateAddress),
      ("Update address", handleUpdateAddress),
      ("Delete address", handleDeleteAddress),
      ("Go back", return ())
    ]

handleGetAddresses :: IO ()
handleGetAddresses =
  do
    putStrLn "Address List"
    conn <- connect localPG
    addresses <- getAddresses conn
    mapM_ print addresses
    pause
    return ()

handleCreateAddress :: IO ()
handleCreateAddress = do
  putStrLn "New Address"
  putStr "City: "
  city <- getLine
  putStr "Street: "
  street <- getLine

  conn <- connect localPG
  address <- createAddress conn street city
  putStrLn $ show address ++ " created!"
  pause
  return ()

handleUpdateAddress :: IO ()
handleUpdateAddress = do
  putStrLn "Update Address"

  putStr "ID: "
  id <- getLine
  putStr "City: "
  city <- getLine
  putStr "Street: "
  street <- getLine

  let parsedId = read id :: Int

  conn <- connect localPG
  address <- updateAddress conn parsedId street city
  putStrLn $ show address ++ " updated!"
  pause
  return ()

handleDeleteAddress :: IO ()
handleDeleteAddress = do
  putStrLn "Delete Address"

  putStr "ID: "
  id <- getLine
  let parsedId = read id :: Int

  conn <- connect localPG
  address <- deleteAddress conn parsedId
  putStrLn "Deleted!"
  pause
  return ()
