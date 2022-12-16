{-# LANGUAGE OverloadedStrings #-}

module Position
  ( getPositions,
    getPosition,
    createPosition,
    updatePosition,
    deletePosition,
    positionChoices,
  )
where

import Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Menu

data Position = Position {id :: Int, name :: String, access_level :: Int}
  deriving (Show)

instance FromRow Position where
  fromRow = Position <$> field <*> field <*> field

getPositions :: Connection -> IO [Position]
getPositions conn = query conn "SELECT * FROM position" ()

getPosition :: Connection -> Int -> IO [Position]
getPosition conn cid = query conn "SELECT * FROM position WHERE id = ?" (Only cid)

createPosition :: Connection -> String -> Int -> IO [Position]
createPosition conn name access_level = do
  query conn "INSERT INTO position (name, access_level) VALUES (?, ?) RETURNING *" (name, access_level)

updatePosition :: Connection -> Int -> String -> IO [Position]
updatePosition conn cid name = do
  query conn "UPDATE position SET name = ? WHERE id = ? RETURNING *" (name, cid)

deletePosition :: Connection -> Int -> IO Bool
deletePosition conn cid = do
  n <- execute conn "DELETE FROM position WHERE id = ?" (Only cid)
  return $ n > 0

positionChoices :: Choices
positionChoices =
  zip
    [1 ..]
    [ ("Print positions", handleGetPositions),
      ("Create position", handleCreatePositions),
      ("Update position", handleUpdatePositions),
      ("Delete position", handleDeletePositions),
      ("Go back", return ())
    ]

handleGetPositions :: IO ()
handleGetPositions =
  do
    putStrLn "Positions List"
    conn <- connect localPG
    positions <- getPositions conn
    mapM_ print positions
    pause
    return ()

handleCreatePositions :: IO ()
handleCreatePositions = do
  putStrLn "New Positions"
  putStr "Name: "
  name <- getLine
  putStr "Access level: "
  accessLevel <- getLine

  let parsedAccessLevel = read accessLevel :: Int

  conn <- connect localPG
  position <- createPosition conn name parsedAccessLevel
  putStrLn $ show position ++ " created!"
  pause
  return ()

handleUpdatePositions :: IO ()
handleUpdatePositions = do
  putStrLn "Update Positions"

  putStr "ID: "
  id <- getLine
  putStr "Name: "
  name <- getLine

  let parsedId = read id :: Int

  conn <- connect localPG
  position <- updatePosition conn parsedId name
  putStrLn $ show position ++ " updated!"
  pause
  return ()

handleDeletePositions :: IO ()
handleDeletePositions = do
  putStrLn "Delete Positions"

  putStr "ID: "
  id <- getLine
  let parsedId = read id :: Int

  conn <- connect localPG
  position <- deletePosition conn parsedId
  putStrLn "Deleted!"
  pause
  return ()
