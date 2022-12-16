{-# LANGUAGE OverloadedStrings #-}

module Staff
  ( getStaff,
    createStaff,
    updateStaff,
    deleteStaff,
    staffChoices,
  )
where

import Database
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Menu

data Staff = Staff {id :: Int, salary :: Int, person_id :: Int, position_id :: Int}
  deriving (Show)

instance FromRow Staff where
  fromRow = Staff <$> field <*> field <*> field <*> field

getStaffs :: Connection -> IO [Staff]
getStaffs conn = query conn "SELECT * FROM staff" ()

getStaff :: Connection -> Int -> IO [Staff]
getStaff conn cid = query conn "SELECT * FROM staff WHERE id = ?" (Only cid)

createStaff :: Connection -> Int -> Int -> Int -> IO [Staff]
createStaff conn salary person_id position_id = do
  query conn "INSERT INTO staff (salary, person_id, position_id) VALUES (?, ?, ?) RETURNING *" (salary, person_id, position_id)

updateStaff :: Connection -> Int -> Int -> Int -> Int -> IO [Staff]
updateStaff conn cid salary person_id position_id = do
  query conn "UPDATE staff SET salary = ?, person_id = ?, position_id = ? WHERE id = ? RETURNING *" (salary, person_id, position_id, cid)

deleteStaff :: Connection -> Int -> IO Bool
deleteStaff conn cid = do
  n <- execute conn "DELETE FROM staff WHERE id = ?" (Only cid)
  return $ n > 0

staffChoices :: Choices
staffChoices =
  zip
    [1 ..]
    [ ("Print staffs", handleGetStaffs),
      ("Create staff", handleCreateStaff),
      ("Update staff", handleUpdateStaff),
      ("Delete staff", handleDeleteStaff),
      ("Go back", return ())
    ]

handleGetStaffs :: IO ()
handleGetStaffs =
  do
    putStrLn "Staff List"
    conn <- connect localPG
    staffs <- getStaffs conn
    mapM_ print staffs
    pause
    return ()

handleCreateStaff :: IO ()
handleCreateStaff = do
  putStrLn "New Staff"
  putStr "Salary: "
  salary <- getLine
  putStr "Person id: "
  personId <- getLine
  putStr "Position id: "
  positionId <- getLine

  let parsedSalary = read salary :: Int
  let parsedPersonId = read personId :: Int
  let parsedPositionId = read positionId :: Int

  conn <- connect localPG
  staff <- createStaff conn parsedSalary parsedPersonId parsedPositionId
  putStrLn $ show staff ++ " created!"
  pause
  return ()

handleUpdateStaff :: IO ()
handleUpdateStaff = do
  putStrLn "Update Staff"

  putStr "ID: "
  id <- getLine
  putStr "Salary: "
  salary <- getLine
  putStr "Person id: "
  personId <- getLine
  putStr "Position id: "
  positionId <- getLine

  let parsedId = read id :: Int
  let parsedSalary = read salary :: Int
  let parsedPersonId = read personId :: Int
  let parsedPositionId = read positionId :: Int

  conn <- connect localPG
  staff <- updateStaff conn parsedId parsedSalary parsedPersonId parsedPositionId
  putStrLn $ show staff ++ " updated!"
  pause
  return ()

handleDeleteStaff :: IO ()
handleDeleteStaff = do
  putStrLn "Delete Staff"

  putStr "ID: "
  id <- getLine
  let parsedId = read id :: Int

  conn <- connect localPG
  staff <- deleteStaff conn parsedId
  putStrLn "Deleted!"
  pause
  return ()
