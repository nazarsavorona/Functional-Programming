module Database
  ( localPG,
  )
where

import Database.PostgreSQL.Simple

localPG =
  defaultConnectInfo
    { connectHost = "localhost",
      connectDatabase = "postgres",
      connectUser = "postgres",
      connectPassword = "postgres",
      connectPort = 5431
    }