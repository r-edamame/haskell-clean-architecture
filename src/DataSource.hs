{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module DataSource where

import Database.HDBC.Query.TH
import Database.HDBC.Sqlite3
import Database.HDBC.Schema.SQLite3

connect = connectSqlite3 "example.db"


defineTable table derivings =
    defineTableFromDB
        connect
        driverSQLite3
        "main"
        table
        derivings
