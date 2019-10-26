{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Infra.Sqlite.UserRepoImpl where

import Prelude hiding (id)

import qualified Shared.Fields as F
import Control.Lens ((^.))

import DataSource (defineTable, connect)
import Database.HDBC (commit)
import qualified Database.HDBC.Sqlite3 as Sqlite3

import Domain.Model.User (UserRepo(..), UserId(..), constructUser)

import Data.Extensible
import Data.Extensible.Effect

import Database.HDBC.Record.Query (runQuery)
import Database.HDBC.Record.Insert (runInsert)
import Database.Relational ((!) ,(.=.), (<-#), query, placeholder, wheres, relation', value, insertValueNoPH)
import Database.Relational.Type (relationalQuery)


-- READ TABLE SCHEMA

$(defineTable "users" [''Show])



-- SQL QUERIES

findUserQuery = relationalQuery $ relation' $ do
    u <- query users
    (ph,_) <- placeholder $ \ph' -> wheres $ u ! id' .=. ph'
    return (ph, u)


findUserByNameQuery = relationalQuery $ relation' $ do
    u <- query users
    (ph,_) <- placeholder $ \ph' -> wheres $ u ! name' .=. ph'
    return (ph, u)


saveUserQuery uid name = insertValueNoPH $ do
    id' <-# value uid
    name' <-# value name



-- REPOSITORY IMPLEMENTATION

type UserRepoSqliteImpl = ReaderEff Sqlite3.Connection


instance (Lookup xs "userRepo" UserRepoSqliteImpl, Lookup xs "io" IO)
        => UserRepo (Eff xs) where
    
    findUser (UserId uid) = do
        conn <- askEff #userRepo
        usr <- liftEff #io $ runQuery conn findUserQuery uid
        if null usr then return Nothing
        else return $ Just $ constructUser (UserId . id $ head usr) (name $ head usr)
    
    findUserByName name_ = do
        conn <- askEff #userRepo
        usr <- liftEff #io $ runQuery conn findUserByNameQuery name_
        if null usr then return Nothing
        else return $ Just $ constructUser (UserId . id $ head usr) (name $ head usr)

    saveUser user = do
        let (UserId uid) = user ^. F.id
        let name = user ^. F.name
        conn <- askEff #userRepo
        liftEff #io $ do
            runInsert conn (saveUserQuery uid name) ()
            commit conn
        return ()



-- RUNNER

runUserRepoSqliteImpl :: Lookup xs "io" IO
    => Eff (("userRepo" :> UserRepoSqliteImpl) ': xs) a
    -> Eff xs a
runUserRepoSqliteImpl p = do
    conn <- liftEff #io connect
    runReaderEff p conn