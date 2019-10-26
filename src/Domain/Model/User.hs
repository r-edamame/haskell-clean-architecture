{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts, OverloadedLabels, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Domain.Model.User where

import qualified Shared.Fields as F
import Prelude hiding (id)
import qualified Prelude as Pl
import Control.Lens ((^.), (&), (.~))
import Data.Extensible
import Domain.IdFactory (IdFactory(..))



-- DEFINITIONS

newtype UserId = UserId { getUserId :: String } deriving (Eq)

instance Show UserId where
    show (UserId uid) = uid

type User = Record
   '[ "id" :> UserId
    , "name" :> String
    ]



-- REPOSITORY

class Monad m => UserRepo m where
    findUser :: UserId -> m (Maybe User)
    findUserByName :: String -> m (Maybe User)
    saveUser :: User -> m ()



-- CONSTRUCTOR

constructUser :: UserId -> String -> User
constructUser _id name
    = F.id @= _id
    <: F.name @= name
    <: nil



-- FACTORY

newUser :: IdFactory m => String -> m User
newUser name = do
    _id <- UserId <$> createUUID
    return $ constructUser _id name
