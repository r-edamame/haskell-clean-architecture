{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module UseCase.User.Create where

import Data.Extensible
import qualified Shared.Fields as F

import Domain.Model.User
import Domain.Model.Date


-- INTERFACE DEFINITION

type UserCreateInput = Record
   '[ "name" >: String
    ]

userCreateInput _name = F.name @= _name <: nil


type UserCreateOutput = Record
   '[ "userId" >: UserId
    ]

userCreateOutput _userId = F.userId @= _userId <: nil



-- USECASE

type UserCreate m = UserCreateInput -> m UserCreateOutput



-- PRESENTER

class Monad m => UserCreatePresenter m where
    completeUserCreate :: UserCreateOutput -> m ()