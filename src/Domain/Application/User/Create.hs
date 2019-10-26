
{-# LANGUAGE TemplateHaskell, DataKinds, TypeOperators, FlexibleContexts, OverloadedLabels, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Domain.Application.User.Create where


import Control.Monad (when)
import Control.Lens ((^.))
import Data.Extensible


import Shared.Lang
import qualified Shared.Fields as F

import Domain.IdFactory (IdFactory(..))
import Domain.Error (Throws, throw)
import Domain.Error.DuplicatedEntity
import Domain.Model.User

import UseCase.User.Create



-- INTERACTOR

createUser :: (IdFactory m, UserRepo m, Throws DuplicatedEntityError m, UserCreatePresenter m)
                => UserCreate m
createUser input = do
    let name = input ^. F.name
    duplicated <- findUserByName name
    when (exist duplicated) $ throw (DuplicatedEntityError "this name is already used")

    user <- newUser name
    saveUser user

    let output = userCreateOutput (user ^. F.id)
    completeUserCreate output
    return output
