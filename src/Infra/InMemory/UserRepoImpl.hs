{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Infra.InMemory.UserRepoImpl (UserRepoIORefImpl, runUserRepoIORefImpl) where

import Prelude hiding (id)
import qualified Shared.Fields as F
import Domain.Model.User

import Control.Lens ((^.))

import Data.IORef
import Data.List (find)

import Data.Extensible
import Data.Extensible.Effect



-- REPOSITORY IMPLEMENTATION

type UserRepoIORefImpl = ReaderEff (IORef [(UserId, User)])


instance (Lookup xs "userRepo" UserRepoIORefImpl, Lookup xs "io" IO) => UserRepo (Eff xs) where
    findUser _id = askEff #userRepo >>= \ref -> liftEff #io (lookup _id <$> readIORef ref)
    findUserByName _name = askEff #userRepo >>= \ref -> liftEff #io $ (find ((==_name).(^. F.name)) . map snd) <$> readIORef ref
    saveUser _user = askEff #userRepo >>= \ref -> liftEff #io $ modifyIORef ref ((_user ^. F.id, _user):) 



-- RUNNER

runUserRepoIORefImpl :: (Lookup xs "io" IO) => Eff (("userRepo" >: UserRepoIORefImpl) ': xs) a -> Eff xs a
runUserRepoIORefImpl p = do
    ref <- liftEff #io (newIORef ([] :: [(UserId, User)]))
    runReaderEff p ref
