{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Shared.Config where

import qualified Shared.Fields as F
import Data.Extensible
import Data.Extensible.Effect

import Infra.Sqlite.UserRepoImpl



runIO :: Eff '["io" >: IO] a -> IO a
runIO = retractEff


runUserRepo :: Lookup xs "io" IO =>
                Eff ("userRepo" >: UserRepoSqliteImpl ': xs) a ->
                Eff xs a
runUserRepo = runUserRepoSqliteImpl
