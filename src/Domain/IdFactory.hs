{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Domain.IdFactory where

import qualified Data.UUID as U
import qualified Data.UUID.V4 as Uv4

import Data.Extensible
import Data.Extensible.Effect



-- FACTORY DEFINITION

class Monad m => IdFactory m where
    createUUID :: m String



-- DEFAULT IMPLEMENTATION

instance Lookup xs "io" IO => IdFactory (Eff xs) where
    createUUID = liftEff #io $ U.toString <$> Uv4.nextRandom
