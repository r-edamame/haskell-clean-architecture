{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Console.App where

import Data.Extensible.Effect

import UseCase.User.Create (userCreateInput)

import Shared.Config

import Domain.Error (Catch, catch)
import Domain.Error.DuplicatedEntity
import Domain.Application.User.Create (createUser)

import Console.Presenter.User.Create

main :: IO ()
main = run $ do
    let input = userCreateInput "edamame"
    createUser input
    return ()

    where
        run = runIO .
            (catch :: Catch DuplicatedEntityError) (liftEff #io . print) .
            runUserRepo