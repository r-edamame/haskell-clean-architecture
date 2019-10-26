{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

module Console.Presenter.User.Create where

import Data.Extensible
import Data.Extensible.Effect
import Control.Lens ((^.))
import qualified Shared.Fields as F

import UseCase.User.Create (UserCreatePresenter(..))



-- IMPLEMENTATION

instance Lookup xs "io" IO => UserCreatePresenter (Eff xs) where
    completeUserCreate output = liftEff #io $ do
        let userId = output ^. F.userId
        putStrLn $ "user created: id = " ++ show userId