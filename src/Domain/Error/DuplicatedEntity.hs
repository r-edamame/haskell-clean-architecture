{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies#-}

module Domain.Error.DuplicatedEntity where

import Data.Extensible.Effect
import Domain.Error
import Data.Proxy (Proxy(..))



-- DEFINITION

data DuplicatedEntityError = DuplicatedEntityError String

instance Error DuplicatedEntityError where
    type ELabel DuplicatedEntityError = "duplicatedEntityError"
    errorMessage (DuplicatedEntityError mes) = mes

instance Show DuplicatedEntityError where
    show = defaultShowError Proxy

instance ThrowDefault DuplicatedEntityError xs => Throws DuplicatedEntityError (Eff xs) where
    throw = defaultThrow Proxy
