{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Domain.Error where


import GHC.TypeLits (Symbol, symbolVal)
import Data.Extensible
import Data.Char (toUpper)



-- ERROR INTERFACE DEFINITION

class Error e where
    type ELabel e :: Symbol
    errorMessage :: e -> String

class (Error e, Monad m) => Throws e m where
    throw :: e -> m a


type Catch e = forall xs a. (e -> Eff xs a) -> Eff (((ELabel e) :> EitherEff e) ': xs) a -> Eff xs a

catch :: Error e => Catch e
catch handler p = either handler return =<< runEitherEff p



-- UTILITY FUNCTIONS

defaultShowError :: (Error e, KnownSymbol (ELabel e)) => Proxy (ELabel e) -> e -> String
defaultShowError label err = toUpperHead (symbolVal label) ++ ": " ++ errorMessage err where
    toUpperHead (x:xs) = toUpper x : xs


type ThrowDefault e xs = (Lookup xs (ELabel e) (EitherEff e), KnownSymbol (ELabel e))

defaultThrow :: (Error e ,ThrowDefault e xs) => Proxy (ELabel e) -> e -> Eff xs a
defaultThrow label = throwEff label