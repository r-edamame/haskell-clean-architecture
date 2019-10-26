
module Shared.Lang where

import Data.Maybe (isJust)

exist :: Maybe a -> Bool
exist = isJust
