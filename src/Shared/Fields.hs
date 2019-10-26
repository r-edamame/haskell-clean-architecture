{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE #-}


module Shared.Fields where

import Data.Extensible

mkField "id name userId owner title due status taskId"

mkField "year month day hour min sec"
