{-# LANGUAGE NoImplicitPrelude #-}

module Cache
    ( module Data.Map
    , InfoCache
    ) where

import Data.Map
import Data.Text

type InfoCache = Map Text Text
