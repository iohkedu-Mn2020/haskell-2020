{-# LANGUAGE ScopedTypeVariables #-}

module Proxies where

import Data.Proxy (Proxy (..))
import Text.Read  (readMaybe)

-- data Proxy a = Proxy

weird :: forall a. (Read a, Show a) => Proxy a -> String -> String
weird Proxy s = case readMaybe s of
    Nothing -> "NO PARSE"
    Just x  -> show (x :: a)
