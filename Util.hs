module Util where

mergeMaybe (Just a)    _        = Just a
mergeMaybe (Nothing)  (Just b)  = Just b
mergeMaybe (Nothing)  (Nothing) = Nothing
