module Util where

import Data.Text

trim  = unpack . strip . pack

mergeMaybe (Just a)    _        = Just a
mergeMaybe (Nothing)  (Just b)  = Just b
mergeMaybe (Nothing)  (Nothing) = Nothing
