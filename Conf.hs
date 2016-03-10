-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Conf (IntermediateCfg(..),defaultCfg,emptyCfg,mergeCfg) where

import Util

data IntermediateCfg = IntermediateCfg
    { host   :: Maybe String
    , port   :: Maybe String
    , conf   :: Maybe String
    , gameId :: Maybe String
    , player :: Maybe Int
    }


-- data Cfg = Cfg
--     { host   :: String
--     , port   :: String
--     , conf   :: Maybe String
--     , gameId :: String
--     , player :: Maybe Int
--     }


defaultCfg = IntermediateCfg
    { host   = Just "sysprak.onmars.eu"
    , port   = Just "1357"
    , conf   = Just "client.cfg"
    , gameId = Nothing
    , player = Nothing
    }


-- This definition is so boring, we had to add a joke:
-- Chuck Norris can write Haskell... in assembler.
emptyCfg = IntermediateCfg
    { host   = Nothing
    , port   = Nothing
    , conf   = Nothing
    , gameId = Nothing
    , player = Nothing
    }


mergeCfg cfgs = foldr merger emptyCfg cfgs
    where
        merger a b = IntermediateCfg
            { host   = mergeMaybe (host a)   (host b)
            , port   = mergeMaybe (port a)   (port b)
            , conf   = mergeMaybe (conf a)   (conf b)
            , gameId = mergeMaybe (gameId a) (gameId b)
            , player = mergeMaybe (player a) (player b)
            }
