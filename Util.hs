-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Util where

mergeMaybe (Just a)    _        = Just a
mergeMaybe (Nothing)  (Just b)  = Just b
mergeMaybe (Nothing)  (Nothing) = Nothing

maybio (Just io) = io
maybio (Nothing) = return ()
