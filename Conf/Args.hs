-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module Conf.Args (getCfg) where

import Conf as C
import Options.Applicative


argParseCfg :: Parser IntermediateCfg
argParseCfg = IntermediateCfg <$> (
        optional $ option str
      $ long    "host"
     <> short   's'
     <> metavar "NAME"
     <> help    "Gameserver hostname"
      )
    <*> (
        optional $ option str
      $ long    "port"
     <> short   'p'
     <> metavar "NAME/NUMBER"
     <> help    "Gameserver port number or service name"
      )
    <*> (
        optional $ option str
      $ long    "conf"
     <> short   'c'
     <> metavar "PATH"
     <> help    "Read configuration from file"
      )
    <*> (
        optional $ argument str
      $ metavar "GAMEID"
     <> help    "Game ID"
      )
    <*> (
        optional $ argument auto
      $ metavar "PLAYER"
     <> help    "Player number (0, 1, ..)"
      )

getCfg :: IO (IntermediateCfg)
getCfg = do
    cfg <- execParser $ info
        (helper <*> argParseCfg)
        (fullDesc <> header "AI for the sysprak gameserver.")
    return cfg