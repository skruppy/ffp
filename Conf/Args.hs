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
      $ metavar "GAMEID" -- "mSPb8GUCKxc"
     <> help    "Game ID"
      )
    <*> (
        optional $ argument auto
      $ metavar "PLAYER"
     <> help    "Player number (0, 1, ..)"
      )

getCfg = do
    cfg <- execParser $ info
        (helper <*> argParseCfg)
        (fullDesc <> header "AI for the sysprak gameserver.")
    return cfg