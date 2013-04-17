-- Module      : Main
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Command-line wrapper around Wwdc module.
--

module Main where

import Control.Applicative
import Data.Maybe
import Control.Monad (when)
import System.Environment (getProgName)
import System.Posix.Daemonize
import Options.Applicative
import Wwdcc
import Logging
import qualified Config as C

wwdcUrl = "https://developer.apple.com/wwdc/"
description = "Send email to SRC_EMAIL from DEST_EMAIL when WWDC site changes or stops responding."

data Options = Options { verbose :: !Bool
                       , syslog :: !Bool
                       , daemon :: !Bool
                       , url :: !String
                       , unmodifiedDelay :: !Int
                       , modifiedDelay :: !Int
                       , notRespondingDelay :: !Int
                       , srcEmail :: !String
                       , dstEmail :: !String }
  
parser :: Parser Options
parser = Options
         <$> switch (long "verbose"
                     <> short 'v'
                     <> help "Verbose logging")
         <*> switch (long "syslog"
                     <> short 's'
                     <> help "Log to syslog (default is stderr)")
         <*> switch (long "daemon"
                     <> help "Run as a daemon (implies --syslog)")
         <*> strOption (long "url"
                        <> short 'u'
                        <> metavar "URL"
                        <> value wwdcUrl
                        <> help "Override WWDC URL")
         <*> option (long "unmodified-delay"
                     <> metavar "DELAY"
                     <> value 30
                     <> help "Delay (in seconds) between actions when site is unmodified")
         <*> option (long "modified-delay"
                     <> metavar "DELAY"
                     <> value 60
                     <> help "Delay (in seconds) beteen actions when site has been modified")
         <*> option (long "not-responding-delay"
                     <> metavar "DELAY"
                     <> value 60
                     <> help "Delay (in seconds) between actions when site is note responding")
         <*> argument str ( metavar "SRC_EMAIL" )
         <*> argument str ( metavar "DEST_EMAIL" )

main :: IO ()
main = do
  cmdLineOptions <- execParser opts
  when (verbose cmdLineOptions) verboseLogging
  when ((syslog cmdLineOptions) || (daemon cmdLineOptions)) $ getProgName >>= logToSyslog
  if (daemon cmdLineOptions)
    then daemonize $ startChecks $ buildConfig cmdLineOptions
    else startChecks $ buildConfig cmdLineOptions
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc description
     <> header "wwdcc - a WWDC checker" )

buildConfig :: Options -> C.Config
buildConfig options = C.Config { C.daemon = daemon options
                               , C.url = url options
                               , C.unmodifiedDelay = unmodifiedDelay options
                               , C.modifiedDelay = modifiedDelay options
                               , C.notRespondingDelay = notRespondingDelay options
                               , C.srcEmail = srcEmail options
                               , C.dstEmail = dstEmail options }
