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
description = "Send email to TO_EMAIL from FROM_EMAIL when WWDC site changes or stops responding."
defaultUnmodifiedDelay = 30
defaultModifiedDelay = 60
defaultNotRespondingDelay = 30

data Options = Options { verbose :: !Bool
                       , syslog :: !Bool
                       , daemon :: !Bool
                       , testMode :: !Bool
                       , url :: !String
                       , unmodifiedDelay :: !Int
                       , modifiedDelay :: !Int
                       , notRespondingDelay :: !Int
                       , fromEmail :: !String
                       , toEmail :: !String }
  
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
         <*> switch (long "test"
                     <> help "Run in test mode, no email will be sent")
         <*> strOption (long "url"
                        <> short 'u'
                        <> metavar "URL"
                        <> value wwdcUrl
                        <> (help $! "Override WWDC URL (default is " ++ wwdcUrl ++ ")"))
         <*> option (long "unmodified-delay"
                     <> metavar "DELAY"
                     <> value defaultUnmodifiedDelay
                     <> (help $! "Delay in seconds between actions when site is unmodified (default is " ++ (show defaultUnmodifiedDelay) ++ ")"))
         <*> option (long "modified-delay"
                     <> metavar "DELAY"
                     <> value defaultModifiedDelay
                     <> (help $! "Delay in seconds beteen actions when site has been modified (default is " ++ (show defaultModifiedDelay) ++ ")"))
         <*> option (long "not-responding-delay"
                     <> metavar "DELAY"
                     <> value defaultNotRespondingDelay
                     <> (help $! "Delay in seconds between actions when site doesn't respond for two cycles (default is " ++ (show defaultNotRespondingDelay) ++ ")"))
         <*> argument str ( metavar "FROM_EMAIL" )
         <*> argument str ( metavar "TO_EMAIL" )

main :: IO ()
main = do
  cmdLineOptions <- execParser opts
  when (verbose cmdLineOptions) verboseLogging
  when ((syslog cmdLineOptions) || (daemon cmdLineOptions)) $ getProgName >>= logToSyslog
  when (testMode cmdLineOptions) $ logWarning "WARNING: Running in test mode -- no email will be sent!"
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
                               , C.testMode = testMode options
                               , C.url = url options
                               , C.unmodifiedDelay = unmodifiedDelay options
                               , C.modifiedDelay = modifiedDelay options
                               , C.notRespondingDelay = notRespondingDelay options
                               , C.fromEmail = fromEmail options
                               , C.toEmail = toEmail options }
