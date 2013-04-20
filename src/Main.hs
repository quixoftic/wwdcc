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
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import qualified Control.Exception as E
import System.Posix.Daemonize
import Options.Applicative
import Wwdcc
import Logging
import qualified Config as C

wwdcUrl = "https://developer.apple.com/wwdc/"
description = "Send email to TO_EMAIL from FROM_EMAIL when WWDC site changes or stops responding."
defaultPeriod = 30

data Options = Options { verbose :: !Bool
                       , syslog :: !Bool
                       , daemon :: !Bool
                       , testMode :: !Bool
                       , url :: !String
                       , period :: !Int
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
         <*> option (long "period"
                     <> short 'p'
                     <> metavar "DELAY"
                     <> value defaultPeriod
                     <> (help $! "Time between pings, in seconds (default is " ++ (show defaultPeriod) ++ ")"))
         <*> argument str ( metavar "FROM_EMAIL" )
         <*> argument str ( metavar "TO_EMAIL" )

main :: IO ()
main = do
  cmdLineOptions <- execParser opts
  when (verbose cmdLineOptions) verboseLogging
  when ((syslog cmdLineOptions) || (daemon cmdLineOptions)) $ getProgName >>= logToSyslog
  when (testMode cmdLineOptions) $ logWarning "WARNING: Running in test mode -- no email will be sent!"
  let config = buildConfig cmdLineOptions
    in do
      if (daemon cmdLineOptions)
        then daemonize $ startUp config
        else startUp config
  where
    opts = info (helper <*> parser)
                ( fullDesc
                  <> progDesc description
                  <> header "wwdcc - a WWDC checker" )
    startUp config = do
      mainThreadId <- myThreadId
      installHandler keyboardSignal (Catch (terminationHandler mainThreadId config)) Nothing
      installHandler softwareTermination (Catch (terminationHandler mainThreadId config)) Nothing
      startChecks config

buildConfig :: Options -> C.Config
buildConfig options = C.Config { C.daemon = daemon options
                               , C.testMode = testMode options
                               , C.url = url options
                               , C.period = period options
                               , C.fromEmail = fromEmail options
                               , C.toEmail = toEmail options }

terminationBody :: [String]
terminationBody = [
  "Hi!",
  "",
  "This is the wwdcc service writing to tell you that I have been",
  "terminated. I am no longer monitoring the WWDC homepage.",
  "",
  "FYI,",
  "The wwdcc service"
  ]
  
terminationHandler :: ThreadId -> C.Config -> IO ()
terminationHandler tid config = do
  sendMail "wwdcc was terminated!" (unlines terminationBody) config
  E.throwTo tid ExitSuccess
