{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as T
import qualified Control.Exception as E
import Data.List.Utils
import System.Posix.Daemonize
import Options.Applicative
import Wwdcc
import Logging
import qualified Config as C

wwdcUrl = "https://developer.apple.com/wwdc/"
description = "Send a notification(s) when WWDC site changes or stops responding."
defaultPeriod = 30
defaultNotifications = 3
defaultWait = 30

data Options = Options { verbose :: !Bool
                       , syslog :: !Bool
                       , daemon :: !Bool
                       , url :: !String
                       , period :: !Int
                       , notifications :: !Int
                       , wait :: !Int
                       , email :: Maybe C.Email
                       }
  
parsePositiveInt :: String -> Either ParseError Int
parsePositiveInt str = parsePositiveInt' $ (reads str :: [(Int, String)])
  where
    parsePositiveInt' :: [(Int, String)] -> Either ParseError Int
    parsePositiveInt' [(x, "")] = if (x > 0) then Right x else Left ShowHelpText
    parsePositiveInt' _ = Left ShowHelpText
  
parseEmail :: String -> Either ParseError C.Email
parseEmail str = parseEmail' $ split "," str
  where
    parseEmail' :: [String] -> Either ParseError C.Email
    parseEmail' (x:y:[]) = Right $ C.Email (T.pack x) (T.pack y)
    parseEmail' _ = Left $ ErrorMsg "Email format is from@example.com,to@example.com"

parser :: Parser Options
parser = Options
         <$> switch (long "verbose"
                     <> short 'v'
                     <> help "Verbose logging")
         <*> switch (long "syslog"
                     <> short 's'
                     <> help "Log to syslog (default: log to stderr)")
         <*> switch (long "daemon"
                     <> help "Run as a daemon (implies --syslog)")
         <*> strOption (long "url"
                        <> short 'u'
                        <> metavar "URL"
                        <> value wwdcUrl
                        <> showDefault
                        <> help "Override WWDC URL")
         <*> nullOption (long "period"
                         <> short 'p'
                         <> metavar "DELAY"
                         <> value defaultPeriod
                         <> showDefault
                         <> reader parsePositiveInt
                         <> help "Time between pings, in seconds")
         <*> nullOption (long "notifications"
                         <> short 'n'
                         <> metavar "NUM"
                         <> value defaultNotifications
                         <> showDefault
                         <> reader parsePositiveInt
                         <> help "Number of notifications to send when a change is detected")
         <*> nullOption (long "wait"
                         <> short 'w'
                         <> metavar "DELAY"
                         <> value defaultWait
                         <> showDefault
                         <> reader parsePositiveInt
                         <> help "Time between notifications, in seconds")
         <*> optional (nullOption (long "email"
                                   <> short 'e'
                                   <> metavar "FROM_EMAIL,TO_EMAIL"
                                   <> reader parseEmail
                                   <> help "Send email notifications from/to address, comma-delimited (default: don't send email notifications)."))

main :: IO ()
main = do
  cmdLineOptions <- execParser opts
  when (verbose cmdLineOptions) verboseLogging
  when ((syslog cmdLineOptions) || (daemon cmdLineOptions)) $ getProgName >>= logToSyslog
  let config = buildConfig cmdLineOptions
    in do
      when (Nothing == (C.email config)) $ logWarning "Warning: no email notifications will be sent! Running anyway...."
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
                               , C.url = T.pack $ url options
                               , C.period = period options
                               , C.notifications = notifications options
                               , C.wait = wait options
                               , C.email = email options }

terminationBody :: [T.Text]
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
  logWarning "Terminated."
  sendMail "wwdcc was terminated!" (T.unlines terminationBody) (C.email config)
  E.throwTo tid ExitSuccess
