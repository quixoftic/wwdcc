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
import Data.List.Utils
import System.Posix.Daemonize
import Options.Applicative
import Wwdcc
import Logging
import qualified Config as C

wwdcUrl = "https://developer.apple.com/wwdc/"
description = "Send email to TO_EMAIL from FROM_EMAIL when WWDC site changes or stops responding."
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
  
parseEmail :: String -> Either ParseError C.Email
parseEmail str = parseEmail' $ split "," str
  where
    parseEmail' :: [String] -> Either ParseError C.Email
    parseEmail' (x:y:[]) = Right $ C.Email x y
    parseEmail' _ = Left $ ErrorMsg "Email format is from@example.com,to@example.com"

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
                        <> (help $! "Override WWDC URL (default is " ++ wwdcUrl ++ ")"))
         <*> option (long "period"
                     <> short 'p'
                     <> metavar "DELAY"
                     <> value defaultPeriod
                     <> (help $! "Time between pings, in seconds (default is " ++ (show defaultPeriod) ++ ")"))
         <*> option (long "notifications"
                     <> short 'n'
                     <> metavar "NUM"
                     <> value defaultNotifications
                     <> (help $! "Number of notifications to send when a change is detected (default is " ++ (show defaultNotifications) ++ ")"))
         <*> option (long "wait"
                     <> short 'w'
                     <> metavar "DELAY"
                     <> value defaultWait
                     <> (help $! "Time between notifications, in seconds (default is " ++ (show defaultWait) ++ ")"))
         <*> optional (nullOption (long "email"
                                   <> short 'e'
                                   <> metavar "FROM_EMAIL,TO_EMAIL"
                                   <> reader parseEmail
                                   <> help "Send email notifications from/to address, comma-delimited (default is not to send email notifications)."))

main :: IO ()
main = do
  cmdLineOptions <- execParser opts
  when ((notifications cmdLineOptions) < 0) $ do
    putStrLn "Number of notifications must be a non-negative integer."
    exitFailure
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
                               , C.url = url options
                               , C.period = period options
                               , C.notifications = notifications options
                               , C.wait = wait options
                               , C.email = email options
                               }

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
  logWarning "Terminated."
  sendMail "wwdcc was terminated!" (unlines terminationBody) (C.email config)
  E.throwTo tid ExitSuccess
