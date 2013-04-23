{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import System.IO (FilePath)
import System.Environment (getProgName)
import System.Exit
import System.Posix.Signals
import Control.Concurrent
import qualified Data.Text as T
import qualified Control.Exception as E
import Data.List.Utils
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DC
import System.Posix.Daemonize
import Options.Applicative
import Wwdcc
import Logging
import qualified Config as C

configFile = "$(HOME)/.wwdcc" :: FilePath
wwdcUrl = "https://developer.apple.com/wwdc/"
description = "Send a notification(s) when WWDC site changes or stops responding."
defaultPeriod = 30
defaultNotifications = 3
defaultWait = 30

-- Config file datatypes and loaders.
--

-- Because it contains secrets, Twilio authentication configuration
-- can only be read from a config file.
--
data TwilioAcct = TwilioAcct { accountSid :: !T.Text
                             , authToken :: !T.Text
                             }

data ConfigFile = ConfigFile { twilioAcct :: Maybe TwilioAcct }

loadConfigFile :: FilePath -> IO (ConfigFile)
loadConfigFile path = do
  dotFileConfig <- DC.load [ DC.Optional path ]
  accountSid <- DC.lookup dotFileConfig "twilio.accountSid" :: IO (Maybe T.Text)
  authToken <- DC.lookup dotFileConfig "twilio.authToken" :: IO (Maybe T.Text)
  return ConfigFile { twilioAcct = makeTwilioAcct accountSid authToken }
  where
    makeTwilioAcct :: Maybe T.Text -> Maybe T.Text -> Maybe TwilioAcct
    makeTwilioAcct Nothing _ = Nothing
    makeTwilioAcct _ Nothing = Nothing
    makeTwilioAcct (Just sid) (Just token) = Just TwilioAcct { accountSid = sid
                                                             , authToken = token
                                                             }
  
-- Command-line datatypes and parsers.
--

data SMSOption = SMSOption { fromPhone :: !T.Text
                           , toPhone :: !T.Text
                           }
                 
data Options = Options { verbose :: !Bool
                       , config :: !FilePath
                       , syslog :: !Bool
                       , daemon :: !Bool
                       , url :: !String
                       , period :: !Int
                       , notifications :: !Int
                       , wait :: !Int
                       , sms :: Maybe SMSOption
                       , email :: Maybe C.Email
                       }
  
parsePositiveInt :: String -> Either ParseError Int
parsePositiveInt str = parsePositiveInt' $ (reads str :: [(Int, String)])
  where
    parsePositiveInt' :: [(Int, String)] -> Either ParseError Int
    parsePositiveInt' [(x, "")] = if (x > 0) then Right x else Left ShowHelpText
    parsePositiveInt' _ = Left ShowHelpText

parseSms :: String -> Either ParseError SMSOption
parseSms str = parseSms' $ split "," str
  where
    parseSms' :: [String] -> Either ParseError SMSOption
    parseSms' (x:y:[]) = Right $ SMSOption (T.pack x) (T.pack y)
    parseSms' _ = Left $ ErrorMsg "SMS format is from_number,to_number"
               
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
         <*> strOption (long "config"
                        <> short 'c'
                        <> metavar "PATH"
                        <> value configFile
                        <> showDefault
                        <> help "Path to config file")
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
         <*> optional (nullOption (long "sms"
                                   <> short 's'
                                   <> metavar "FROM_NUMBER,TO_NUMBER"
                                   <> reader parseSms
                                   <> help "Send SMS notifications from/to phone number, comma-delimited (default: don't send SMS notifications)."))
         <*> optional (nullOption (long "email"
                                   <> short 'e'
                                   <> metavar "FROM_EMAIL,TO_EMAIL"
                                   <> reader parseEmail
                                   <> help "Send email notifications from/to address, comma-delimited (default: don't send email notifications)."))

configFileHelp :: [T.Text]
configFileHelp = [ "Here's an example config file:"
                 , ""
                 , "twilio {"
                 , "  accountSid = \"Your Twilio Account SID here\""
                 , "  authToken = \"The corresponding Twilio auth token here\"" 
                 , "}"
                 , ""
                 , "If you don't have a Twilio account, you don't need a config"
                 , "file; simply remove your existing one."
                 ]
                 
main :: IO ()
main = do
  cmdLineOptions <- execParser opts
  dotFileConfig <- E.catch (loadConfigFile (config cmdLineOptions))
                           (\(err :: DC.ConfigError) -> do
                               logError $ T.unlines [ "Your config file is broken. The parser error was:"
                                                    , T.pack $ show err]
                               logError $ T.unlines configFileHelp
                               exitFailure)
  when (verbose cmdLineOptions) verboseLogging
  when ((syslog cmdLineOptions) || (daemon cmdLineOptions)) $ getProgName >>= logToSyslog
  let config = buildConfig cmdLineOptions dotFileConfig
    in do
      when (Nothing == (C.email config) && Nothing == (C.twilio config)) $ logWarning "Warning: no notifications will be sent! Running anyway...."
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

buildConfig :: Options -> ConfigFile -> C.Config
buildConfig options configFile = C.Config { C.daemon = daemon options
                                          , C.url = T.pack $ url options
                                          , C.period = period options
                                          , C.notifications = notifications options
                                          , C.wait = wait options
                                          , C.twilio = twilioConfig options configFile
                                          , C.email = email options }
  where
    twilioConfig :: Options -> ConfigFile -> Maybe C.Twilio
    twilioConfig opts cfg = do
      smsConfig <- sms opts
      acctConfig <- twilioAcct cfg
      return C.Twilio { C.accountSid = accountSid acctConfig
                      , C.authToken = authToken acctConfig
                      , C.fromPhone = fromPhone smsConfig
                      , C.toPhone = toPhone smsConfig
                      }

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
  sendSms "wwdc was terminated!" (C.twilio config)
  E.throwTo tid ExitSuccess
