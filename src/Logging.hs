-- Module      : Logging
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Logging functions.
--


module Logging ( verboseLogging
               , logToSyslog
               , defaultName
               , logDebug
               , logInfo
               , logNotice
               , logWarning
               , logError
               , logCritical
               , logAlert
               , logEmergency 
               ) where
                 
import System.Log.Logger
import System.Log.Handler.Syslog

defaultName = "wwdcc"

verboseLogging :: IO ()
verboseLogging = updateGlobalLogger defaultName (setLevel INFO)

logToSyslog :: String -> IO ()
logToSyslog progName = do
  syslog <- openlog progName [PID] DAEMON DEBUG 
  updateGlobalLogger rootLoggerName $ setHandlers [syslog]
  
-- Convenience wrappers around hslogger functions of same name.
--

logDebug :: String -> IO ()
logDebug = debugM defaultName

logInfo :: String -> IO ()
logInfo = infoM defaultName

logNotice :: String -> IO ()
logNotice = noticeM defaultName

logWarning :: String -> IO ()
logWarning = warningM defaultName

logError :: String -> IO ()
logError = errorM defaultName

logCritical :: String -> IO ()
logCritical = criticalM defaultName

logAlert :: String -> IO ()
logAlert = alertM defaultName

logEmergency :: String -> IO ()
logEmergency = emergencyM defaultName
