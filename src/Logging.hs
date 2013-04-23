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
               , defaultLogging
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
                 
import qualified Data.Text as T
import System.Log.Logger
import System.Log.Handler.Syslog

defaultName = "wwdcc"

verboseLogging :: IO ()
verboseLogging = updateGlobalLogger defaultName (setLevel INFO)

defaultLogging :: IO ()
defaultLogging = updateGlobalLogger defaultName (setLevel WARNING)

logToSyslog :: String -> IO ()
logToSyslog progName = do
  syslog <- openlog progName [PID] USER DEBUG 
  updateGlobalLogger rootLoggerName $ setHandlers [syslog]
  
-- Convenience wrappers around hslogger functions of same name.
--

wrapLog :: (String -> String -> IO ()) -> T.Text -> IO ()
wrapLog logf = (logf defaultName) . T.unpack

logDebug :: T.Text -> IO ()
logDebug = wrapLog debugM

logInfo :: T.Text -> IO ()
logInfo = wrapLog infoM

logNotice :: T.Text -> IO ()
logNotice = wrapLog noticeM

logWarning :: T.Text -> IO ()
logWarning = wrapLog warningM

logError :: T.Text -> IO ()
logError = wrapLog errorM

logCritical :: T.Text -> IO ()
logCritical = wrapLog criticalM

logAlert :: T.Text -> IO ()
logAlert = wrapLog alertM

logEmergency :: T.Text -> IO ()
logEmergency = wrapLog emergencyM
