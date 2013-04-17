-- Module      : Logging
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Logging functions.
--


module Logging ( configureLogger
               , defaultName
               , debugM
               , infoM
               , noticeM
               , warningM
               , errorM
               , criticalM
               , alertM
               , emergencyM 
               ) where
                 
import qualified System.Log.Logger as Logger
  
defaultName = "wwdcc"

configureLogger :: Bool -> IO ()
configureLogger True = Logger.updateGlobalLogger defaultName (Logger.setLevel Logger.INFO)
configureLogger False = return ()  

-- Convenience wrappers around hslogger functions of same name.
--

debugM :: String -> IO ()
debugM = Logger.debugM defaultName

infoM :: String -> IO ()
infoM = Logger.infoM defaultName

noticeM :: String -> IO ()
noticeM = Logger.noticeM defaultName

warningM :: String -> IO ()
warningM = Logger.warningM defaultName

errorM :: String -> IO ()
errorM = Logger.errorM defaultName

criticalM :: String -> IO ()
criticalM = Logger.criticalM defaultName

alertM :: String -> IO ()
alertM = Logger.alertM defaultName

emergencyM :: String -> IO ()
emergencyM = Logger.emergencyM defaultName
