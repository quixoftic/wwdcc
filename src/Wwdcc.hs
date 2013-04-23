{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
-- Module      : Wwdcc
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Check https://developer.apple.com/wwdc/ repeatedly, and look for a
-- possible 2013 announcement.
--

module Wwdcc ( startChecks
             , sendMail
             , sendSms
             ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy.UTF8 as BS
import Control.Concurrent (threadDelay)
import System.Exit
import Network.HTTP.Conduit hiding (def)
import Text.HTML.TagSoup
import Data.Maybe
import Control.Monad
import qualified Control.Exception as E
import Control.Monad.Reader
import Network.Mail.Mime
import Config
import Logging

data SiteStatus = Unmodified | Modified | NotResponding deriving Show

welcomeBody :: [T.Text]
welcomeBody = [
  "Hi!",
  "",
  "This is the wwdcc service writing to tell you that I am now monitoring",
  "the WWDC homepage for updates and downtime. I will notify you as soon",
  "as I detect a change.",
  "",
  "Humbly yours,",
  "The wwdcc service"
  ]

startChecks :: Config -> IO ()
startChecks config = do
  sendMail "wwdcc is now activated!" (T.unlines welcomeBody) (email config)
  sendSms "wwdcc is now activated!" (twilio config)
  getStatus Unmodified Unmodified config

getStatus :: SiteStatus -> SiteStatus -> Config -> IO ()
getStatus oldestStatus oldStatus config = do
  newStatus <- siteStatus $ url config
  action oldestStatus oldStatus newStatus config
  threadDelay ((period config) * 10^6) 
  getStatus oldStatus newStatus config

-- Take action depending on the last 3 site statuses.
--

action :: SiteStatus -> SiteStatus -> SiteStatus -> Config -> IO ()

action _ NotResponding Unmodified config = logInfo $ T.unwords [url config, "is back up, but unchanged."]

action _ _ Unmodified config = logInfo $ T.unwords [url config, "unchanged."]

-- Modified: send notifications and exit.
--
action _ _ Modified config =
  let msg = T.unwords [url config, "has changed!"]
      ntotal = notifications config
      nTotalStr = T.pack $ show ntotal
      msgNumber n = T.concat [ "("
                             , T.pack $ show n
                             , "/"
                             , nTotalStr
                             , ")"
                             ]
      sendNotification 0 = do
        logNotice "Exiting."
        exitSuccess -- quit the program.
      sendNotification 1 = do
        sendMail msg 
                 (T.unlines ["Hi!",
                           "",
                           msg,
                           "",
                           "This is the last notification I will send. I am now exiting.",
                           "",
                           "FYI,",
                           "The wwdcc service"
                          ])
                 (email config)
        sendSms (T.unwords [msg, msgNumber ntotal])
                (twilio config)
        sendNotification 0
      sendNotification nleft = do
        sendMail msg
                 (T.unlines ["Hi!",
                           "",
                           msg,
                           "",
                           "FYI,",
                           "The wwdcc service"
                           ])
                 (email config)
        sendSms (T.unwords [msg, msgNumber $ ntotal - nleft + 1])
                (twilio config)
        threadDelay ((wait config) * 10^6) 
        sendNotification $ nleft - 1
  in do
    logWarning msg
    sendNotification ntotal

-- Email once, as soon as the site doesn't respond twice in a row, to
-- give the user a heads-up that things may be about to change.
--
-- Don't email for one-time timeouts, nor during extended outages.
--
action Unmodified NotResponding NotResponding config =
  let msg = T.unwords [url config, "is not responding."]
  in do
    logWarning msg
    sendMail msg msg (email config)
    sendSms msg (twilio config)
  
action _ _ NotResponding config = logInfo $ T.unwords [url config, "is not responding."]


-- SMS generation
--

twilioSendSmsUri :: T.Text -> T.Text
twilioSendSmsUri accountSid = T.concat [ "https://api.twilio.com/2010-04-01/Accounts/"
                                       , accountSid
                                       , "/SMS/Messages.json" ]

sendSms :: T.Text -> Maybe Twilio -> IO ()
sendSms _ Nothing = return ()
sendSms body (Just twilio) = do
  logNotice $ T.unwords ["Sending SMS to", toPhone twilio, "with text:", body]
  E.catch (do request' <- parseUrl $ T.unpack $ twilioSendSmsUri (accountSid twilio)
              let request = urlEncodedBody [ ("From", TE.encodeUtf8 $ fromPhone twilio)
                                           , ("To", TE.encodeUtf8 $ toPhone twilio)
                                           , ("Body", TE.encodeUtf8 body) ] $
                            applyBasicAuth (TE.encodeUtf8 $ accountSid twilio)
                                           (TE.encodeUtf8 $ authToken twilio)
                                           request'
              result <- withManager $ httpLbs request
              return ())
          (\(err :: HttpException) -> do logError $ T.concat [ "Unable to send SMS notification! ("
                                                              , T.pack $ show err
                                                              , ")" ])

-- Email generation
--

sendMail :: T.Text -> T.Text -> Maybe Email -> IO ()
sendMail _ _ Nothing = return ()
sendMail subject body (Just email) = do
  logNotice $ T.unwords ["Sending email to", toEmail email]
  renderSendMail Mail { mailFrom = (toAddr $ fromEmail email)
                      , mailTo = [(toAddr $ toEmail email)]
                      , mailCc = []
                      , mailBcc = []
                      , mailHeaders = [("Subject", subject)]
                      , mailParts = [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
                                       $ TLE.encodeUtf8 $ TL.fromChunks [body]
                                     ]]
                      }
  where
    toAddr :: T.Text -> Address
    toAddr str = Address { addressName = Nothing, addressEmail = str }

-- Site-related stuff.
--

siteStatus :: T.Text -> IO (SiteStatus)
siteStatus url = do
  result <- checkWwdc url
  case result of
    Left _ -> return NotResponding
    Right False -> return Unmodified
    Right True -> return Modified

checkWwdc :: T.Text -> IO (Either HttpException Bool)
checkWwdc url = E.try $ do
  xml <- simpleHttp $ T.unpack url
  return $ pageIsModified xml

expected = "WWDC 2012. June 11-15 in San Francisco. It's the week we've all been waiting for."

pageIsModified :: BS.ByteString -> Bool
pageIsModified xml = maybe True (/= expected) (findCanary $ parseTags xml)

-- A little trick to avoid all the ::String type signatures that are
-- needed with TagSoup and OverloadedString. Thanks to sclv at Stack
-- Overflow for the tip.
s :: String -> String
s = id

findCanary :: [Tag BS.ByteString] -> Maybe BS.ByteString
findCanary = liftM (fromAttrib "alt" . head) . listToMaybe . sections (~== s "<img>") . takeWhile (~/= s "</a>") . dropWhile (~/= s "<header class=\"hero\">")
