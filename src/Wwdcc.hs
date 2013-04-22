{-# LANGUAGE OverloadedStrings #-}
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
             ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy.UTF8 as BS
import Control.Concurrent (threadDelay)
import System.Exit
import Network.HTTP.Conduit hiding (def)
import Text.HTML.TagSoup
import Data.Maybe
import Control.Monad
import Control.Exception (try)
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
  let msg = T.unwords [url config, "has changed."]
      sendNotification 0 = exitSuccess -- quit the program.
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
        threadDelay ((wait config) * 10^6) 
        sendNotification $ nleft - 1
  in do
    logWarning msg
    sendNotification $ notifications config

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
  
action _ _ NotResponding config = logInfo $ T.unwords [url config, "is not responding."]

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
                                       $ E.encodeUtf8 $ TL.fromChunks [body]
                                     ]]
                      }
  where
    toAddr :: T.Text -> Address
    toAddr str = Address { addressName = Nothing, addressEmail = str }

siteStatus :: T.Text -> IO (SiteStatus)
siteStatus url = do
  result <- checkWwdc url
  case result of
    Left _ -> return NotResponding
    Right False -> return Unmodified
    Right True -> return Modified
    
checkWwdc :: T.Text -> IO (Either HttpException Bool)
checkWwdc url = try $ do
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
