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
import qualified Data.ByteString.Lazy.UTF8 as BS
import Control.Concurrent (threadDelay)
import Network.HTTP.Conduit hiding (def)
import Text.HTML.TagSoup
import Data.Maybe
import Control.Monad
import Control.Exception (try)
import Control.Monad.Reader
import Network.Mail.Mime
import Config (Config(..))
import Logging

data SiteStatus = Unmodified | Modified | NotResponding deriving Show

welcomeBody :: [String]
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
  sendMail "wwdcc is now activated!" (unlines welcomeBody) config
  getStatus Unmodified Unmodified config

getStatus :: SiteStatus -> SiteStatus -> Config -> IO ()
getStatus oldestStatus oldStatus config = do
  newStatus <- siteStatus $ url config
  action oldestStatus oldStatus newStatus config
  threadDelay ((delayFor newStatus config) * 10^6) 
  getStatus oldStatus newStatus config

-- Take action depending on the last 3 site statuses.
--

action :: SiteStatus -> SiteStatus -> SiteStatus -> Config -> IO ()

action _ NotResponding Unmodified config = logInfo $ (url config) ++ " is back up, but unchanged."

action _ _ Unmodified config = logInfo $ (url config) ++ " unchanged."

action _ _ Modified config =
  let msg = (url config) ++ " has changed."
  in do
    logWarning msg
    sendMail msg msg config

-- Email once, as soon as the site doesn't respond twice in a row, to
-- give the user a heads-up that things may be about to change.
--
-- Don't email for one-time timeouts, nor during extended outages.
--
action Unmodified NotResponding NotResponding config =
  let msg = (url config) ++ " is not responding."
  in do
    logWarning msg
    sendMail msg msg config
  
action _ _ NotResponding config = logInfo $ (url config) ++ " is not responding."

-- Email generation
--

sendMail :: String -> String -> Config -> IO ()
sendMail subject body config =
  let mail = Mail { mailFrom = (toAddr $ fromEmail config)
                  , mailTo = [(toAddr $ toEmail config)]
                  , mailCc = []
                  , mailBcc = []
                  , mailHeaders = [("Subject", T.pack subject)]
                  , mailParts = [[ Part "text/plain; charset=utf-8" QuotedPrintableText Nothing []
                                    $ BS.fromString body
                                 ]]
                  }
  in
   if (testMode config)
     then do
       mailBS <- renderMail' mail
       logInfo "Test mode: outputing email to log only"
       logInfo $! BS.toString mailBS
     else do
       logNotice $! "Sending email to " ++ (toEmail config)
       renderSendMail mail
  where
    toAddr :: String -> Address
    toAddr str = Address { addressName = Nothing, addressEmail = T.pack str }

delayFor :: SiteStatus -> Config -> Int
delayFor Unmodified = unmodifiedDelay
delayFor Modified = modifiedDelay
delayFor NotResponding = notRespondingDelay

siteStatus :: String -> IO (SiteStatus)
siteStatus url = do
  result <- checkWwdc url
  case result of
    Left _ -> return NotResponding
    Right False -> return Unmodified
    Right True -> return Modified
    
checkWwdc :: String -> IO (Either HttpException Bool)
checkWwdc url = try $ do
  xml <- simpleHttp url
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
