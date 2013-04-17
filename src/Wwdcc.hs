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

module Wwdcc (startChecks) where

import qualified Data.ByteString.Lazy as BS
import Control.Concurrent (threadDelay)
import Network.HTTP.Conduit hiding (def)
import Text.HTML.TagSoup
import Data.Maybe
import Control.Monad
import Control.Exception (try)
import Control.Monad.Reader
import Network.Mail.Mime
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Config (Config(..))
import Logging

data SiteStatus = Unmodified | Modified | NotResponding deriving Show

startChecks :: Config -> IO ()
startChecks = getStatus Unmodified

getStatus :: SiteStatus -> Config -> IO ()
getStatus oldStatus config = do
  newStatus <- siteStatus $ url config
  action oldStatus newStatus config
  threadDelay ((delayFor newStatus config) * 10^6) 
  getStatus newStatus config

-- Take action depending on site status.
--

action :: SiteStatus -> SiteStatus -> Config -> IO ()

action NotResponding Unmodified config =
  let msg = (url config) ++ " is back up, but unchanged."
  in do
    logWarning msg
    sendMail msg config

action _ Unmodified config = logInfo $ (url config) ++ " unchanged."

action _ Modified config =
  let msg = (url config) ++ " has changed."
  in do
    logWarning msg
    sendMail msg config

action Modified NotResponding config = logInfo $ (url config) ++ " is not responding."

action _ NotResponding config =
  let msg = (url config) ++ " is not responding."
  in do
    logWarning msg
    sendMail msg config
  
sendMail :: String -> Config -> IO ()
sendMail msg config =
  let msgText = T.pack msg
  in do
    logNotice $ "Sending email to " ++ (dstEmail config)
    mail <- simpleMail (toAddr $ dstEmail config)
                       (toAddr $ srcEmail config)
                       msgText
                       (TL.fromChunks [msgText]) -- why is this argument of type Text.Lazy???
                       ""
                       []
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
