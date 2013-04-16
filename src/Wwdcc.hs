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
import System.Posix.Syslog (syslog, Priority(Notice))
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

action NotResponding Unmodified config = do
  putStrLn "Site is back, but unchanged; emailing user."
  sendMail "WWDC site back up, but unchanged" config

action _ Unmodified _ = putStrLn "Site unchanged."

action _ Modified config = do
  putStrLn "Site has changed! Emailing user."
  sendMail "has changed!" config

action Modified NotResponding _ = putStrLn "Site is not responding!"

action _ NotResponding config = do
  putStrLn "Site is not responding! Emailing user."
  sendMail "has stopped responding" config
  
sendMail :: T.Text -> Config -> IO ()
sendMail subject config = do
  msg <- simpleMail 
           (toAddr $ dstEmail config) 
           (toAddr $ srcEmail config)
           (T.concat ["WWDC site ", subject])
           (TL.concat [(TL.pack $ url config), " " , (TL.fromChunks [subject])])
           ""
           []
  renderSendMail msg
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
