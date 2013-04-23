-- Module      : Config.
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Wwdcc configuration.
--

module Config ( Config(..)
              , Email(..)
              , Twilio(..)
              ) where

import qualified Data.Text as T
import Data.Maybe

data Twilio = Twilio { accountSid :: !T.Text
                     , authToken :: !T.Text
                     , fromPhone :: !T.Text
                     , toPhone :: !T.Text
                     } deriving (Show, Eq)

data Email = Email { fromEmail :: !T.Text
                   , toEmail:: !T.Text
                   } deriving (Show, Eq)
                     
data Config = Config { daemon :: !Bool
                     , url :: !T.Text
                     , period :: !Int
                     , notifications :: !Int
                     , wait :: !Int
                     , twilio :: Maybe Twilio
                     , email :: Maybe Email
                     } deriving (Show, Eq)
