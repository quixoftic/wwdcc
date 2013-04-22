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
              ) where

import qualified Data.Text as T
import Data.Maybe

data Email = Email { fromEmail :: !T.Text
                   , toEmail:: !T.Text
                   } deriving (Show, Eq)
                     
data Config = Config { daemon :: !Bool
                     , url :: !T.Text
                     , period :: !Int
                     , notifications :: !Int
                     , wait :: !Int
                     , email :: Maybe Email
                     } deriving (Show, Eq)
