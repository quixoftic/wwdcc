{-# LANGUAGE DeriveDataTypeable #-}
-- Module      : Config.
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Wwdcc configuration.
--

module Config (Config(..)) where

import Data.Data

data Config = Config { url :: !String
                     , unmodifiedDelay :: !Int
                     , modifiedDelay :: !Int
                     , notRespondingDelay :: !Int
                     , srcEmail :: !String
                     , dstEmail :: !String
                     } deriving (Show, Typeable, Data)