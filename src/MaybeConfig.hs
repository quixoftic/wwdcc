{-# LANGUAGE DeriveDataTypeable #-}
-- Module      : MaybeConfig
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Partial configuration, as from a command line or config file.
--

module MaybeConfig ( Config(..)
                   , defaultConfig
                   , combineConfigs
                   , validateConfig
                   ) where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Data
import qualified Config as C

data Config = Config { url :: Maybe String
                     , unmodifiedDelay :: Maybe Int
                     , modifiedDelay :: Maybe Int
                     , notRespondingDelay :: Maybe Int
                     , srcEmail :: Maybe String
                     , dstEmail :: Maybe String
                     } deriving (Show, Typeable, Data)

defaultConfig = Config { url = Just "https://developer.apple.com/wwdc/"
                                 , unmodifiedDelay = Just 30
                                 , modifiedDelay = Just 60
                                 , notRespondingDelay = Just 60
                                 , srcEmail = Nothing
                                 , dstEmail = Nothing }

combineConfigs :: Config -> Config -> Config
combineConfigs primary secondary = Config { url = maybe (url secondary) Just (url primary)
                                          , unmodifiedDelay = maybe (unmodifiedDelay secondary) Just (unmodifiedDelay primary)
                                          , modifiedDelay = maybe (modifiedDelay secondary) Just (modifiedDelay primary)
                                          , notRespondingDelay = maybe (notRespondingDelay secondary) Just (notRespondingDelay primary)
                                          , srcEmail = maybe (srcEmail secondary) Just (srcEmail primary)
                                          , dstEmail = maybe (dstEmail secondary) Just (dstEmail primary) }

validateConfig :: Config -> Maybe C.Config
validateConfig maybeConfig = do
  url <- url maybeConfig
  unmodifiedDelay <- unmodifiedDelay maybeConfig
  modifiedDelay <- modifiedDelay maybeConfig
  notRespondingDelay <- notRespondingDelay maybeConfig
  srcEmail <- srcEmail maybeConfig
  dstEmail <- dstEmail maybeConfig
  return C.Config { C.url = url
                  , C.unmodifiedDelay = unmodifiedDelay
                  , C.modifiedDelay = modifiedDelay
                  , C.notRespondingDelay = notRespondingDelay
                  , C.srcEmail = srcEmail
                  , C.dstEmail = dstEmail }
