-- Module      : Main
-- Copyright   : Copyright © 2013, Quixoftic, LLC <src@quixoftic.com>
-- License     : BSD3 (see LICENSE file)
-- Maintainer  : dhess-src@quixoftic.com
-- Stability   : experimental
-- Portability : GHC
--
-- Command-line wrapper around Wwdc module.
--

module Main where

import Control.Applicative
import Data.Maybe
import Options.Applicative
import qualified MaybeConfig as MC
import Wwdcc
import Config

parser :: Parser MC.Config
parser = MC.Config
         <$> optional (strOption
                       (long "url"
                        <> short 'u'
                        <> metavar "URL"
                        <> help "Override WWDC URL"))
         <*> optional (option
                       (long "unmodified-delay"
                        <> metavar "DELAY"
                        <> help "Delay (in seconds) between actions when site is unmodified"))
         <*> optional (option
                       (long "modified-delay"
                        <> metavar "DELAY"
                        <> help "Delay (in seconds) beteen actions when site has been modified"))
         <*> optional (option
                       (long "not-responding-delay"
                        <> metavar "DELAY"
                        <> help "Delay (in seconds) between actions when site is note responding"))
         <*> optional (argument str ( metavar "SRC_EMAIL" ))
         <*> optional (argument str ( metavar "DEST_EMAIL" ))

main :: IO ()
main = do
  cmdLineConfig <- execParser opts
  startChecks $ fromJust $ MC.validateConfig $ MC.combineConfigs cmdLineConfig MC.defaultConfig
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc description
     <> header "wwdcc - a WWDC checker" )

description = "Send email to SRC_EMAIL from DEST_EMAIL when WWDC site changes or stops responding."
