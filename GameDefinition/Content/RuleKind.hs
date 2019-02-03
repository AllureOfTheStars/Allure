{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game rules and assorted game setup data.
module Content.RuleKind
  ( standardRules
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Language.Haskell.TH.Syntax
import System.FilePath
import System.IO (readFile)

-- Cabal
import qualified Paths_Allure as Self (getDataFileName, version)

import Game.LambdaHack.Content.RuleKind

standardRules :: RuleContent
standardRules = RuleContent
  { rtitle = "Allure of the Stars"
  , rXmax = 80
  , rYmax = 42
  , rfontDir = $(do
      x <- qRunIO (Self.getDataFileName "GameDefinition/fonts")
      lift x)
  , rexeVersion = Self.version
  -- The strings containing the default configuration file
  -- included from config.ui.default.
  , rcfgUIName = "config.ui" <.> "ini"
  , rcfgUIDefault = $(do
      let path = "GameDefinition" </> "config.ui" <.> "default"
      qAddDependentFile path
      x <- qRunIO (readFile path)
      lift x)
  , rfirstDeathEnds = False
  , rwriteSaveClips = 1000000
  , rleadLevelClips = 50
  , rscoresFile = "Allure.scores"
  , rnearby = 20
  , rstairWordCarried = ["staircase", "lift"]
  , rsymbolProjectile = '{'
  }
