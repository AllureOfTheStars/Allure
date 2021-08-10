{-# LANGUAGE TemplateHaskell #-}
-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game rules and assorted game setup data.
module Content.RuleKind
  ( standardRules
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Ini.Reader as Ini
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Syntax
import           System.FilePath
import           System.IO
  (IOMode (ReadMode), hGetContents, hSetEncoding, openFile, utf8)

-- Cabal
import qualified Paths_Allure as Self (version)

import Game.LambdaHack.Content.ItemKind (ItemSymbolsUsedInEngine (..))
import Game.LambdaHack.Content.RuleKind

standardRules :: RuleContent
standardRules = RuleContent
  { rtitle = "Allure of the Stars"
  , rXmax = 80
  , rYmax = 42
  , rexeVersion = Self.version
  -- The strings containing the default configuration file
  -- included from config.ui.default.
  , rcfgUIName = "config.ui" <.> "ini"
  , rcfgUIDefault = $(do
      let path = "GameDefinition" </> "config.ui" <.> "default"
      qAddDependentFile path
      s <- qRunIO $ do
        inputHandle <- openFile path ReadMode
        hSetEncoding inputHandle utf8
        hGetContents inputHandle
      let cfgUIDefault =
            either (error . ("Ini.parse of default config" `showFailure`)) id
            $ Ini.parse s
      lift (s, cfgUIDefault))
  , rwriteSaveClips = 1000
  , rleadLevelClips = 50
  , rscoresFile = "Allure.scores"
  , rnearby = 30
  , rstairWordCarried = ["staircase", "lift"]
  , ritemSymbols = ItemSymbolsUsedInEngine
      { rsymbolProjectile = '{'
      , rsymbolLight      = '('
      , rsymbolTool       = ')'
      , rsymbolSpecial    = '*'  -- don't overuse; it clashes with projectiles
      , rsymbolGold       = '$'  -- also gems
      , rsymbolNecklace   = '"'
      , rsymbolRing       = '='
      , rsymbolPotion     = '!'  -- also concoction, bottle, jar, vial, canister
      , rsymbolFlask      = '!'
      , rsymbolScroll     = '?'  -- book, note, tablet, remote, chip, card
      , rsymbolTorsoArmor = '['
      , rsymbolMiscArmor  = '['
      , rsymbolClothes    = '['
      , rsymbolShield     = ']'
      , rsymbolPolearm    = '/'
      , rsymbolEdged      = '|'
      , rsymbolHafted     = '\\'
      , rsymbolWand       = '-'  -- transmitter, pistol, rifle, instrument
      , rsymbolFood       = ','  -- also body part;
                                 -- distinct from floor: not middle dot
      }
  }
