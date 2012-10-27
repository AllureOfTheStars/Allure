{-# LANGUAGE QuasiQuotes #-}
-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game rules and assorted data for Allure of the Stars.
module Content.RuleKind ( cdefs ) where

import Multiline

-- Cabal
import qualified Paths_Allure as Self (getDataFileName, version)

import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import Game.LambdaHack.CDefs

cdefs :: CDefs RuleKind
cdefs = CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validate = ruvalidate
  , content =
      [standard]
  }
standard :: RuleKind

standard = RuleKind
  { rsymbol        = 's'
  , rname          = "standard Allure of the Stars ruleset"
  , rfreq          = [("standard", 100)]
    -- Check whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
    -- TODO: in the future check flying for chasms, swimming for water, etc.
  , raccessible    = \ _lxsize _sloc _src _tloc tgt ->
      F.Walkable `elem` tfeature tgt
  , rtitle         = "Allure of the Stars"
  , rpathsDataFile = Self.getDataFileName
  , rpathsVersion  = Self.version
  , ritemMelee     = "/|\\"
  , ritemProject   = "!?,-~}{"
  -- ASCII art for the Main Menu. Only pure 7-bit ASCII characters are
  -- allowed. The picture should be exactly 24 rows by 80 columns,
  -- plus an extra frame of any charecters that is ignored for all purposes.
  -- For a different screen size, the picture is centered and the outermost
  -- rows and columns cloned. When displayed in the Main Menu screen,
  -- it's overwritten with the game version string and keybinding strings.
  -- The game version string begins and ends with a space and is placed
  -- in the very bottom right corner. The keybindings overwrite places
  -- marked with 25 left curly brace signs '{' in a row. The sign is forbidden
  -- everywhere else. Exactly five such places with 25 left braces
  -- are required, at most one per row, and all are overwritten
  -- with text that is flushed left and padded with spaces.
  -- The Main Menu is displayed dull white on black.
  -- TODO: Highlighted keybinding is in inverse video or bright white on grey
  -- background. The spaces that pad keybindings are not highlighted.
  , rmainMenuArt   = [multiline|
----------------------------------------------------------------------------------
|                                                                                |
|                                                                                |
|                      >> Allure of the Stars <<                                 |
|                                                                                |
|                                                                                |
|                                                                                |
|                      {{{{{{{{{{{{{{{{{{{{{{{{{                                 |
|                                                                                |
|                      {{{{{{{{{{{{{{{{{{{{{{{{{                                 |
|                                                                                |
|                      {{{{{{{{{{{{{{{{{{{{{{{{{                                 |
|                                                                                |
|                      {{{{{{{{{{{{{{{{{{{{{{{{{                                 |
|                                                                                |
|                      {{{{{{{{{{{{{{{{{{{{{{{{{                                 |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
|                                                                                |
|                        Version X.X.X (frontend: gtk, engine: LambdaHack X.X.X) |
----------------------------------------------------------------------------------
|]}
