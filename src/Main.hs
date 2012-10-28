-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The main code file of Allure of the Stairs. Here the knot of engine
-- code pieces and the Allure-specific content defintions is tied,
-- resulting in an executable game.
module Main ( main ) where

import qualified Game.LambdaHack.Kind as Kind
import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.FactionKind
import qualified Content.ItemKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import Game.LambdaHack.Turn
import Game.LambdaHack.Action
import qualified Game.LambdaHack.BindingAction as BindingAction

import qualified ConfigDefault

-- | Fire up the frontend with the engine fueled by content and config.
-- Which of the frontends is run depends on the flags supplied
-- when compiling the engine library.
main :: IO ()
main = do
  config <- mkConfig ConfigDefault.configDefault
  let cops = Kind.COps
        { coactor = Kind.createOps Content.ActorKind.cdefs
        , cocave  = Kind.createOps Content.CaveKind.cdefs
        , cofact  = Kind.createOps Content.FactionKind.cdefs
        , coitem  = Kind.createOps Content.ItemKind.cdefs
        , coplace = Kind.createOps Content.PlaceKind.cdefs
        , corule  = Kind.createOps Content.RuleKind.cdefs
        , cotile  = Kind.createOps Content.TileKind.cdefs
        }
      binding = BindingAction.stdBinding config
  startFrontend cops binding config handleGame
