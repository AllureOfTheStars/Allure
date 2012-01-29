-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The main code file of Allure of the Stairs. Here the knot of engine
-- code pieces and the Allure-specific content defintions is tied,
-- resulting in an executable game.
module Main ( main ) where

import qualified Game.LambdaHack.Display as Display
import qualified Game.LambdaHack.Kind as Kind
import qualified Content.ActorKind
import qualified Content.CaveKind
import qualified Content.ItemKind
import qualified Content.PlaceKind
import qualified Content.RuleKind
import qualified Content.TileKind
import qualified Game.LambdaHack.Start as Start
import Game.LambdaHack.Command
import Game.LambdaHack.Display
import qualified Game.LambdaHack.Config as Config
import Game.LambdaHack.Action
import qualified Game.LambdaHack.BindingAction as BindingAction

import qualified ConfigDefault

-- | Gather together the content and verify its consistency.
cops :: Kind.COps
cops = Kind.COps
  { coactor = Kind.createOps Content.ActorKind.cdefs
  , cocave  = Kind.createOps Content.CaveKind.cdefs
  , coitem  = Kind.createOps Content.ItemKind.cdefs
  , coplace = Kind.createOps Content.PlaceKind.cdefs
  , corule  = Kind.createOps Content.RuleKind.cdefs
  , cotile  = Kind.createOps Content.TileKind.cdefs
  }

-- | Wire together config, content and the definitions of game commands
-- to form the starting game session. Evaluate to check for errors.
sess :: Config.CP -> FrontendSession -> Session
sess config sfs =
  let !skeyb = BindingAction.stdBinding config cmdSemantics cmdDescription
      !scops = cops
  in Session{..}

-- | Create the starting game config from the default config file
-- and initialize the engine with the starting session.
start :: FrontendSession -> IO ()
start sfs = do
  config <- Config.mkConfig ConfigDefault.configDefault
  Start.start config $ sess config sfs

-- | Fire up the frontend with the engine fueled by config and content.
-- Which of the frontends is run depends on the flags supplied
-- when compiling the engine library.
main :: IO ()
main = Display.startup start
