-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.).
module Content.FactionKind ( cdefs ) where

import qualified Game.LambdaHack.Content as Content
import Game.LambdaHack.Content.FactionKind

cdefs :: Content.CDefs FactionKind
cdefs = Content.CDefs
  { getSymbol = fsymbol
  , getName = fname
  , getFreq = ffreq
  , validate = fvalidate
  , content =
      [hero, alien, animal, robot]
  }
hero,        alien, animal, robot :: FactionKind

hero = FactionKind
  { fsymbol  = '@'
  , fname    = "hero"
  , ffreq    = [("hero", 1), ("playable", 50)]
  , fmoveAll = False
  , fenemy   = ["alien"]
  , fally    = []
  }

-- Includes alien-operated robots, alien-conditioned animals and hybrids.
alien = FactionKind
  { fsymbol  = 'a'
  , fname    = "alien"
  , ffreq    = [("alien", 1), ("playable", 50), ("spawn", 20)]
  , fmoveAll = False
  , fenemy   = ["hero"]
  , fally    = []
  }

animal = FactionKind
  { fsymbol  = 'd'
  , fname    = "animal"
  , ffreq    = [("animal", 1), ("spawn", 50)]
  , fmoveAll = True
  , fenemy   = ["hero", "alien"]  -- animals hunt external intruders
  , fally    = []
  }

-- Autonomous robots.
robot = FactionKind
  { fsymbol  = 'r'
  , fname    = "robot"
  , ffreq    = [("robot", 1), ("spawn", 30)]
  , fmoveAll = True
  , fenemy   = ["hero", "alien", "animal", "robot"]  -- attack all that moves
  , fally    = []
  }
