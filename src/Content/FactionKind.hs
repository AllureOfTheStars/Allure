{-# LANGUAGE OverloadedStrings #-}
-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game factions (heroes, enemies, NPCs, etc.) for Allure of the Stars.
module Content.FactionKind ( cdefs ) where

import Game.LambdaHack.CDefs
import Game.LambdaHack.Content.FactionKind

cdefs :: CDefs FactionKind
cdefs = CDefs
  { getSymbol = fsymbol
  , getName = fname
  , getFreq = ffreq
  , validate = fvalidate
  , content =
      [hero, alien, animal, robot]
  }
hero,        alien, animal, robot :: FactionKind

hero = FactionKind
  { fsymbol     = '@'
  , fname       = "hero"
  , ffreq       = [("hero", 1), ("playable", 50)]
  , fAiSelected = "noAbility"  -- no AI, fully manual control
  , fAiIdle     = "meleeAdjacent"
  , fenemy      = ["alien"]
  , fally       = []
  }

-- Includes alien-operated robots, alien-conditioned animals and hybrids.
alien = FactionKind
  { fsymbol     = 'a'
  , fname       = "alien"
  , ffreq       = [("alien", 1), ("playable", 50), ("spawn", 20)]
  , fAiSelected = "fullAbility"
  , fAiIdle     = "fullAbility"
  , fenemy      = ["hero"]
  , fally       = []
  }

animal = FactionKind
  { fsymbol     = 'd'
  , fname       = "animal"
  , ffreq       = [("animal", 1), ("spawn", 50)]
  , fAiSelected = "fullAbility"
  , fAiIdle     = "fullAbility"
  , fenemy      = ["hero", "alien"]  -- animals hunt external intruders
  , fally       = []
  }

-- Autonomous robots.
robot = FactionKind
  { fsymbol     = 'r'
  , fname       = "robot"
  , ffreq       = [("robot", 1), ("spawn", 30)]
  , fAiSelected = "fullAbility"
  , fAiIdle     = "fullAbility"
  , fenemy      = ["hero", "alien", "animal", "robot"]  -- hunt all that moves
  , fally       = []
  }
