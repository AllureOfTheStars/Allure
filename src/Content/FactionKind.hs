{-# LANGUAGE OverloadedStrings #-}
-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.)
-- for LambdaHack.
module Content.FactionKind ( cdefs ) where

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.FactionKind

cdefs :: ContentDef FactionKind
cdefs = ContentDef
  { getSymbol = fsymbol
  , getName = fname
  , getFreq = ffreq
  , validate = fvalidate
  , content =
      [hero, alien, animal, robot]
  }
hero,        alien, animal, robot :: FactionKind

hero = FactionKind
  { fsymbol   = '@'
  , fname     = "hero"
  , ffreq     = [("hero", 1), ("playable", 50)]
  , fAiLeader = "fullAbility"
  , fAiMember = "meleeAdjacent"
  , fenemy    = ["monster"]
  , fally     = []
  , fspawn    = 0
  , fentry    = toEnum 1
  }

-- Includes alien-operated robots, alien-conditioned animals and hybrids.
alien = FactionKind
  { fsymbol     = 'a'
  , fname       = "alien"
  , ffreq       = [("alien", 1), ("playable", 50)]
  , fAiLeader   = "fullAbility"
  , fAiMember   = "fullAbility"
  , fenemy      = ["hero"]
  , fally       = []
  , fspawn      = 20
  , fentry      = toEnum 5  -- doesn't matter
  }

animal = FactionKind
  { fsymbol     = 'd'
  , fname       = "animal"
  , ffreq       = [("animal", 1)]
  , fAiLeader   = "animalAbility"
  , fAiMember   = "animalAbility"
  , fenemy      = ["hero", "alien"]  -- animals hunt external intruders
  , fally       = []
  , fspawn      = 50
  , fentry      = toEnum 1  -- doesn't matter
  }

-- Autonomous robots.
robot = FactionKind
  { fsymbol     = 'r'
  , fname       = "robot"
  , ffreq       = [("robot", 1)]
  , fAiLeader   = "robotAbility"
  , fAiMember   = "robotAbility"
  , fenemy      = ["hero", "alien", "animal"]  -- hunt all organic
  , fally       = []
  , fspawn      = 10
  , fentry      = toEnum 3  -- doesn't matter
  }
