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
      [hero, alien, animal, robot, horror]
  }
hero,        alien, animal, robot, horror :: FactionKind

hero = FactionKind
  { fsymbol   = '@'
  , fname     = "hero"
  , ffreq     = [("hero", 1)]
  , fAiLeader = "fullAbility"
  , fAiMember = "meleeAdjacent"
  , fentry    = toEnum 1
  }

-- Includes alien-operated robots, alien-conditioned animals and hybrids.
alien = FactionKind
  { fsymbol     = 'a'
  , fname       = "alien"
  , ffreq       = [("alien", 1), ("spawn", 20), ("summon", 20)]
  , fAiLeader   = "fullAbility"
  , fAiMember   = "fullAbility"
  , fentry      = toEnum 5
  }

animal = FactionKind
  { fsymbol     = 'd'
  , fname       = "animal"
  , ffreq       = [("animal", 1), ("spawn", 50), ("summon", 50)]
  , fAiLeader   = "animalAbility"
  , fAiMember   = "animalAbility"
  , fentry      = toEnum 1
  }

-- Autonomous robots.
robot = FactionKind
  { fsymbol     = 'r'
  , fname       = "robot"
  , ffreq       = [("robot", 1), ("spawn", 10), ("summon", 10)]
  , fAiLeader   = "robotAbility"
  , fAiMember   = "robotAbility"
  , fentry      = toEnum 3
  }

horror = FactionKind
  { fsymbol   = 'h'
  , fname     = "horror"
  , ffreq     = [("horror", 1), ("summon", 50)]
  , fAiLeader = "fullAbility"
  , fAiMember = "fullAbility"
  , fentry    = toEnum 1
  }
