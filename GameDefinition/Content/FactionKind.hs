-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.)
-- for Allure of the Stars.
module Content.FactionKind ( cdefs ) where

import Data.List

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.FactionKind

cdefs :: ContentDef FactionKind
cdefs = ContentDef
  { getSymbol = fsymbol
  , getName = fname
  , getFreq = ffreq
  , validate = validateFactionKind
  , content =
      [hero, alien, animal, robot, horror]
  }
hero,        alien, animal, robot, horror :: FactionKind

hero = FactionKind
  { fsymbol        = '@'
  , fname          = "hero"
  , ffreq          = [("hero", 1)]
  , fAbilityLeader = allAbilities
  , fAbilityOther  = meleeAdjacent
  }

-- Includes alien-operated robots, alien-conditioned animals and hybrids.
alien = FactionKind
  { fsymbol        = 'a'
  , fname          = "alien"
  , ffreq          = [("alien", 1), ("summon", 20)]
  , fAbilityLeader = allAbilities
  , fAbilityOther  = allAbilities
  }

animal = FactionKind
  { fsymbol        = 'd'
  , fname          = "animal"
  , ffreq          = [("animal", 1), ("summon", 50)]
  , fAbilityLeader = animalAbility
  , fAbilityOther  = animalAbility
  }

-- Autonomous robots.
robot = FactionKind
  { fsymbol        = 'r'
  , fname          = "robot"
  , ffreq          = [("robot", 1), ("summon", 10)]
  , fAbilityLeader = robotAbility
  , fAbilityOther  = robotAbility
  }

horror = FactionKind
  { fsymbol        = 'h'
  , fname          = "horror"
  , ffreq          = [("horror", 1), ("summon", 50)]
  , fAbilityLeader = allAbilities
  , fAbilityOther  = allAbilities
  }


_noAbility, _onlyFollowTrack, meleeAdjacent, _meleeAndRanged, animalAbility, robotAbility, allAbilities :: [Ability]

_noAbility = []  -- not even projectiles will fly

_onlyFollowTrack = [Track]  -- projectiles enabled

meleeAdjacent = [Track, Melee]

_meleeAndRanged = [Track, Melee, Ranged]  -- melee and reaction fire

animalAbility = [Track, Flee, Melee, Displace, Chase, Wander]

robotAbility = delete Flee [minBound..maxBound]

allAbilities = [minBound..maxBound]
