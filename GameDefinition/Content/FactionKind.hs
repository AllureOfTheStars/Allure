-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.)
-- for Allure of the Stars.
module Content.FactionKind ( cdefs ) where

import qualified Data.EnumMap.Strict as EM

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
      [hero, civilian, alien, animal, robot, horror]
  }
hero,        civilian, alien, animal, robot, horror :: FactionKind

hero = FactionKind
  { fsymbol       = '1'
  , fname         = "hero"
  , ffreq         = [("hero", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = meleeAdjacent
  }

civilian = FactionKind
  { fsymbol       = '@'
  , fname         = "civilian"
  , ffreq         = [("civilian", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = allSkills  -- not coordinated by any leadership
  }

-- Includes alien-operated robots, alien-conditioned animals and hybrids.
alien = FactionKind
  { fsymbol       = 'a'
  , fname         = "alien"
  , ffreq         = [("alien", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = allSkills
  }

animal = FactionKind
  { fsymbol       = 'd'
  , fname         = "animal"
  , ffreq         = [("animal", 1)]
  , fSkillsLeader = animalSkills
  , fSkillsOther  = animalSkills
  }

-- Autonomous robots.
robot = FactionKind
  { fsymbol       = 'r'
  , fname         = "robot"
  , ffreq         = [("robot", 1)]
  , fSkillsLeader = robotSkills
  , fSkillsOther  = robotSkills
  }

horror = FactionKind
  { fsymbol       = 'h'
  , fname         = "horror"
  , ffreq         = [("horror", 1)]
  , fSkillsLeader = allSkills
  , fSkillsOther  = allSkills
  }


meleeAdjacent, _meleeAndRanged, animalSkills, robotSkills, allSkills :: Skills

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1, 1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1, 1..]

animalSkills =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1, 1..]

robotSkills = EM.delete AbApply unitSkills

allSkills = unitSkills
