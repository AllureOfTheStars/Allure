-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game factions (heroes, enemies, NPCs, etc.)
-- for Allure of the Stars.
module Content.ModeKindPlayer
  ( playerHero, playerAntiHero, playerCivilian, playerAlien, playerAnimal
  , playerRobot, playerHorror
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Content.ModeKind

playerHero, playerAntiHero, playerCivilian, playerAlien, playerAnimal, playerRobot, playerHorror :: Player

playerHero = Player
  { fname = "Spacefarer Crew"
  , fgroup = "hero"
  , fskillsLeader = allSkills
  , fskillsOther  = meleeAdjacent
  , fisSpawn = False
  , fisHero = True
  , fentry = 1
  , finitial = 3
  , fleader = True
  , fisAI = False
  , fisUI = True
  }

playerAntiHero = playerHero
  { fisAI = True
  , fisUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills  -- not coordinated by any leadership
  , fisSpawn = False
  , fisHero = False
  , fentry = 1
  , finitial = 3
  , fleader = False  -- unorganized
  , fisAI = True
  , fisUI = False
  }

playerAlien = Player
  { fname = "Alien Hierarchy"
  , fgroup = "alien"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fisSpawn = True
  , fisHero = False
  , fentry = 4
  , finitial = 3
  , fleader = True
  , fisAI = True
  , fisUI = False
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsLeader = animalSkills
  , fskillsOther  = animalSkills
  , fisSpawn = True
  , fisHero = False
  , fentry = 2
  , finitial = 3
  , fleader = False
  , fisAI = True
  , fisUI = False
  }

playerRobot = Player
  { fname = "Robot Anarchy"
  , fgroup = "robot"
  , fskillsLeader = robotSkills
  , fskillsOther  = robotSkills
  , fisSpawn = True
  , fisHero = False
  , fentry = 3
  , finitial = 3
  , fleader = False
  , fisAI = True
  , fisUI = False
  }

playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fisSpawn = False
  , fisHero = False
  , fentry = 1
  , finitial = 0
  , fleader = False
  , fisAI = True
  , fisUI = False
  }


meleeAdjacent, _meleeAndRanged, animalSkills, robotSkills, allSkills :: Skills

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1, 1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1, 1..]

animalSkills =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1, 1..]

robotSkills = EM.delete AbApply unitSkills

allSkills = unitSkills
