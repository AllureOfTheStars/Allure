-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Definition of basic players for Allure of the Stars.
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
  , fentryLevel = 1
  , finitialActors = 3
  , fhasLeader = True
  , fisAI = False
  , fhasUI = True
  }

playerAntiHero = playerHero
  { fisAI = True
  , fhasUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills  -- not coordinated by any leadership
  , fisSpawn = False
  , fisHero = False
  , fentryLevel = 1
  , finitialActors = 3
  , fhasLeader = False  -- unorganized
  , fisAI = True
  , fhasUI = False
  }

playerAlien = Player
  { fname = "Alien Hierarchy"
  , fgroup = "alien"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fisSpawn = True
  , fisHero = False
  , fentryLevel = 4
  , finitialActors = 3
  , fhasLeader = True
  , fisAI = True
  , fhasUI = False
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsLeader = animalSkills
  , fskillsOther  = animalSkills
  , fisSpawn = True
  , fisHero = False
  , fentryLevel = 2
  , finitialActors = 3
  , fhasLeader = False
  , fisAI = True
  , fhasUI = False
  }

playerRobot = Player
  { fname = "Robot Anarchy"
  , fgroup = "robot"
  , fskillsLeader = robotSkills
  , fskillsOther  = robotSkills
  , fisSpawn = True
  , fisHero = False
  , fentryLevel = 3
  , finitialActors = 3
  , fhasLeader = False
  , fisAI = True
  , fhasUI = False
  }

playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fisSpawn = False
  , fisHero = False
  , fentryLevel = 1
  , finitialActors = 0
  , fhasLeader = False
  , fisAI = True
  , fhasUI = False
  }


meleeAdjacent, _meleeAndRanged, animalSkills, robotSkills, allSkills :: Skills

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1, 1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1, 1..]

animalSkills =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1, 1..]

robotSkills = EM.delete AbApply unitSkills

allSkills = unitSkills
