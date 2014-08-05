-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Basic players definitions.
module Content.ModeKindPlayer
  ( playerHero, playerAntiHero, playerCivilian, playerMonster
  , playerAntiMonster, playerAnimal, playerRobot, playerHorror
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Content.ModeKind

playerHero, playerAntiHero, playerCivilian, playerMonster, playerAnimal, playerAntiMonster, playerRobot, playerHorror :: Player

playerHero = Player
  { fname = "Spacefarer Crew"
  , fgroup = "hero"
  , fskillsLeader = allSkills
  , fskillsOther  = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhasNumbers = True
  , fhasGender = True
  , foverrideAI = Nothing
  , fentryLevel = 1
  , finitialActors = 3
  , fhasLeader = LeaderMode False False
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
  , fcanEscape = False
  , fneverEmpty = True
  , fhasNumbers = False
  , fhasGender = True
  , foverrideAI = Nothing
  , fentryLevel = 1
  , finitialActors = 3
  , fhasLeader = LeaderNull  -- unorganized
  , fisAI = True
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Alien Hierarchy"
  , fgroup = "alien"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , foverrideAI = Nothing
  , fentryLevel = 4
  , finitialActors = 3
  , fhasLeader = LeaderMode True False
  , fisAI = True
  , fhasUI = False
  }

playerAntiMonster = playerMonster
  { fisAI = False
  , fhasUI = True
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsLeader = animalSkills
  , fskillsOther  = animalSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , foverrideAI = Nothing
  , fentryLevel = 2
  , finitialActors = 3
  , fhasLeader = LeaderNull
  , fisAI = True
  , fhasUI = False
  }

playerRobot = Player
  { fname = "Robot Anarchy"
  , fgroup = "robot"
  , fskillsLeader = robotSkills
  , fskillsOther  = robotSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , foverrideAI = Nothing
  , fentryLevel = 3
  , finitialActors = 3
  , fhasLeader = LeaderNull
  , fisAI = True
  , fhasUI = False
  }

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a duel game between two hero players land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
-- Actors that can be summoned should have "horror" in their @ifreq@ set.
playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , fskillsLeader = allSkills
  , fskillsOther  = allSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , foverrideAI = Nothing
  , fentryLevel = 3
  , finitialActors = 0
  , fhasLeader = LeaderNull
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
