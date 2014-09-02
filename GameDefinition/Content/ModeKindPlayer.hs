-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Basic players definitions.
module Content.ModeKindPlayer
  ( playerHero, playerSoldier, playerAntiHero, playerCivilian, playerMonster
  , playerMobileMonster, playerAntiMonster, playerAnimal, playerMobileAnimal
  , playerRobot, playerMobileRobot, playerHorror
  ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Content.ModeKind

playerHero, playerSoldier, playerAntiHero, playerCivilian, playerMonster, playerMobileMonster, playerAntiMonster, playerAnimal, playerMobileAnimal, playerRobot, playerMobileRobot, playerHorror :: Player

playerHero = Player
  { fname = "Spacefarer Crew"
  , fgroup = "hero"
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhasNumbers = True
  , fhasGender = True
  , ftactic = TExplore
  , fentryLevel = 3
  , finitialActors = 3
  , fleaderMode = LeaderUI $ AutoLeader False False
  , fhasUI = True
  }

playerSoldier = playerHero
  { fname = "Armed Spacefarer Crew"
  , fgroup = "soldier"
  }

playerAntiHero = playerHero
  { fleaderMode = LeaderAI $ AutoLeader True False
  , fhasUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , fskillsOther = unitSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhasNumbers = False
  , fhasGender = True
  , ftactic = TPatrol
  , fentryLevel = 1
  , finitialActors = 3
  , fleaderMode = LeaderNull  -- unorganized
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Alien Hierarchy"
  , fgroup = "alien"
  , fskillsOther = unitSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TExplore
  , fentryLevel = 4
  , finitialActors = 0
  , fleaderMode = LeaderAI $ AutoLeader True True
  , fhasUI = False
  }

playerMobileMonster = playerMonster

playerAntiMonster = playerMonster
  { fhasUI = True
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsOther = animalSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TRoam  -- can't pick up, so no point exploring
  , fentryLevel = 3
  , finitialActors = 2
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

playerMobileAnimal = playerAnimal
  { fgroup = "mobile animal" }

playerRobot = Player
  { fname = "Robot Anarchy"
  , fgroup = "robot"
  , fskillsOther = robotSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TFollow  -- coordinated via net, follow alien leader (TODO)
  , fentryLevel = 3
  , finitialActors = 3
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

playerMobileRobot = playerRobot
  { fgroup = "mobile robot" }

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a duel game between two hero players land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
-- Actors that can be summoned should have "horror" in their @ifreq@ set.
playerHorror = Player
  { fname = "Horror Den"
  , fgroup = "horror"
  , fskillsOther = unitSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TPatrol  -- disoriented
  , fentryLevel = 3
  , finitialActors = 0
  , fleaderMode = LeaderNull
  , fhasUI = False
  }


meleeAdjacent, _meleeAndRanged, animalSkills, robotSkills :: Skills

meleeAdjacent = EM.fromList $ zip [AbWait, AbMelee] [1, 1..]

-- Melee and reaction fire.
_meleeAndRanged = EM.fromList $ zip [AbWait, AbMelee, AbProject] [1, 1..]

animalSkills =
  EM.fromList $ zip [AbMove, AbMelee, AbAlter, AbWait, AbTrigger] [1, 1..]

robotSkills = EM.delete AbApply unitSkills
