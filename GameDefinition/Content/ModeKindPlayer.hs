-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Basic players definitions.
module Content.ModeKindPlayer
  ( playerHero, playerSoldier, playerSniper
  , playerAntiHero, playerAntiSniper, playerCivilian
  , playerMonster, playerMobileMonster, playerAntiMonster
  , playerAnimal, playerMobileAnimal
  , playerRobot, playerMobileRobot
  , playerHorror
  , hiHero, hiDweller
  ) where

import qualified Data.EnumMap.Strict as EM
import Data.List

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ModeKind

playerHero, playerSoldier, playerSniper, playerAntiHero, playerAntiSniper, playerCivilian, playerMonster, playerMobileMonster, playerAntiMonster, playerAnimal, playerMobileAnimal, playerRobot, playerMobileRobot, playerHorror :: Player Dice

playerHero = Player
  { fname = "Spacefarer Crew"
  , fgroup = "hero"
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhiCondPoly = hiHero
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

playerSniper = playerHero
  { fname = "Sniper Adventurer Party"
  , fgroup = "sniper"
  }

playerAntiHero = playerHero
  { fleaderMode = LeaderAI $ AutoLeader True False
  , fhasUI = False
  }

playerAntiSniper = playerSniper
  { fleaderMode = LeaderAI $ AutoLeader True False
  , fhasUI = False
  }

playerCivilian = Player
  { fname = "Civilian Crowd"
  , fgroup = "civilian"
  , fskillsOther = zeroSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhiCondPoly = []
  , fhasNumbers = False
  , fhasGender = True
  , ftactic = TPatrol
  , fentryLevel = 1
  , finitialActors = d 2 + 1
  , fleaderMode = LeaderNull  -- unorganized
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Alien Hierarchy"
  , fgroup = "alien"
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TExplore
  , fentryLevel = 4
  , finitialActors = 0
  , fleaderMode =  -- no point changing leader on level, since all move
      LeaderAI $ AutoLeader True True
  , fhasUI = False
  }

playerMobileMonster = playerMonster

playerAntiMonster = playerMonster
  { fhasUI = True
  , fleaderMode = LeaderUI $ AutoLeader True True
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroup = "animal"
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TRoam  -- can't pick up, so no point exploring
  , fentryLevel = 3
  , finitialActors = 1 + d 2
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

playerMobileAnimal = playerAnimal
  { fgroup = "mobile animal" }

playerRobot = Player
  { fname = "Robot Anarchy"
  , fgroup = "robot"
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TFollow  -- coordinated via net, follow alien leader (TODO)
  , fentryLevel = 3
  , finitialActors = 1 + d 2
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
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasNumbers = False
  , fhasGender = False
  , ftactic = TPatrol  -- disoriented
  , fentryLevel = 4
  , finitialActors = 0
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

victoryOutcomes :: [Outcome]
victoryOutcomes = [Conquer, Escape]

hiHero, hiDweller :: HiCondPoly

-- Heroes rejoice in loot.
hiHero = [ ( [(HiLoot, 1)]
           , [minBound..maxBound] )
         , ( [(HiConst, 1000), (HiLoss, -100)]
           , victoryOutcomes )
         ]

-- Spawners or skirmishers get no points from loot, but try to kill
-- all opponents fast or at least hold up for long.
hiDweller = [ ( [(HiConst, 1000)]  -- no loot
              , victoryOutcomes )
            , ( [(HiConst, 1000), (HiLoss, -10)]
              , victoryOutcomes )
            , ( [(HiBlitz, -100)]
              , victoryOutcomes )
            , ( [(HiSurvival, 100)]
              , [minBound..maxBound] \\ victoryOutcomes )
            ]

minusTen, meleeAdjacent, _meleeAndRanged :: Skills

-- To make sure weak items can't override move-only-leader, etc.
minusTen = EM.fromList $ zip [minBound..maxBound] [-10, -10..]

meleeAdjacent = EM.delete AbWait $ EM.delete AbMelee minusTen

-- Melee and reaction fire.
_meleeAndRanged = EM.delete AbProject $ meleeAdjacent
