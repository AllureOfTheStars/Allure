-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Basic players definitions.
module Content.ModeKindPlayer
  ( playerHero, playerAntiHero, playerCivilian
  , playerMonster, playerAntiMonster, playerAnimal
  , playerHorror, playerMonsterTourist, playerHunamConvict
  , playerAnimalMagnificent, playerAnimalExquisite
  , hiHeroShort, hiHeroMedium, hiHeroLong, hiDweller
  , playerRobot
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Definition.Ability

playerHero, playerAntiHero, playerCivilian, playerMonster, playerAntiMonster, playerAnimal, playerHorror, playerMonsterTourist, playerHunamConvict, playerAnimalMagnificent, playerAnimalExquisite :: Player
playerRobot :: Player

playerHero = Player
  { fname = "Spacefarer"
  , fgroups = ["hero"]
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhiCondPoly = hiHeroLong
  , fhasGender = True
  , ftactic = TExplore
  , fleaderMode = LeaderUI $ AutoLeader False False
  , fhasUI = True
  }

playerAntiHero = playerHero
  { fleaderMode = LeaderAI $ AutoLeader True False
  , fhasUI = False
  }

playerCivilian = Player
  { fname = "Civilian"
  , fgroups = ["hero", "civilian"]
  , fskillsOther = zeroSkills  -- not coordinated by any leadership
  , fcanEscape = False
  , fneverEmpty = True
  , fhiCondPoly = hiHeroMedium
  , fhasGender = True
  , ftactic = TPatrol
  , fleaderMode = LeaderNull  -- unorganized
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Alien Hierarchy"
  , fgroups = ["monster", "mobile monster", "aquatic monster"]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , ftactic = TExplore
  , fleaderMode =
      -- No point changing leader on level, since all move and they
      -- don't follow the leader.
      LeaderAI $ AutoLeader True True
  , fhasUI = False
  }

playerAntiMonster = playerMonster
  { fleaderMode = LeaderUI $ AutoLeader True True
  , fhasUI = True
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroups = [ "animal", "mobile animal", "immobile animal", "aquatic animal"
              , "scavenger" ]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , ftactic = TRoam  -- can't pick up, so no point exploring
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

-- | A special player, for summoned actors that don't belong to any
-- of the main players of a given game. E.g., animals summoned during
-- a brawl game between two hero factions land in the horror faction.
-- In every game, either all factions for which summoning items exist
-- should be present or a horror player should be added to host them.
playerHorror = Player
  { fname = "Horror Den"
  , fgroups = [horrorGroup]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = []
  , fhasGender = False
  , ftactic = TPatrol  -- disoriented
  , fleaderMode = LeaderNull
  , fhasUI = False
  }

playerMonsterTourist =
  playerAntiMonster { fname = "Alien Tourist Office"
                    , fcanEscape = True
                    , fneverEmpty = True  -- no spawning
                    , fhiCondPoly = hiHeroMedium
                    , ftactic = TFollow  -- follow-the-guide, as tourists do
                    , fleaderMode = LeaderUI $ AutoLeader False False }

playerHunamConvict =
  playerCivilian { fname = "Hunam Convict"
                 , fleaderMode = LeaderAI $ AutoLeader True False }

playerAnimalMagnificent =
  playerAnimal { fname = "Animal Magnificent Specimen Variety"
               , fneverEmpty = True }

playerAnimalExquisite =
  playerAnimal { fname = "Animal Exquisite Herds and Packs Galore"
               , fneverEmpty = True }

victoryOutcomes :: [Outcome]
victoryOutcomes = [Conquer, Escape]

hiHeroLong, hiHeroMedium, hiHeroShort, hiDweller :: HiCondPoly

hiHeroShort =
  [ ( [(HiLoot, 100)]
    , [minBound..maxBound] )
  , ( [(HiConst, 100)]
    , victoryOutcomes )
  , ( [(HiSprint, -500)]  -- speed matters, but only if fast enough
    , victoryOutcomes )
  , ( [(HiSurvival, 10)]  -- few points for surviving long
    , [minBound..maxBound] \\ victoryOutcomes )
  ]

hiHeroMedium =
  [ ( [(HiLoot, 200)]  -- usually no loot, but if so, no harm
    , [minBound..maxBound] )
  , ( [(HiConst, 200), (HiLoss, -10)]
    , victoryOutcomes )
  , ( [(HiSprint, -500)]  -- speed matters, but only if fast enough
    , victoryOutcomes )
  , ( [(HiBlitz, -100)]  -- speed matters always
    , victoryOutcomes )
  , ( [(HiSurvival, 10)]  -- few points for surviving long
    , [minBound..maxBound] \\ victoryOutcomes )
  ]

-- Heroes in long crawls rejoice in loot.
hiHeroLong =
  [ ( [(HiLoot, 10000)]  -- multiplied by fraction of collected
    , [minBound..maxBound] )
  , ( [(HiSprint, -20000)]  -- speedrun bonus, if below this number of turns
    , victoryOutcomes )
  , ( [(HiBlitz, -100)]  -- speed matters always
    , victoryOutcomes )
  , ( [(HiSurvival, 10)]  -- few points for surviving long
    , [minBound..maxBound] \\ victoryOutcomes )
  ]

-- Spawners get no points from loot, but try to kill
-- all opponents fast or at least hold up for long.
hiDweller = [ ( [(HiConst, 1000)]  -- no loot, so big win reward
              , victoryOutcomes )
            , ( [(HiConst, 1000), (HiLoss, -10)]
              , victoryOutcomes )
            , ( [(HiSprint, -1000)]  -- speedrun bonus, if below
              , victoryOutcomes )
            , ( [(HiBlitz, -100)]  -- speed matters
              , victoryOutcomes )
            , ( [(HiSurvival, 100)]
              , [minBound..maxBound] \\ victoryOutcomes )
            ]

-- Allure-specific

playerRobot = Player
  { fname = "Robot Anarchy"
  , fgroups = [ "robot", "mobile robot", "immobile robot" --, "aquatic robot"
              , "construction robot" ]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , ftactic = TRoam  -- TODO:TFollow -- coordinated via net, follow alien leader
  , fleaderMode = LeaderNull
  , fhasUI = False
  }
