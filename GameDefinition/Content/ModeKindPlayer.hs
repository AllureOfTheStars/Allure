-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
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
  , hiHero, hiDweller, hiRaid, hiEscapist
  , playerRobot
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Faction
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ModeKind

playerHero, playerAntiHero, playerCivilian, playerMonster, playerAntiMonster, playerAnimal, playerHorror, playerMonsterTourist, playerHunamConvict, playerAnimalMagnificent, playerAnimalExquisite :: Player
playerRobot :: Player

playerHero = Player
  { fname = "Spacefarer"
  , fgroups = ["hero"]
  , fskillsOther = meleeAdjacent
  , fcanEscape = True
  , fneverEmpty = True
  , fhiCondPoly = hiHero
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
  , fhiCondPoly = hiDweller
  , fhasGender = True
  , ftactic = TPatrol
  , fleaderMode = LeaderNull  -- unorganized
  , fhasUI = False
  }

playerMonster = Player
  { fname = "Alien Hierarchy"
  , fgroups = ["monster", "mobile monster", "immobile monster"]
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
  { fhasUI = True
  , fleaderMode = LeaderUI $ AutoLeader True True
  }

playerAnimal = Player
  { fname = "Animal Kingdom"
  , fgroups = ["animal", "mobile animal", "immobile animal", "scavenger"]
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
  , fgroups = [nameOfHorrorFact]
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
                    , fhiCondPoly = hiEscapist
                    , ftactic = TFollow  -- follow-the-guide, as tourists do
                    , fleaderMode = LeaderUI $ AutoLeader False False }

playerHunamConvict =
  playerCivilian {fname = "Hunam Convict"}

playerAnimalMagnificent =
  playerAnimal { fname = "Animal Magnificent Specimen Variety"
               , fneverEmpty = True
               , fleaderMode = -- False to move away from stairs
                               LeaderAI $ AutoLeader True False }

playerAnimalExquisite =
  playerAnimal { fname = "Animal Exquisite Herds and Packs"
               , fneverEmpty = True }

victoryOutcomes :: [Outcome]
victoryOutcomes = [Conquer, Escape]

hiHero, hiRaid, hiDweller, hiEscapist :: HiCondPoly

-- Heroes rejoice in loot.
hiHero = [ ( [(HiLoot, 1)]
           , [minBound..maxBound] )
         , ( [(HiConst, 1000), (HiLoss, -1)]
           , victoryOutcomes )
         ]

hiRaid = [ ( [(HiLoot, 1)]
           , [minBound..maxBound] )
         , ( [(HiConst, 100)]
           , victoryOutcomes )
         ]

-- Spawners or skirmishers get no points from loot, but try to kill
-- all opponents fast or at least hold up for long.
hiDweller = [ ( [(HiConst, 1000)]  -- no loot, so big win reward
              , victoryOutcomes )
            , ( [(HiConst, 1000), (HiLoss, -10)]
              , victoryOutcomes )
            , ( [(HiBlitz, -100)]  -- speed matters
              , victoryOutcomes )
            , ( [(HiSurvival, 100)]
              , [minBound..maxBound] \\ victoryOutcomes )
            ]

hiEscapist = ( [(HiLoot, 1)]  -- loot matters a little bit
             , [minBound..maxBound] )
             : hiDweller

playerRobot = Player
  { fname = "Robot Anarchy"
  , fgroups = ["robot", "mobile monster", "immobile monster"]
  , fskillsOther = zeroSkills
  , fcanEscape = False
  , fneverEmpty = False
  , fhiCondPoly = hiDweller
  , fhasGender = False
  , ftactic = TRoam  -- TODO:TFollow -- coordinated via net, follow alien leader
  , fleaderMode = LeaderNull
  , fhasUI = False
  }
