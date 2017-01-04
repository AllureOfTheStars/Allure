-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game mode definitions.
module Content.ModeKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.IntMap.Strict as IM

import Content.ModeKindPlayer
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ModeKind

cdefs :: ContentDef ModeKind
cdefs = ContentDef
  { getSymbol = msymbol
  , getName = mname
  , getFreq = mfreq
  , validateSingle = validateSingleModeKind
  , validateAll = validateAllModeKind
  , content = contentFromList
      [raid, brawl, ambush, battle, exploration, battleSurvival, safari, safariSurvival, defense, screensaverSafari, screensaverRaid, screensaverBrawl, screensaverAmbush]
  }
raid,        brawl, ambush, battle, exploration, battleSurvival, safari, safariSurvival, defense, screensaverSafari, screensaverRaid, screensaverBrawl, screensaverAmbush :: ModeKind

raid = ModeKind
  { msymbol = 'r'
  , mname   = "raid"
  , mfreq   = [("raid", 1), ("campaign scenario", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mdesc   = "The Triton City sewers need purging. The first person to break through to the other end will be paid 100 gold grains. Please don't fight each other."
  }

brawl = ModeKind
  { msymbol = 'k'
  , mname   = "brawl"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mdesc   = "\"You cheated. Come alone to the woody biosphere behind the saloon at noon, if you dare. The winner takes all the spoils, including the keys and the papers of the decrepit giant spaceship.\""
  }

ambush = ModeKind
  { msymbol = 'm'
  , mname   = "ambush"
  , mfreq   = [("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Conveniently, on the path to the Triton's spaceport, passengers can relax in a shady park."
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1), ("campaign scenario", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mdesc   = "Not even the unexplained ruin of the largest and tightest security Neptune's moon spaceport will prevent you from claiming your prize."
  }

exploration = ModeKind
  { msymbol = 'c'
  , mname   = "exploration"
  , mfreq   = [("exploration", 1), ("campaign scenario", 1)]
  , mroster = rosterExploration
  , mcaves  = cavesExploration
  , mdesc   = "You got stranded looting the blasted bridge of a once luxurious cruise liner. Your current plan is to fight through, gathering your spoils, to the shuttle airlock somewhere among the giant spaceship's uppermost decks. There are animal cries down below and ominous silence up above."
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

safari = ModeKind
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "\"In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the topmost level.\" This is VR recording recovered from an alien nest debris."
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

defense = ModeKind
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesExploration
  , mdesc   = "Don't let the half-witted humans derail your operation and flee, like the puny, naked, tentacle-less beasts that they are!"
  }

screensaverSafari = safari
  { mname   = "auto-safari"
  , mfreq   = [("starting", 1), ("no confirms", 1)]
  , mroster = rosterSafari
      { rosterList = (head (rosterList rosterSafari))
                       -- changing leader by client needed, because of TFollow
                       {fleaderMode = LeaderAI $ AutoLeader False True}
                     : tail (rosterList rosterSafari)
      }
  }

screensaverRaid = raid
  { mname   = "auto-raid"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = rosterRaid
      { rosterList = (head (rosterList rosterRaid))
                       {fleaderMode = LeaderAI $ AutoLeader False False}
                     : tail (rosterList rosterRaid)
      }
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = rosterBrawl
      { rosterList = (head (rosterList rosterBrawl))
                       {fleaderMode = LeaderAI $ AutoLeader False False}
                     : tail (rosterList rosterBrawl)
      }
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = rosterAmbush
      { rosterList = (head (rosterList rosterAmbush))
                       {fleaderMode = LeaderAI $ AutoLeader False False}
                     : tail (rosterList rosterAmbush)
      }
  }


rosterRaid, rosterBrawl, rosterAmbush, rosterBattle, rosterExploration, rosterBattleSurvival, rosterSafari, rosterSafariSurvival, rosterDefense :: Roster

rosterRaid = Roster
  { rosterList = [ playerHero { fhiCondPoly = hiRaid
                              , fentryLevel = 4
                              , finitialActors = 1 }
                 , playerAntiHero { fname = "Red Collars"
                                  , fhiCondPoly = hiRaid
                                  , fentryLevel = 4
                                  , finitialActors = 1 }
                 , playerAnimal { fentryLevel = 4
                                , finitialActors = 1 }
                 , playerRobot { fentryLevel = 4
                               , finitialActors = 1 } ]
  , rosterEnemy = [ ("Spacefarer Crew", "Animal Kingdom")
                  , ("Spacefarer Crew", "Robot Anarchy")
                  , ("Red Collars", "Animal Kingdom")
                  , ("Red Collars", "Robot Anarchy") ]
  , rosterAlly = [("Robot Anarchy", "Animal Kingdom")] }

rosterBrawl = Roster
  { rosterList = [ playerHero { fcanEscape = False
                              , fhiCondPoly = hiDweller
                              , fentryLevel = -4 }
                 , playerAntiHero { fname = "Red Collars"
                                  , fcanEscape = False
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = -4 }
                 , playerHorror ]
  , rosterEnemy = [ ("Spacefarer Crew", "Red Collars")
                  , ("Spacefarer Crew", "Horror Den")
                  , ("Red Collars", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ playerSniper { fcanEscape = False
                                , fhiCondPoly = hiDweller
                                , fentryLevel = -7
                                , finitialActors = 4 }
                 , playerAntiSniper { fname = "Red Collars"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller
                                    , fentryLevel = -7
                                    , finitialActors = 4 }
                 , playerHorror {fentryLevel = -7} ]
  , rosterEnemy = [ ("Spacefarer Crew", "Red Collars")
                  , ("Spacefarer Crew", "Horror Den")
                  , ("Red Collars", "Horror Den") ]
  , rosterAlly = [] }

rosterBattle = Roster
  { rosterList = [ playerSoldier { fcanEscape = False
                                 , fhiCondPoly = hiDweller
                                 , fentryLevel = -7
                                 , finitialActors = 5 }
                 , playerMobileMonster { fentryLevel = -7
                                       , finitialActors = 35
                                       , fneverEmpty = True }
                 , playerMobileAnimal { fentryLevel = -7
                                      , finitialActors = 20
                                      , fneverEmpty = True }
                 , playerMobileRobot { fentryLevel = -7
                                     , finitialActors = 15
                                     , fneverEmpty = True } ]
  , rosterEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                  , ("Spacefarer Crew", "Animal Kingdom")
                  , ("Spacefarer Crew", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                , ("Alien Hierarchy", "Robot Anarchy")
                , ("Robot Anarchy", "Animal Kingdom") ] }

rosterExploration = Roster
  { rosterList = [ playerHero
                 , playerMonster
                 , playerAnimal
                 , playerRobot ]
  , rosterEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                  , ("Spacefarer Crew", "Animal Kingdom")
                  , ("Spacefarer Crew", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ playerSoldier { fcanEscape = False
                                 , fhiCondPoly = hiDweller
                                 , fentryLevel = -7
                                 , finitialActors = 5
                                 , fleaderMode =
                                     LeaderAI $ AutoLeader False False
                                 , fhasUI = False }
                 , playerMobileMonster { fentryLevel = -7
                                       , finitialActors = 35
                                       , fneverEmpty = True }
                 , playerMobileAnimal { fentryLevel = -7
                                      , finitialActors = 20
                                      , fneverEmpty = True }
                 , playerMobileRobot { fentryLevel = -7
                                     , finitialActors = 15
                                     , fneverEmpty = True
                                     , fhasUI = True } ] }

playerMonsterTourist, playerHunamConvict, playerAnimalMagnificent, playerAnimalExquisite :: Player Dice

playerMonsterTourist =
  playerAntiMonster { fname = "Alien Tourist Office"
                    , fcanEscape = True
                    , fneverEmpty = True  -- no spawning
                      -- Follow-the-guide, as tourists do.
                    , ftactic = TFollow
                    , fentryLevel = -4
                    , finitialActors = 15
                    , fleaderMode =
                        LeaderUI $ AutoLeader False False }

playerHunamConvict =
  playerCivilian { fname = "Hunam Convict Pack"
                 , fentryLevel = -4 }

playerAnimalMagnificent =
  playerMobileAnimal { fname = "Animal Magnificent Specimen Variety"
                     , fneverEmpty = True
                     , fentryLevel = -7
                     , finitialActors = 10
                     , fleaderMode =  -- False to move away from stairs
                         LeaderAI $ AutoLeader True False }

playerAnimalExquisite =
  playerMobileAnimal { fname = "Animal Exquisite Herds and Packs"
                     , fneverEmpty = True
                     , fentryLevel = -10
                     , finitialActors = 30 }

rosterSafari = Roster
  { rosterList = [ playerMonsterTourist
                 , playerHunamConvict
                 , playerAnimalMagnificent
                 , playerAnimalExquisite
                 ]
  , rosterEnemy = [ ( "Alien Tourist Office", "Hunam Convict Pack")
                  , ( "Alien Tourist Office"
                    , "Animal Magnificent Specimen Variety" )
                  , ( "Alien Tourist Office"
                    , "Animal Exquisite Herds and Packs" ) ]
  , rosterAlly = [ ( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs" )
                 , ( "Animal Magnificent Specimen Variety"
                   , "Hunam Convict Pack" )
                 , ( "Hunam Convict Pack"
                   , "Animal Exquisite Herds and Packs" ) ] }

rosterSafariSurvival = rosterSafari
  { rosterList = [ playerMonsterTourist
                     { fleaderMode = LeaderAI $ AutoLeader True True
                     , fhasUI = False }
                 , playerHunamConvict
                 , playerAnimalMagnificent
                     { fleaderMode = LeaderUI $ AutoLeader True False
                     , fhasUI = True }
                 , playerAnimalExquisite
                 ] }

rosterDefense = rosterExploration
  { rosterList = [ playerAntiHero
                 , playerAntiMonster {finitialActors = 1}  -- hack, for travis
                 , playerAnimal
                 , playerRobot ] }

cavesRaid, cavesBrawl, cavesAmbush, cavesBattle, cavesExploration, cavesSafari :: Caves

cavesRaid = IM.fromList [(4, "caveRogueLit")]

cavesBrawl = IM.fromList [(-4, "caveBrawl")]

cavesAmbush = IM.fromList [(-7, "caveAmbush")]

cavesBattle = IM.fromList [(-7, "caveBattle")]

cavesExploration = IM.fromList $
  [(1, "shallow random 1")]
  ++ [(2, "shallow random 2")]
  ++ [(3, "caveBridge")]
  ++ [(4, "caveNoise")]
  ++ zip [5..9] (repeat "default random")
  ++ [(10, "caveEmptyExit")]
  ++ [(11, "default random")]
  ++ [(12, "caveNoise")]

cavesSafari = IM.fromList [ (-4, "caveSafari1")
                          , (-7, "caveSafari2")
                          , (-10, "caveSafari3") ]
