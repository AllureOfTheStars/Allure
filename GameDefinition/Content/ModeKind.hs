-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game mode definitions.
module Content.ModeKind ( cdefs ) where

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
  , content =
      [campaign, duel, skirmish, ambush, battle, battleSurvival, safari, safariSurvival, pvp, coop, defense, screensaver]
  }
campaign,        duel, skirmish, ambush, battle, battleSurvival, safari, safariSurvival, pvp, coop, defense, screensaver :: ModeKind

campaign = ModeKind
  { msymbol = 'a'
  , mname   = "campaign"
  , mfreq   = [("campaign", 1)]
  , mroster = rosterCampaign
  , mcaves  = cavesCampaign
  , mdesc   = "You got stranded looting the blasted bridge of a once luxurious cruise liner. Your current plan is to fight through, gathering your spoils, to the shuttle airlock at the giant spaceship's uppermost deck. There are animal cries down below and ominous silence up above."
  }

duel = ModeKind
  { msymbol = 'u'
  , mname   = "duel"
  , mfreq   = [("duel", 1)]
  , mroster = rosterDuel
  , mcaves  = cavesSkirmish
  , mdesc   = "Let's settle the argument about this noble lady outside, in the woody biosphere behind the saloon."
  }

skirmish = ModeKind
  { msymbol = 'k'
  , mname   = "skirmish"
  , mfreq   = [("skirmish", 1)]
  , mroster = rosterSkirmish
  , mcaves  = cavesSkirmish
  , mdesc   = "You owe restorative surgery to one of our crew: if we win, we take all you have; if you win, you take our old giant spaceship (if you still want it when you see it)."
  }

ambush = ModeKind
  { msymbol = 'm'
  , mname   = "ambush"
  , mfreq   = [("ambush", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Conveniently, on the path to the Triton's spaceport, passengers can relax in a shady park."
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mdesc   = "Not even the unexplained ruin of the largest and tightest security Neptune's moon spaceport will prevent you from claiming your prize."
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
  , mdesc   = "In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent (exit at the uppermost level)."
  }

safariSurvival = ModeKind
  { msymbol = 'n'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

pvp = ModeKind
  { msymbol = 'v'
  , mname   = "PvP"
  , mfreq   = [("PvP", 1)]
  , mroster = rosterPvP
  , mcaves  = cavesSkirmish
  , mdesc   = "(Not usable right now.) This is a fight to the death between two human-controlled teams."
  }

coop = ModeKind
  { msymbol = 'o'
  , mname   = "Coop"
  , mfreq   = [("Coop", 1)]
  , mroster = rosterCoop
  , mcaves  = cavesCampaign
  , mdesc   = "(This mode is intended solely for automated testing.)"
  }

defense = ModeKind
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesCampaign
  , mdesc   = "Don't let the half-witted humans derail your operation and flee, like the puny, naked, tentacle-less beasts that they are!"
  }

screensaver = safari
  { mname   = "safari screensaver"
  , mfreq   = [("screensaver", 1), ("starting", 1)]
  , mroster = rosterSafari
      { rosterList = (head (rosterList rosterSafari))
                       -- changing leader by client needed, because of TFollow
                       {fleaderMode = LeaderAI $ AutoLeader True False}
                     : tail (rosterList rosterSafari)
      }
  }


rosterCampaign, rosterDuel, rosterSkirmish, rosterAmbush, rosterBattle, rosterBattleSurvival, rosterSafari, rosterSafariSurvival, rosterPvP, rosterCoop, rosterDefense :: Roster

rosterCampaign = Roster
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

rosterDuel = Roster
  { rosterList = [ playerHero { fname = "Spacefarer Crew"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = 4
                              , finitialActors = 1 }
                 , playerAntiHero { fname = "Red Collars"
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = 4
                                  , finitialActors = 1 }
                 , playerHorror ]
  , rosterEnemy = [ ("Spacefarer Crew", "Red Collars")
                  , ("Spacefarer Crew", "Horror Den")
                  , ("Red Collars", "Horror Den") ]
  , rosterAlly = [] }

rosterSkirmish = rosterDuel
  { rosterList = [ playerHero { fname = "Spacefarer Crew"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = 4 }
                 , playerAntiHero { fname = "Red Collars"
                                  , fhiCondPoly = hiDweller
                                  , fentryLevel = 4 }
                 , playerHorror ] }

rosterAmbush = rosterDuel
  { rosterList = [ playerSniper { fname = "Spacefarer Crew"
                                , fhiCondPoly = hiDweller
                                , fentryLevel = 7
                                , finitialActors = 4 }
                 , playerAntiSniper { fname = "Red Collars"
                                    , fhiCondPoly = hiDweller
                                    , fentryLevel = 7
                                    , finitialActors = 4 }
                 , playerHorror {fentryLevel = 7} ] }

rosterBattle = Roster
  { rosterList = [ playerSoldier { fhiCondPoly = hiDweller
                                 , fentryLevel = 7
                                 , finitialActors = 5 }
                 , playerMobileMonster { fentryLevel = 7
                                       , finitialActors = 35
                                       , fneverEmpty = True }
                 , playerMobileAnimal { fentryLevel = 7
                                      , finitialActors = 20
                                      , fneverEmpty = True }
                 , playerMobileRobot { fentryLevel = 7
                                     , finitialActors = 15
                                     , fneverEmpty = True } ]
  , rosterEnemy = [ ("Armed Spacefarer Crew", "Alien Hierarchy")
                  , ("Armed Spacefarer Crew", "Animal Kingdom")
                  , ("Armed Spacefarer Crew", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                , ("Alien Hierarchy", "Robot Anarchy")
                , ("Robot Anarchy", "Animal Kingdom") ] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ playerSoldier { fhiCondPoly = hiDweller
                                 , fentryLevel = -5
                                 , finitialActors = 5
                                 , fleaderMode = LeaderAI $ AutoLeader True True
                                 , fhasUI = False }
                 , playerMobileMonster { fentryLevel = 7
                                       , finitialActors = 35
                                       , fneverEmpty = True }
                 , playerMobileAnimal { fentryLevel = 7
                                      , finitialActors = 20
                                      , fneverEmpty = True }
                 , playerMobileRobot { fentryLevel = 7
                                     , finitialActors = 15
                                     , fneverEmpty = True
--                                     , fleaderMode =
--                                         LeaderUI $ AutoLeader False False
                                     , fhasUI = True } ] }

playerMonsterTourist, playerHunamConvict, playerAnimalMagnificent, playerAnimalExquisite :: Player Dice

playerMonsterTourist =
  playerAntiMonster { fname = "Alien Tourist Office"
                                     , fcanEscape = True
                                     , fneverEmpty = True  -- no spawning
                                     -- Follow-the-guide, as tourists do.
                                     , ftactic = TFollow
                                     , fentryLevel = 4
                                     , finitialActors = 15
                                     , fleaderMode =
                                         LeaderUI $ AutoLeader False False }

playerHunamConvict =
  playerCivilian { fname = "Hunam Convict Pack"
                 , fentryLevel = 4 }

playerAnimalMagnificent =
  playerMobileAnimal { fname =
                                          "Animal Magnificent Specimen Variety"
                                      , fneverEmpty = True
                                      , fentryLevel = 7
                                      , finitialActors = 10
                                      , fleaderMode =  -- move away from stairs
                                          LeaderAI $ AutoLeader True True }

playerAnimalExquisite =
  playerMobileAnimal { fname =
                                          "Animal Exquisite Herds and Packs"
                                      , fneverEmpty = True
                                      , fentryLevel = 10
                                      , finitialActors = 30 }

rosterSafari = Roster
  { rosterList = [ playerMonsterTourist
                 , playerHunamConvict
                 , playerAnimalMagnificent
                 , playerAnimalExquisite
                 ]
  , rosterEnemy = [ ("Alien Tourist Office", "Hunam Convict Pack")
                  , ("Alien Tourist Office",
                     "Animal Magnificent Specimen Variety")
                  , ("Alien Tourist Office",
                     "Animal Exquisite Herds and Packs") ]
  , rosterAlly = [( "Animal Magnificent Specimen Variety"
                  , "Animal Exquisite Herds and Packs" )] }

rosterSafariSurvival = rosterSafari
  { rosterList = [ playerMonsterTourist
                     { fleaderMode = LeaderAI $ AutoLeader True True
                     , fhasUI = False }
                 , playerHunamConvict
                 , playerAnimalMagnificent
                     { fleaderMode = LeaderUI $ AutoLeader False False
                     , fhasUI = True }
                 , playerAnimalExquisite
                 ] }

rosterPvP = Roster
  { rosterList = [ playerHero { fname = "Red"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = 4 }
                 , playerHero { fname = "Blue"
                              , fhiCondPoly = hiDweller
                              , fentryLevel = 4 }
                 , playerHorror ]
  , rosterEnemy = [ ("Red", "Blue")
                  , ("Red", "Horror Den")
                  , ("Blue", "Horror Den") ]
  , rosterAlly = [] }

rosterCoop = Roster
  { rosterList = [ playerAntiHero { fname = "Coral" }
                 , playerAntiHero { fname = "Amber"
                                  , fleaderMode = LeaderNull }
                 , playerAntiHero { fname = "Green" }
                 , playerAnimal { fhasUI = True }
                 , playerMonster { fname = "Alien Hierarchy"
                                 , finitialActors = 3 }
                 , playerMonster { fname = "Leaderless Alien Hierarchy"
                                 , finitialActors = 1
                                 , fleaderMode = LeaderNull }
                 , playerRobot ]
  , rosterEnemy = [ ("Coral", "Alien Hierarchy")
                  , ("Amber", "Alien Hierarchy")
                  , ("Animal Kingdom", "Leaderless Alien Hierarchy") ]
  , rosterAlly = [ ("Coral", "Amber")
                 , ("Coral", "Green")
                 , ("Amber", "Green")
                 , ("Green", "Animal Kingdom")
                 , ("Green", "Robot Anarchy")
                 , ("Green", "Alien Hierarchy")
                 , ("Green", "Leaderless Alien Hierarchy") ] }

rosterDefense = Roster
  { rosterList = [ playerAntiHero
                 , playerAntiMonster
                 , playerAnimal
                 , playerRobot ]
  , rosterEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                  , ("Spacefarer Crew", "Animal Kingdom")
                  , ("Spacefarer Crew", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

cavesCampaign, cavesSkirmish, cavesAmbush, cavesBattle, cavesSafari :: Caves

cavesCampaign = IM.fromList
                $ [(1, ("shallow random 1", Nothing))]
                  ++ [(2, ("shallow random 2", Nothing))]
                  ++ [(3, ("caveBridge", Nothing))]
                  ++ [(4, ("caveNoise", Nothing))]
                  ++ zip [5..9] (repeat ("campaign random", Nothing))
                  ++ [(10, ("caveEmpty", Nothing))]
                  ++ [(11, ("campaign random", Nothing))]
                  ++ [(12, ("caveNoise", Just True))]

cavesSkirmish = IM.fromList [(4, ("caveSkirmish", Nothing))]

cavesAmbush = IM.fromList [(7, ("caveAmbush", Nothing))]

cavesBattle = IM.fromList [(7, ("caveBattle", Nothing))]

cavesSafari = IM.fromList [ (4, ("caveSafari1", Nothing))
                          , (7, ("caveSafari2", Nothing))
                          , (10, ("caveSafari3", Just False)) ]
