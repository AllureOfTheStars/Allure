-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game modes for Allure of the Stars.
module Content.ModeKind ( cdefs ) where

import qualified Data.IntMap.Strict as IM

import Content.ModeKindPlayer
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.ModeKind

cdefs :: ContentDef ModeKind
cdefs = ContentDef
  { getSymbol = msymbol
  , getName = mname
  , getFreq = mfreq
  , validate = validateModeKind
  , content =
      [campaign, duel, skirmish, ambush, battle, safari, pvp, coop, defense]
  }
campaign,        duel, skirmish, ambush, battle, safari, pvp, coop, defense :: ModeKind

campaign = ModeKind
  { msymbol = 'a'
  , mname   = "campaign"
  , mfreq   = [("campaign", 1)]
  , mroster = rosterCampaign
  , mcaves  = cavesCampaign
  , mdesc   = "You got stranded looting a once luxurious cruise liner and your current plan is to fight through, gathering your spoils, to the bridge at the giant spaceship's opposite end."
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
  , mdesc   = "You owe restorative surgery to one of our crew: if we win, we take all you have; if you win, you take ours old giant spaceship (if you still want it when you see it)."
  }

ambush = ModeKind
  { msymbol = 'm'
  , mname   = "ambush"
  , mfreq   = [("ambush", 1)]
  , mroster = rosterSkirmish
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

safari = ModeKind
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent (exit at the bottommost level)."
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


rosterCampaign, rosterDuel, rosterSkirmish, rosterBattle, rosterSafari, rosterPvP, rosterCoop, rosterDefense :: Roster

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
                              , finitialActors = 1 }
                 , playerAntiHero { fname = "Red Collars"
                                  , finitialActors = 1 }
                 , playerHorror ]
  , rosterEnemy = [ ("Spacefarer Crew", "Red Collars")
                  , ("Spacefarer Crew", "Horror Den")
                  , ("Red Collars", "Horror Den") ]
  , rosterAlly = [] }

rosterSkirmish = rosterDuel
  { rosterList = [ playerHero {fname = "Spacefarer Crew"}
                 , playerAntiHero {fname = "Red Collars"}
                 , playerHorror ] }

rosterBattle = Roster
  { rosterList = [ playerHero {finitialActors = 5}
                 , playerMonster { finitialActors = 15
                                 , fneverEmpty = True }
                 , playerAnimal { finitialActors = 5
                                , fneverEmpty = True }
                 , playerRobot { finitialActors = 5
                               , fneverEmpty = True } ]
  , rosterEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                  , ("Spacefarer Crew", "Animal Kingdom")
                  , ("Spacefarer Crew", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                , ("Alien Hierarchy", "Robot Anarchy")
                , ("Robot Anarchy", "Animal Kingdom") ] }

rosterSafari = Roster
  { rosterList = [ playerAntiMonster { fname = "Alien Tourist Office"
                                     , fcanEscape = True
                                     , fneverEmpty = True
                                     -- Follow-the-leader, as tourists do.
                                     , foverrideAI = Just ()
                                     , fentryLevel = 4
                                     , finitialActors = 15
                                     , fhasLeader = LeaderMode False False }
                 , playerCivilian { fname = "Hunam Convict Pack"
                                  , fentryLevel = 4 }
                 , playerAnimal { fname =
                                    "Animal Magnificent Specimen Variety"
                                , fneverEmpty = True
                                , fentryLevel = 7
                                , finitialActors = 7 }
                 , playerAnimal { fname =
                                    "Animal Exquisite Herds and Packs"
                                , fneverEmpty = True
                                , fentryLevel = 10
                                , finitialActors = 20 } ]
  , rosterEnemy = [ ("Alien Tourist Office", "Hunam Convict Pack")
                  , ("Alien Tourist Office",
                     "Animal Magnificent Specimen Variety")
                  , ("Alien Tourist Office",
                     "Animal Exquisite Herds and Packs") ]
  , rosterAlly = [( "Animal Magnificent Specimen Variety"
                  , "Animal Exquisite Herds and Packs" )] }

rosterPvP = Roster
  { rosterList = [ playerHero {fname = "Red"}
                 , playerHero {fname = "Blue"}
                 , playerHorror ]
  , rosterEnemy = [ ("Red", "Blue")
                  , ("Red", "Horror Den")
                  , ("Blue", "Horror Den") ]
  , rosterAlly = [] }

rosterCoop = Roster
  { rosterList = [ playerAntiHero { fname = "Coral" }
                 , playerAntiHero { fname = "Amber"
                                  , fhasLeader = LeaderNull }
                 , playerAntiHero { fname = "Green" }
                 , playerAnimal { fhasUI = True }
                 , playerMonster { fname = "Alien Hierarchy" }
                 , playerMonster { fname = "Leaderless Alien Hierarchy"
                                 , fhasLeader = LeaderNull }
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
  { rosterList = [ playerAntiMonster { finitialActors = 1 }
                 , playerAntiHero { fname = "Yellow"
                                  , finitialActors = 10 }
                 , playerAnimal
                 , playerRobot ]
  , rosterEnemy = [ ("Yellow", "Alien Hierarchy")
                  , ("Yellow", "Animal Kingdom")
                  , ("Yellow", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }


cavesCampaign, cavesSkirmish, cavesAmbush, cavesBattle, cavesSafari :: Caves

cavesCampaign = IM.fromList $ [(1, ("caveRogue", Nothing))]
                              ++ zip [2..11] (repeat ("dng", Nothing))
                              ++ [(12, ("caveNoise", Just True))]

cavesSkirmish = IM.fromList [(3, ("caveSkirmish", Nothing))]

cavesAmbush = IM.fromList [(5, ("caveAmbush", Nothing))]

cavesBattle = IM.fromList [(3, ("caveBattle", Nothing))]

cavesSafari = IM.fromList [ (4, ("caveSafari1", Nothing))
                          , (7, ("caveSafari2", Nothing))
                          , (10, ("caveSafari3", Just False)) ]
