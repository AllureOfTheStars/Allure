-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game modes for Allure of the Stars.
module Content.ModeKind ( cdefs ) where

import qualified Data.EnumMap.Strict as EM

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
  { msymbol  = 'a'
  , mname    = "campaign"
  , mfreq    = [("campaign", 1)]
  , mplayers = playersCampaign
  , mcaves   = cavesCampaign
  , mdesc    = "You are stranded on a once luxurious cruise liner and your plan is to fight through to the bridge at the spacehip's opposite end, gathering spoils along the way."
  }

duel = ModeKind
  { msymbol  = 'u'
  , mname    = "duel"
  , mfreq    = [("duel", 1)]
  , mplayers = playersDuel
  , mcaves   = cavesSkirmish
  , mdesc    = "Let's settle the argument about the lady in the woody biosphere behind the saloon."
  }

skirmish = ModeKind
  { msymbol  = 'k'
  , mname    = "skirmish"
  , mfreq    = [("skirmish", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesSkirmish
  , mdesc    = "You owe restorative surgery to one of our crew: if we win, we take your little spaceship; if you win, you take ours (if you still want it)."
  }

ambush = ModeKind
  { msymbol  = 'm'
  , mname    = "ambush"
  , mfreq    = [("ambush", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesAmbush
  , mdesc    = "Conveniently, on the path to the Triton's spaceport, passengers can relax in a shady park."
  }

battle = ModeKind
  { msymbol  = 'b'
  , mname    = "battle"
  , mfreq    = [("battle", 1)]
  , mplayers = playersBattle
  , mcaves   = cavesBattle
  , mdesc    = "Not even the unexplained ruin of the largest and tightest security Neptune's moon spaceport will prevent you from claiming your prize."
  }

safari = ModeKind
  { msymbol  = 'f'
  , mname    = "safari"
  , mfreq    = [("safari", 1)]
  , mplayers = playersSafari
  , mcaves   = cavesSafari
  , mdesc    = "In this simulation you'll discover the joys of hunting the most exquisite of Earth's fauna, both animal and semi-intelligent (exit at the opposite level)."
  }

pvp = ModeKind
  { msymbol  = 'v'
  , mname    = "PvP"
  , mfreq    = [("PvP", 1)]
  , mplayers = playersPvP
  , mcaves   = cavesSkirmish
  , mdesc    = "(Not usable right now.) This is a fight to the death between two human-controlled teams."
  }

coop = ModeKind
  { msymbol  = 'o'
  , mname    = "Coop"
  , mfreq    = [("Coop", 1)]
  , mplayers = playersCoop
  , mcaves   = cavesCampaign
  , mdesc    = "(This mode is intended solely for automated testing.)"
  }

defense = ModeKind
  { msymbol  = 'e'
  , mname    = "defense"
  , mfreq    = [("defense", 1)]
  , mplayers = playersDefense
  , mcaves   = cavesCampaign
  , mdesc    = "Don't let the puny humans steal you secrets and flee, like dirty scoundrels that they are, to the exit!"
  }


playersCampaign, playersDuel, playersSkirmish, playersBattle, playersSafari, playersPvP, playersCoop, playersDefense :: Players

playersCampaign = Players
  { playersList = [ playerHero
                  , playerAlien
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                   , ("Spacefarer Crew", "Animal Kingdom")
                   , ("Spacefarer Crew", "Robot Anarchy") ]
 , playersAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

playersDuel = Players
  { playersList = [ playerHero { playerName = "Spacefarer Crew"
                               , playerInitial = 1 }
                  , playerAntiHero { playerName = "Red Collars"
                                   , playerInitial = 1 }
                  , playerHorror ]
  , playersEnemy = [ ("Spacefarer Crew", "Red Collars")
                   , ("Spacefarer Crew", "Horror Den")
                   , ("Red Collars", "Horror Den") ]
  , playersAlly = [] }

playersSkirmish = playersDuel
  { playersList = [ playerHero {playerName = "Spacefarer Crew"}
                  , playerAntiHero {playerName = "Red Collars"}
                  , playerHorror ] }

playersBattle = Players
  { playersList = [ playerHero {playerInitial = 5}
                  , playerAlien { playerInitial = 10
                                , playerIsSpawn = False }
                  , playerAnimal { playerInitial = 5
                                 , playerIsSpawn = False }
                  , playerRobot { playerInitial = 5
                                 , playerIsSpawn = False } ]
  , playersEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                   , ("Spacefarer Crew", "Animal Kingdom")
                   , ("Spacefarer Crew", "Robot Anarchy") ]
 , playersAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

playersSafari = Players
  { playersList = [ playerAlien { playerName = "Alien Tourist Office"
                                , playerIsSpawn = False
                                , playerEntry = -8
                                , playerInitial = 10
                                , playerAI = False
                                , playerUI = True }
                  , playerCivilian { playerName = "Hunam Convict Pack"
                                   , playerEntry = -8 }
                  , playerAnimal { playerName =
                                     "Animal Magnificent Specimen Variety"
                                 , playerIsSpawn = False
                                 , playerEntry = -9
                                 , playerInitial = 7 }
                  , playerAnimal { playerName =
                                     "Animal Exquisite Herds and Packs"
                                 , playerIsSpawn = False
                                 , playerEntry = -10
                                 , playerInitial = 20 } ]
  , playersEnemy = [ ("Alien Tourist Office", "Hunam Convict Pack")
                   , ("Alien Tourist Office",
                      "Animal Magnificent Specimen Variety")
                   , ("Alien Tourist Office",
                      "Animal Exquisite Herds and Packs") ]
  , playersAlly = [( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs" )] }

playersPvP = Players
  { playersList = [ playerHero {playerName = "Red"}
                  , playerHero {playerName = "Blue"}
                  , playerHorror ]
  , playersEnemy = [ ("Red", "Blue")
                   , ("Red", "Horror Den")
                   , ("Blue", "Horror Den") ]
  , playersAlly = [] }

playersCoop = Players
  { playersList = [ playerAntiHero { playerName = "Coral" }
                  , playerAntiHero { playerName = "Amber" }
                  , playerAntiHero { playerName = "Green" }
                  , playerAntiHero { playerName = "Yellow" }
                  , playerAntiHero { playerName = "Cyan" }
                  , playerAntiHero { playerName = "Red"
                                   , playerLeader = False }
                  , playerAntiHero { playerName = "Blue"
                                   , playerLeader = False }
                  , playerAnimal { playerUI = True }
                  , playerAlien
                  , playerAlien { playerName = "Leaderless Alien Hierarchy"
                                  , playerLeader = False }
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Coral", "Alien Hierarchy")
                   , ("Coral", "Animal Kingdom")
                   , ("Coral", "Robot Anarchy")
                   , ("Amber", "Alien Hierarchy")
                   , ("Amber", "Animal Kingdom")
                   , ("Amber", "Robot Anarchy")
                   , ("Green", "Alien Hierarchy")
                   , ("Yellow", "Alien Hierarchy")
                   , ("Cyan", "Alien Hierarchy")
                   , ("Red", "Alien Hierarchy")
                   , ("Blue", "Alien Hierarchy")
                   , ("Animal Kingdom", "Leaderless Alien Hierarchy") ]
  , playersAlly = [ ("Coral", "Amber")
                  , ("Green", "Yellow")
                  , ("Green", "Cyan")
                  , ("Yellow", "Cyan") ] }

playersDefense = Players
  { playersList = [ playerAlien { playerInitial = 1
                                , playerAI = False
                                , playerUI = True }
                  , playerAntiHero { playerName = "Yellow"
                                   , playerInitial = 10 }
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Yellow", "Alien Hierarchy")
                   , ("Yellow", "Animal Kingdom")
                   , ("Yellow", "Robot Anarchy") ]
  , playersAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                  , ("Alien Hierarchy", "Robot Anarchy")
                  , ("Robot Anarchy", "Animal Kingdom") ] }

playerHero, playerAntiHero, playerCivilian, playerAlien, playerAnimal, playerRobot, playerHorror :: Player

playerHero = Player
  { playerName = "Spacefarer Crew"
  , playerFaction = "hero"
  , playerIsSpawn = False
  , playerIsHero = True
  , playerEntry = 1
  , playerInitial = 3
  , playerLeader = True
  , playerAI = False
  , playerUI = True
  }

playerAntiHero = playerHero
  { playerAI = True
  , playerUI = False
  }

playerCivilian = Player
  { playerName = "Civilian Crowd"
  , playerFaction = "civilian"
  , playerIsSpawn = False
  , playerIsHero = False
  , playerEntry = 1
  , playerInitial = 3
  , playerLeader = False  -- unorganized
  , playerAI = True
  , playerUI = False
  }

playerAlien = Player
  { playerName = "Alien Hierarchy"
  , playerFaction = "alien"
  , playerIsSpawn = True
  , playerIsHero = False
  , playerEntry = 4
  , playerInitial = 3
  , playerLeader = True
  , playerAI = True
  , playerUI = False
  }

playerAnimal = Player
  { playerName = "Animal Kingdom"
  , playerFaction = "animal"
  , playerIsSpawn = True
  , playerIsHero = False
  , playerEntry = 2
  , playerInitial = 3
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }

playerRobot = Player
  { playerName = "Robot Anarchy"
  , playerFaction = "robot"
  , playerIsSpawn = True
  , playerIsHero = False
  , playerEntry = 3
  , playerInitial = 3
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }

playerHorror = Player
  { playerName = "Horror Den"
  , playerFaction = "horror"
  , playerIsSpawn = False
  , playerIsHero = False
  , playerEntry = 1
  , playerInitial = 0
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }


cavesCampaign, cavesSkirmish, cavesAmbush, cavesBattle, cavesSafari :: Caves

cavesCampaign = EM.fromList [ (1, ("caveRogue", Nothing))
                            , (12, ("caveNoise", Just True))]

cavesSkirmish = EM.fromList [(3, ("caveSkirmish", Nothing))]

cavesAmbush = EM.fromList [(5, ("caveAmbush", Nothing))]

cavesBattle = EM.fromList [(3, ("caveBattle", Nothing))]

cavesSafari = EM.fromList [ (4, ("caveSafari1", Nothing))
                          , (7, ("caveSafari2", Nothing))
                          , (10, ("caveSafari3", Just False)) ]
