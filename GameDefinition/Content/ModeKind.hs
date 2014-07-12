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
  }

duel = ModeKind
  { msymbol  = 'u'
  , mname    = "duel"
  , mfreq    = [("duel", 1)]
  , mplayers = playersDuel
  , mcaves   = cavesSkirmish
  }

skirmish = ModeKind
  { msymbol  = 'k'
  , mname    = "skirmish"
  , mfreq    = [("skirmish", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesSkirmish
  }

ambush = ModeKind
  { msymbol  = 'm'
  , mname    = "ambush"
  , mfreq    = [("ambush", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesAmbush
  }

battle = ModeKind
  { msymbol  = 'b'
  , mname    = "battle"
  , mfreq    = [("battle", 1)]
  , mplayers = playersBattle
  , mcaves   = cavesBattle
  }

safari = ModeKind
  { msymbol  = 'f'
  , mname    = "safari"
  , mfreq    = [("safari", 1)]
  , mplayers = playersSafari
  , mcaves   = cavesSafari
  }

pvp = ModeKind
  { msymbol  = 'v'
  , mname    = "PvP"
  , mfreq    = [("PvP", 1)]
  , mplayers = playersPvP
  , mcaves   = cavesSkirmish
  }

coop = ModeKind
  { msymbol  = 'o'
  , mname    = "Coop"
  , mfreq    = [("Coop", 1)]
  , mplayers = playersCoop
  , mcaves   = cavesCampaign
  }

defense = ModeKind
  { msymbol  = 'e'
  , mname    = "defense"
  , mfreq    = [("defense", 1)]
  , mplayers = playersDefense
  , mcaves   = cavesCampaign
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
  { playersList = [ playerHero { playerName = "White"
                               , playerInitial = 1 }
                  , playerAntiHero { playerName = "Purple"
                                   , playerInitial = 1 }
                  , playerHorror ]
  , playersEnemy = [ ("White", "Purple")
                   , ("White", "Horror Den")
                   , ("Purple", "Horror Den") ]
  , playersAlly = [] }

playersSkirmish = playersDuel
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero {playerName = "Purple"}
                  , playerHorror ] }

playersBattle = Players
  { playersList = [ playerHero {playerInitial = 5}
                  , playerAlien { playerInitial = 10
                                , playerSpawn = 0 }
                  , playerAnimal { playerInitial = 5
                                 , playerSpawn = 0 }
                  , playerRobot { playerInitial = 5
                                 , playerSpawn = 0 } ]
  , playersEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                   , ("Spacefarer Crew", "Animal Kingdom")
                   , ("Spacefarer Crew", "Robot Anarchy") ]
 , playersAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

playersSafari = Players
  { playersList = [ playerAlien { playerName = "Alien Tourist Office"
                                , playerSpawn = 0
                                , playerEntry = -8
                                , playerInitial = 10
                                , playerAI = False
                                , playerUI = True }
                  , playerCivilian { playerName = "Hunam Convict Pack"
                                   , playerEntry = -8 }
                  , playerAnimal { playerName =
                                     "Animal Magnificent Specimen Variety"
                                 , playerSpawn = 0
                                 , playerEntry = -9
                                 , playerInitial = 7 }
                  , playerAnimal { playerName =
                                     "Animal Exquisite Herds and Packs"
                                 , playerSpawn = 0
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
  , playerSpawn = 0
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
  , playerSpawn = 0
  , playerEntry = 1
  , playerInitial = 3
  , playerLeader = False  -- unorganized
  , playerAI = True
  , playerUI = False
  }

playerAlien = Player
  { playerName = "Alien Hierarchy"
  , playerFaction = "alien"
  , playerSpawn = 50
  , playerEntry = 4
  , playerInitial = 3
  , playerLeader = True
  , playerAI = True
  , playerUI = False
  }

playerAnimal = Player
  { playerName = "Animal Kingdom"
  , playerFaction = "animal"
  , playerSpawn = 30
  , playerEntry = 2
  , playerInitial = 3
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }

playerRobot = Player
  { playerName = "Robot Anarchy"
  , playerFaction = "robot"
  , playerSpawn = 20
  , playerEntry = 3
  , playerInitial = 3
  , playerLeader = False
  , playerAI = True
  , playerUI = False
  }

playerHorror = Player
  { playerName = "Horror Den"
  , playerFaction = "horror"
  , playerSpawn = 0
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

cavesSafari = EM.fromList [ (8, ("caveAmbush", Nothing))
                          , (9, ("caveBattle", Nothing))
                          , (10, ("caveSkirmish", Just False)) ]
