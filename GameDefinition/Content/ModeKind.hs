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
      [campaign, skirmish, battle, pvp, coop, defense, testCampaign, testSkirmish, testBattle, testPvP, testCoop, testDefense, peekCampaign, peekSkirmish]
  }
campaign,        skirmish, battle, pvp, coop, defense, testCampaign, testSkirmish, testBattle, testPvP, testCoop, testDefense, peekCampaign, peekSkirmish :: ModeKind

campaign = ModeKind
  { msymbol  = 'r'
  , mname    = "campaign"
  , mfreq    = [("campaign", 1)]
  , mplayers = playersCampaign
  , mcaves   = cavesCampaign
  }

skirmish = ModeKind
  { msymbol  = 'k'
  , mname    = "skirmish"
  , mfreq    = [("skirmish", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesCombat
  }

battle = ModeKind
  { msymbol  = 'b'
  , mname    = "battle"
  , mfreq    = [("battle", 1)]
  , mplayers = playersBattle
  , mcaves   = cavesBattle
  }

pvp = ModeKind
  { msymbol  = 'v'
  , mname    = "PvP"
  , mfreq    = [("PvP", 1)]
  , mplayers = playersPvP
  , mcaves   = cavesCombat
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

testCampaign = ModeKind
  { msymbol  = 't'
  , mname    = "testCampaign"
  , mfreq    = [("testCampaign", 1)]
  , mplayers = playersTestCampaign
  , mcaves   = cavesCampaign
  }

testSkirmish = ModeKind
  { msymbol  = 't'
  , mname    = "testSkirmish"
  , mfreq    = [("testSkirmish", 1)]
  , mplayers = playersTestSkirmish
  , mcaves   = cavesCombat
  }

testBattle = ModeKind
  { msymbol  = 't'
  , mname    = "testBattle"
  , mfreq    = [("testBattle", 1)]
  , mplayers = playersTestBattle
  , mcaves   = cavesBattle
  }

testPvP = ModeKind
  { msymbol  = 't'
  , mname    = "testPvP"
  , mfreq    = [("testPvP", 1)]
  , mplayers = playersTestPvP
  , mcaves   = cavesCombat
  }

testCoop = ModeKind
  { msymbol  = 't'
  , mname    = "testCoop"
  , mfreq    = [("testCoop", 1)]
  , mplayers = playersTestCoop
  , mcaves   = cavesCampaign
  }

testDefense = ModeKind
  { msymbol  = 't'
  , mname    = "testDefense"
  , mfreq    = [("testDefense", 1)]
  , mplayers = playersTestDefense
  , mcaves   = cavesCampaign
  }

peekCampaign = ModeKind
  { msymbol  = 'p'
  , mname    = "peekCampaign"
  , mfreq    = [("peekCampaign", 1)]
  , mplayers = playersPeekCampaign
  , mcaves   = cavesCampaign
  }

peekSkirmish = ModeKind
  { msymbol  = 'p'
  , mname    = "peekSkirmish"
  , mfreq    = [("peekSkirmish", 1)]
  , mplayers = playersPeekSkirmish
  , mcaves   = cavesCombat
  }


playersCampaign, playersSkirmish, playersBattle, playersPvP, playersCoop, playersDefense, playersTestCampaign, playersTestSkirmish, playersTestBattle, playersTestPvP, playersTestCoop, playersTestDefense, playersPeekCampaign, playersPeekSkirmish :: Players

playersCampaign = Players
  { playersList = [ playerHero {playerInitial = 1}
                  , playerMonster
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                   , ("Spacefarer Crew", "Animal Kingdom")
                   , ("Spacefarer Crew", "Robot Anarchy") ]
  , playersAlly = [] }

playersSkirmish = Players
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero {playerName = "Purple"}
                  , playerHorror ]
  , playersEnemy = [ ("White", "Purple")
                   , ("White", "Horror Den")
                   , ("Purple", "Horror Den") ]
  , playersAlly = [] }

playersBattle = Players
  { playersList = [ playerHero {playerInitial = 5}
                  , playerMonster { playerInitial = 10
                                  , playerSpawn = 0 }
                  , playerAnimal { playerInitial = 10
                                 , playerSpawn = 0 }
                  , playerRobot { playerInitial = 10
                                , playerSpawn = 0 } ]
  , playersEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                   , ("Spacefarer Crew", "Animal Kingdom")
                   , ("Spacefarer Crew", "Robot Anarchy") ]
  , playersAlly = [] }

playersPvP = Players
  { playersList = [ playerHero {playerName = "Red"}
                  , playerHero {playerName = "Blue"}
                  , playerHorror ]
  , playersEnemy = [ ("Red", "Blue")
                   , ("Red", "Horror Den")
                   , ("Blue", "Horror Den") ]
  , playersAlly = [] }

playersCoop = Players
  { playersList = [ playerHero { playerName = "Coral"
                               , playerInitial = 1 }
                  , playerHero { playerName = "Amber"
                               , playerInitial = 1 }
                  , playerMonster
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Coral", "Alien Hierarchy")
                   , ("Coral", "Animal Kingdom")
                   , ("Coral", "Robot Anarchy")
                   , ("Amber", "Alien Hierarchy")
                   , ("Amber", "Animal Kingdom")
                   , ("Amber", "Robot Anarchy") ]
  , playersAlly = [("Coral", "Amber")] }

playersDefense = Players
  { playersList = [ playerMonster { playerInitial = 1
                                  , playerAiLeader = False
                                  , playerHuman = True
                                  , playerUI = True }
                  , playerAnimal
                  , playerRobot
                  , playerAntiHero {playerName = "Green"}
                  , playerAntiHero {playerName = "Yellow"}
                  , playerAntiHero {playerName = "Cyan"} ]
  , playersEnemy = [ ("Green", "Alien Hierarchy")
                   , ("Green", "Animal Kingdom")
                   , ("Green", "Robot Anarchy")
                   , ("Yellow", "Alien Hierarchy")
                   , ("Yellow", "Animal Kingdom")
                   , ("Yellow", "Robot Anarchy")
                   , ("Cyan", "Alien Hierarchy")
                   , ("Cyan", "Animal Kingdom")
                   , ("Cyan", "Robot Anarchy") ]
  , playersAlly = [ ("Green", "Yellow")
                  , ("Green", "Cyan")
                  , ("Yellow", "Cyan") ] }

playersTestCampaign = playersCampaign
  { playersList = [ playerHero { playerInitial = 5
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerMonster
                  , playerAnimal
                  , playerRobot ] }

playersTestSkirmish = playersSkirmish
  { playersList = [ playerHero { playerName = "White"
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerAntiHero { playerName = "Purple" }
                  , playerHorror ] }

playersTestBattle = playersBattle
  { playersList = [ playerHero { playerInitial = 5
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerMonster { playerInitial = 10
                                  , playerSpawn = 0 }
                  , playerAnimal { playerInitial = 10
                                 , playerSpawn = 0 }
                  , playerRobot { playerInitial = 10
                                , playerSpawn = 0 } ] }

playersTestPvP = playersPvP
  { playersList = [ playerHero { playerName = "Red"
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerHero { playerName = "Blue"
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerHorror ] }

playersTestCoop = playersCoop
  { playersList = [ playerHero { playerName = "Coral"
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerHero { playerName = "Amber"
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerMonster
                  , playerAnimal
                  , playerRobot ] }

playersTestDefense = playersDefense
  { playersList = [ playerMonster { playerInitial = 1
                                  , playerUI = True }
                  , playerAnimal
                  , playerRobot
                  , playerAntiHero {playerName = "Green"}
                  , playerAntiHero {playerName = "Yellow"}
                  , playerAntiHero {playerName = "Cyan"} ] }

playersPeekCampaign = playersCampaign
  { playersList = [ playerHero {playerInitial = 1}
                  , playerMonster {playerUI = True}
                  , playerAnimal
                  , playerRobot ] }

playersPeekSkirmish = playersSkirmish
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero { playerName = "Purple"
                                   , playerUI = True }
                  , playerHorror ] }


playerHero, playerAntiHero, playerMonster, playerAnimal, playerRobot, playerHorror :: Player

playerHero = Player
  { playerName = "Spacefarer Crew"
  , playerFaction = "hero"
  , playerSpawn = 0
  , playerEntry = toEnum 1
  , playerInitial = 3
  , playerAiLeader = False
  , playerAiOther = True
  , playerHuman = True
  , playerUI = True
  }

playerAntiHero = playerHero
  { playerAiLeader = True
  , playerHuman = False
  , playerUI = False
  }

playerMonster = Player
  { playerName = "Alien Hierarchy"
  , playerFaction = "alien"
  , playerSpawn = 20
  , playerEntry = toEnum 5
  , playerInitial = 3
  , playerAiLeader = True
  , playerAiOther = True
  , playerHuman = False
  , playerUI = False
  }

playerAnimal = Player
  { playerName = "Animal Kingdom"
  , playerFaction = "animal"
  , playerSpawn = 50
  , playerEntry = toEnum 3
  , playerInitial = 3
  , playerAiLeader = True
  , playerAiOther = True
  , playerHuman = False
  , playerUI = False
  }

playerRobot = Player
  { playerName = "Robot Anarchy"
  , playerFaction = "robot"
  , playerSpawn = 10
  , playerEntry = toEnum 4
  , playerInitial = 3
  , playerAiLeader = True
  , playerAiOther = True
  , playerHuman = False
  , playerUI = False
  }

playerHorror = Player
  { playerName = "Horror Den"
  , playerFaction = "horror"
  , playerSpawn = 0
  , playerEntry = toEnum 1
  , playerInitial = 0
  , playerAiLeader = True
  , playerAiOther = True
  , playerHuman = False
  , playerUI = False
  }


cavesCampaign, cavesCombat, cavesBattle :: Caves

cavesCampaign = EM.fromList [ (toEnum 1, ("caveRogue", Nothing))
                            , (toEnum 12, ("caveNoise", Just False))]

cavesCombat = EM.fromList [(toEnum 3, ("caveCombat", Nothing))]

cavesBattle = EM.fromList [(toEnum (-3), ("caveBattle", Nothing))]
