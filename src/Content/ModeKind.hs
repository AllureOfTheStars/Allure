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
      [campaign, skirmish, pvp, coop, defense, screensaver, testCoop, testDefense, peekCampaign, peekSkirmish]
  }
campaign,        skirmish, pvp, coop, defense, screensaver, testCoop, testDefense, peekCampaign, peekSkirmish :: ModeKind

campaign = ModeKind
  { msymbol  = 'r'
  , mname    = "campaign"
  , mfreq    = [("campaign", 1)]
  , mplayers = playersCampaign
  , mcaves   = cavesCampaign
  }

skirmish = ModeKind
  { msymbol  = 's'
  , mname    = "skirmish"
  , mfreq    = [("skirmish", 1)]
  , mplayers = playersSkirmish
  , mcaves   = cavesCombat
  }

pvp = ModeKind
  { msymbol  = 'p'
  , mname    = "PvP"
  , mfreq    = [("PvP", 1)]
  , mplayers = playersPvP
  , mcaves   = cavesCombat
  }

coop = ModeKind
  { msymbol  = 'c'
  , mname    = "Coop"
  , mfreq    = [("Coop", 1)]
  , mplayers = playersCoop
  , mcaves   = cavesCampaign
  }

defense = ModeKind
  { msymbol  = 'd'
  , mname    = "defense"
  , mfreq    = [("defense", 1)]
  , mplayers = playersDefense
  , mcaves   = cavesDefense
  }

screensaver = ModeKind
  { msymbol  = 'n'
  , mname    = "screensaver"
  , mfreq    = [("screensaver", 1)]
  , mplayers = playersScreensaver
  , mcaves   = cavesCampaign
  }

testCoop = ModeKind
  { msymbol  = 't'
  , mname    = "testCoop"
  , mfreq    = [("testCoop", 1)]
  , mplayers = playersTestCoop
  , mcaves   = cavesCampaign
  }

testDefense = ModeKind
  { msymbol  = 'u'
  , mname    = "testDefense"
  , mfreq    = [("testDefense", 1)]
  , mplayers = playersTestDefense
  , mcaves   = cavesDefense
  }

peekCampaign = ModeKind
  { msymbol  = 'e'
  , mname    = "peekCampaign"
  , mfreq    = [("peekCampaign", 1)]
  , mplayers = playersPeekCampaign
  , mcaves   = cavesCampaign
  }

peekSkirmish = ModeKind
  { msymbol  = 'f'
  , mname    = "peekSkirmish"
  , mfreq    = [("peekSkirmish", 1)]
  , mplayers = playersPeekSkirmish
  , mcaves   = cavesCombat
  }


playersCampaign, playersSkirmish, playersPvP, playersCoop, playersDefense, playersScreensaver, playersTestCoop, playersTestDefense, playersPeekCampaign, playersPeekSkirmish :: Players

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
                                  , playerEntry = toEnum 1
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

playersScreensaver = Players
  { playersList = [ playerHero { playerInitial = 5
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerMonster
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                   , ("Spacefarer Crew", "Animal Kingdom")
                   , ("Spacefarer Crew", "Robot Anarchy") ]
  , playersAlly = [] }

playersTestCoop = Players
  { playersList = [ playerHero { playerName = "Coral"
                               , playerAiLeader = True
                               , playerHuman = False }
                  , playerHero { playerName = "Amber"
                               , playerAiLeader = True
                               , playerHuman = False }
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

playersTestDefense = Players
  { playersList = [ playerMonster { playerInitial = 1
                                  , playerEntry = toEnum 1
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

playersPeekCampaign = Players
  { playersList = [ playerHero {playerInitial = 1}
                  , playerMonster {playerUI = True}
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Spacefarer Crew", "Alien Hierarchy")
                   , ("Spacefarer Crew", "Animal Kingdom")
                   , ("Spacefarer Crew", "Robot Anarchy") ]
  , playersAlly = [] }

playersPeekSkirmish = Players
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero { playerName = "Purple"
                                   , playerUI = True }
                  , playerHorror ]
  , playersEnemy = [ ("White", "Purple")
                   , ("White", "Horror Den")
                   , ("Purple", "Horror Den") ]
  , playersAlly = [] }


playerHero, playerAntiHero, playerMonster, playerAnimal, playerRobot, playerHorror :: Player

playerHero = Player
  { playerName = "Spacefarer Crew"
  , playerFaction = "hero"
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
  , playerEntry = toEnum 1
  , playerInitial = 0
  , playerAiLeader = True
  , playerAiOther = True
  , playerHuman = False
  , playerUI = False
  }


cavesCampaign, cavesCombat, cavesDefense :: Caves

cavesCampaign = EM.fromList [ (toEnum 1, ("caveRogue", Nothing))
                            , (toEnum 12, ("caveNoise", Just True))]

cavesCombat = EM.fromList [(toEnum 3, ("caveCombat", Nothing))]

cavesDefense = EM.fromList [ (toEnum 1, ("dng", Nothing))
                           , (toEnum 5, ("caveEmpty", Just True))]
