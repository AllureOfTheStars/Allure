-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The type of kinds of game modes for LambdaHack.
module Content.ModeKind ( cdefs ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.ModeKind

cdefs :: ContentDef ModeKind
cdefs = ContentDef
  { getSymbol = msymbol
  , getName = mname
  , getFreq = mfreq
  , validate = mvalidate
  , content =
      [campaign, skirmish, pvp, coop, defense, screensaver, testCoop, testDefense, peekCampaign, peekSkirmish]
  }
campaign,        skirmish, pvp, coop, defense, screensaver, testCoop, testDefense, peekCampaign, peekSkirmish :: ModeKind

campaign = ModeKind
  { msymbol  = 'r'  -- matches the keypress (with C-)
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
  , playersEnemy = [ ("Spaceship Crew", "Alien Hierarchy")
                   , ("Spaceship Crew", "Animal Kingdom")
                   , ("Spaceship Crew", "Robotic Anarchy") ]
  , playersAlly = [] }

playersSkirmish = Players
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero {playerName = "Green"}
                  , playerHorror ]
  , playersEnemy = [ ("White", "Green")
                   , ("White", "Horror Den")
                   , ("Green", "Horror Den") ]
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
                   , ("Coral", "Robotic Anarchy")
                   , ("Amber", "Alien Hierarchy")
                   , ("Amber", "Animal Kingdom")
                   , ("Amber", "Robotic Anarchy") ]
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
                   , ("Green", "Robotic Anarchy")
                   , ("Yellow", "Alien Hierarchy")
                   , ("Yellow", "Animal Kingdom")
                   , ("Yellow", "Robotic Anarchy")
                   , ("Cyan", "Alien Hierarchy")
                   , ("Cyan", "Animal Kingdom")
                   , ("Cyan", "Robotic Anarchy") ]
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
  , playersEnemy = [ ("Spaceship Crew", "Alien Hierarchy")
                   , ("Spaceship Crew", "Animal Kingdom")
                   , ("Spaceship Crew", "Robotic Anarchy") ]
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
                   , ("Coral", "Robotic Anarchy")
                   , ("Amber", "Alien Hierarchy")
                   , ("Amber", "Animal Kingdom")
                   , ("Amber", "Robotic Anarchy") ]
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
                   , ("Green", "Robotic Anarchy")
                   , ("Yellow", "Alien Hierarchy")
                   , ("Yellow", "Animal Kingdom")
                   , ("Yellow", "Robotic Anarchy")
                   , ("Cyan", "Alien Hierarchy")
                   , ("Cyan", "Animal Kingdom")
                   , ("Cyan", "Robotic Anarchy") ]
  , playersAlly = [ ("Green", "Yellow")
                  , ("Green", "Cyan")
                  , ("Yellow", "Cyan") ] }

playersPeekCampaign = Players
  { playersList = [ playerHero {playerInitial = 1}
                  , playerMonster {playerUI = True}
                  , playerAnimal
                  , playerRobot ]
  , playersEnemy = [ ("Spaceship Crew", "Alien Hierarchy")
                   , ("Spaceship Crew", "Animal Kingdom")
                   , ("Spaceship Crew", "Robotic Anarchy") ]
  , playersAlly = [] }

playersPeekSkirmish = Players
  { playersList = [ playerHero {playerName = "White"}
                  , playerAntiHero { playerName = "Green"
                                   , playerUI = True }
                  , playerHorror ]
  , playersEnemy = [ ("White", "Green")
                   , ("White", "Horror Den")
                   , ("Green", "Horror Den") ]
  , playersAlly = [] }


playerHero, playerAntiHero, playerMonster, playerAnimal, playerRobot, playerHorror :: Player

playerHero = Player
  { playerName = "Spaceship Crew"
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
  { playerName = "Robotic Anarchy"
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

-- Cave "dng" means a random choice from caves that can randomly appear;
-- this is the default and the lack of the Escape feature is the default.

cavesCampaign = EM.fromList [ (toEnum 1, ("caveRogue", Nothing))
                            , (toEnum 12, ("caveNoise", Just True))]

cavesCombat = EM.fromList [(toEnum 3, ("caveCombat", Nothing))]

cavesDefense = EM.fromList [ (toEnum 1, ("dng", Nothing))
                           , (toEnum 5, ("caveEmpty", Just True))]
