-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor ( actors ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, eye, fastEye, nose, elbow, armadillo, gilaMonster, komodoDragon, hyena, alligator, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot]

warrior,    adventurer, blacksmith, forester, clerk, hairdresser, lawyer, peddler, taxCollector, eye, fastEye, nose, elbow, armadillo, gilaMonster, komodoDragon, hyena, alligator, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot :: ItemKind

-- * Hunams

warrior = ItemKind
  { isymbol  = '@'
  , iname    = "warrior"  -- modified if in hero faction
  , ifreq    = [("hero", 100), ("civilian", 100)]
  , iflavour = zipPlain [BrBlack]  -- modified if in hero faction
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 50, AddMaxCalm 60, AddSpeed 20
               , AddSight 3 ]  -- no via eyes, but feel, hearing, etc.
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("fist", COrgan), ("foot", COrgan), ("eye 4", COrgan)]
  }
adventurer = warrior
  { iname    = "adventurer" }
blacksmith = warrior
  { iname    = "blacksmith" }
forester = warrior
  { iname    = "forester" }

clerk = warrior
  { iname    = "clerk"
  , ifreq    = [("civilian", 100)] }
hairdresser = clerk
  { iname    = "hairdresser" }
lawyer = clerk
  { iname    = "lawyer" }
peddler = clerk
  { iname    = "peddler" }
taxCollector = clerk
  { iname    = "tax collector" }

-- * Aliens

eye = ItemKind
  { isymbol  = 'e'
  , iname    = "reducible eye"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 20
               , AddSight 4 ]  -- can shoot for as long as lives
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("lash", COrgan), ("pupil", COrgan)]
  }
fastEye = ItemKind
  { isymbol  = 'e'
  , iname    = "super-fast eye"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 6, AddMaxCalm 60, AddSpeed 30
               , AddSight 4 ]  -- can shoot for as long as lives
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("lash", COrgan), ("tentacle", COrgan), ("tentacle", COrgan)
               , ("speed gland 5", COrgan), ("pupil", COrgan) ]
  }
nose = ItemKind
  { isymbol  = 'h'
  , iname    = "tentacled horror"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 6), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 30, AddSpeed 18
               , AddSmell 3 ]  -- depends solely on smell
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("tentacle", COrgan), ("tentacle", COrgan)
               , ("nose tip", COrgan), ("lip", COrgan) ]
  }
elbow = ItemKind
  { isymbol  = 'e'
  , iname    = "ground elbow"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(3, 5), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 80, AddSpeed 10
               , AddSkills $ EM.singleton AbMelee (-1)
               , AddSight 15 ]  -- can shoot for as long as lives
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("armored skin", COrgan), ("speed gland 2", COrgan)
               , ("any arrow", CInv), ("any arrow", CInv)
               , ("any arrow", CInv), ("any arrow", CInv) ]
  }

-- * Animals

armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 30, AddSpeed 18
               , AddSkills $ EM.singleton AbAlter (-1)
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("claw", COrgan), ("snout", COrgan), ("armored skin", COrgan)
               , ("nostril", COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 15, AddMaxCalm 60, AddSpeed 15
               , AddSkills $ EM.singleton AbAlter (-1)
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan) ]
  }
komodoDragon = ItemKind  -- bad hearing
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(5, 5), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 20
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan), ("small claw", COrgan)
               , ("speed gland 2", COrgan), ("armored skin", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(4, 6), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 60, AddSpeed 35
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("jaw", COrgan), ("eye 4", COrgan), ("nostril", COrgan)]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 60, AddSpeed 17
               , AddArmorMelee 30, AddArmorRanged 30
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large jaw", COrgan), ("large tail", COrgan), ("claw", COrgan)
               , ("armored skin", COrgan), ("eye 4", COrgan) ]
  }

-- * Non-animal animals

hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30, AddSight 2
               , AddSkills $ EM.singleton AbAlter (-1)
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("sting", COrgan)]
  }
thornbush = ItemKind
  { isymbol  = 'b'
  , iname    = "rose bush"
  , ifreq    = [("animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 20
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1, 1..])
               , AddArmorMelee 30, AddArmorRanged 30 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("thorn", COrgan)]
  }

-- * Robots

razorwireFence = ItemKind
  { isymbol  = 'f'
  , iname    = "razorwire fence"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 20
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1, 1..])
               , AddArmorMelee 50, AddArmorRanged 50 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Must have been bought by previous ship owners to contain the wild animal infestation."
  , ikit     = [("razor", COrgan)]
  }
electricFence = ItemKind
  { isymbol  = 'f'
  , iname    = "electric fence"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 50
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1, 1..])
               , AddArmorMelee 50, AddArmorRanged 50 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Marginally intelligent electric shepherd. Originally used in the spaceship's dairy farm or on the zoo level."
  , ikit     = [("live wire", COrgan)]
  }
activeFence = ItemKind
  { isymbol  = 'f'
  , iname    = "active fence"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(4, 0), (5, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 20, AddSight 6
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbProject]
                                                [1, 1..])
               , AddArmorMelee 50, AddArmorRanged 50 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Makeshift, mostly non-lethal, autonomous perimeter defense outpost."
  , ikit     = [ ("needle", CInv), ("potion of glue", CInv)
               , ("potion of glue", CInv), ("potion of glue", CInv) ]
  }
steamFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "steam faucet"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 100, AddMaxCalm 999, AddSpeed 5
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1, 1..]) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("boiling vent", COrgan), ("fissure", COrgan)]
  }
biogasFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "biogas faucet"
  , ifreq    = [("robot", 33)]  -- very rare
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(3, 0), (4, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 100, AddMaxCalm 999, AddSpeed 5
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait] [1, 1..]) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("explosion vent", COrgan)]
  }
shepherdDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "shepherd drone"
  , ifreq    = [("robot", 100), ("horror", 100)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 2)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 2, AddMaxCalm 60, AddSpeed 30, AddSight 4
               , AddSkills
                 $ EM.fromDistinctAscList
                 $ zip [AbDisplace, AbMoveItem, AbProject] [-1, -1..]
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("eye2", COrgan), ("live wire", COrgan)]
  }
huntingDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "hunting drone"
  , ifreq    = [("robot", 100), ("horror", 100)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(4, 0), (5, 1), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 500
  , iaspects = [ AddMaxHP 2, AddMaxCalm 60, AddSpeed 40, AddSight 4
               , AddSkills
                 $ EM.fromDistinctAscList
                 $ zip [AbDisplace, AbMoveItem, AbMelee] [-1, -1..]
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Originally designed for hunting down and putting to sleep stray animals. The sleeping agent has long since dried up."
  , ikit     = [("eye2", COrgan), ("needle", CInv)]
  }
homeRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "deranged home robot"
  , ifreq    = [("robot", 100), ("horror", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 60, AddSpeed 20
               , AddSmell 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("fist", COrgan), ("eye2", COrgan)]
  }
wasteRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "waste disposal robot"
  , ifreq    = [("robot", 100), ("horror", 100), ("construction robot", 1)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 20
               , AddSmell 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "You are not in its database, so you are considered waste."
  , ikit     = [ ("jaw", COrgan), ("tentacle", COrgan)
               , ("waste container", COrgan)
               , ("armored skin", COrgan), ("eye3", COrgan) ]
  }
lightRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "decoration robot"
  , ifreq    = [("robot", 100), ("horror", 100), ("construction robot", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 8), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 30
               , AddSight 3, AddLight 3 ]  -- light can't be turned off
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("claw", COrgan), ("tentacle", COrgan)
               , ("armored skin", COrgan), ("eye4", COrgan) ]
  }
heavyRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "construction robot"
  , ifreq    = [("robot", 100), ("horror", 100), ("construction robot", 100)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 4), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 800000
  , iaspects = [ AddMaxHP 80, AddMaxCalm 60, AddSpeed 20
               , AddSight 3, AddLight 4 ]  -- light can't be turned off
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("largeJaw", COrgan), ("claw", COrgan)
               , ("construction hooter", CInv)
               , ("armored skin", COrgan), ("eye3", COrgan) ]
  }
