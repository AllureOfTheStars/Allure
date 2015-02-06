-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor ( actors ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, warrior2, warrior3, warrior4, soldier, sniper, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, cleanerRobot]

warrior,    warrior2, warrior3, warrior4, soldier, sniper, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, cleanerRobot :: ItemKind

-- * Hunams

warrior = ItemKind
  { isymbol  = '@'
  , iname    = "mercenary"  -- modified if in hero faction
  , ifreq    = [("hero", 100), ("civilian", 100)]
  , iflavour = zipPlain [BrBlack]  -- modified if in hero faction
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 60  -- partially from clothes and assumed first aid
               , AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.fromList [(AbProject, 1), (AbApply, 1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("fist", COrgan), ("foot", COrgan), ("eye 5", COrgan)
               , ("sapient brain", COrgan) ]
  }
warrior2 = warrior
  { iname    = "pilot" }
warrior3 = warrior
  { iname    = "engineer" }
warrior4 = warrior
  { iname    = "doctor" }

soldier = warrior
  { iname    = "soldier"
  , ifreq    = [("soldier", 100)]
  , ikit     = ikit warrior ++ [("starting weapon", CEqp)]
  }
sniper = warrior
  { iname    = "sniper"
  , ifreq    = [("sniper", 100)]
  , ikit     = ikit warrior
               ++ [ ("ring of opportunity sniper", CEqp)
                  , ("any arrow", CInv), ("any arrow", CInv)
                  , ("any arrow", CInv), ("any arrow", CInv)
                  , ("flask", CInv), ("light source", CInv)
                  , ("light source", CInv), ("light source", CInv) ]
  }

civilian = warrior
  { iname    = "clerk"
  , ifreq    = [("civilian", 100)] }
civilian2 = civilian
  { iname    = "hairdresser" }
civilian3 = civilian
  { iname    = "lawyer" }
civilian4 = civilian
  { iname    = "peddler" }
civilian5 = civilian
  { iname    = "tax collector" }

-- * Aliens

eye = ItemKind
  { isymbol  = 'w'
  , iname    = "beckoning walker"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(4, 6), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.fromList [(AbProject, 1), (AbApply, 1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Walks with a stately dignity. You read death in the slow beckoning gestures of its revolting upper appendages."
  , ikit     = [ ("foot", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("tentacle", COrgan)
               , ("eye 5", COrgan)
               , ("sapient brain", COrgan) ]
  }
fastEye = ItemKind
  { isymbol  = 'b'
  , iname    = "crawling biter"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(4, 3), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It bites as blindingly fast as it runs. Or rolls? Or crawls? Also, cuts and pierces."
  , ikit     = [ ("tentacle", COrgan), ("tentacle", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("jaw", COrgan)
               , ("eye 4", COrgan), ("speed gland 10", COrgan)
               , ("sapient brain", COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'h'
  , iname    = "tentacled horror"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(4, 5), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 30, AddSpeed 18
               , AddSkills $ EM.fromList [(AbProject, -1), (AbApply, -1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A blind, slimy mass of clawing, stinging and burning. You'd think it's powerless, but as soon as it touches your trembling body, it's always one step ahead."
  , ikit     = [ ("nostril", COrgan), ("small claw", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("thorn", COrgan), ("sting", COrgan)
               , ("sapient brain", COrgan) ]
  }
elbow = ItemKind
  { isymbol  = 's'
  , iname    = "creepy shooter"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(6, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 12, AddMaxCalm 90, AddSpeed 26
               , AddSkills
                 $ EM.fromList [(AbProject, 1), (AbApply, 1), (AbMelee, -1)] ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It moves in sudden jerks and never makes a noise. Speaks in hard objects hurled at deadly speeds."
  , ikit     = [ ("speed gland 4", COrgan)
               , ("eye 7", COrgan)
               , ("any arrow", CInv), ("any arrow", CInv)
               , ("any arrow", CInv), ("any arrow", CInv)
               , ("sapient brain", COrgan) ]
  }
torsor = ItemKind
  { isymbol  = 'M'
  , iname    = "The Maker of Contact"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(11 * 10/12, 0), (10, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ Unique, AddMaxHP 100, AddMaxCalm 100, AddSpeed 10
               , AddSkills $ EM.fromList
                   [(AbProject, 1), (AbApply, 1), (AbTrigger, -1)] ]
                   -- can't switch levels, a miniboss
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "The mind, the heart behind it all. Warmth and sympathy pour out through the graceful undulation of tentacles, sharp claws, snapping jaw, grinding teeth and tensing fangs."
  , ikit     = [ ("tentacle", COrgan), ("claw", COrgan), ("large jaw", COrgan)
               , ("venom tooth", COrgan), ("venom fang", COrgan)
               , ("eye 5", COrgan)
               , ("gem", CInv), ("gem", CInv), ("gem", CInv), ("gem", CInv)
               , ("sapient brain", COrgan) ]
  }

-- * Animals

-- They need rather strong melee, because they don't use items.
-- Unless/until they level up.

goldenJackal = ItemKind  -- basically a much smaller and slower hyena
  { isymbol  = 'j'
  , iname    = "golden jackal"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100), ("scavenger", 50)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 13000
  , iaspects = [ AddMaxHP 12, AddMaxCalm 60, AddSpeed 22 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("small jaw", COrgan), ("eye 5", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
griffonVulture = ItemKind
  { isymbol  = 'v'
  , iname    = "griffon vulture"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100), ("scavenger", 30)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 13000
  , iaspects = [ AddMaxHP 12, AddMaxCalm 30, AddSpeed 20
               , AddSkills $ EM.singleton AbAlter (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("screeching beak", COrgan)  -- in reality it grunts and hisses
               , ("small claw", COrgan), ("eye 6", COrgan)
               , ("animal brain", COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 4000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 30, AddSpeed 20
               , AddSkills $ EM.singleton AbAlter (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("scent gland", COrgan)
               , ("small claw", COrgan), ("snout", COrgan)
               , ("nostril", COrgan), ("eye 2", COrgan)
               , ("animal brain", COrgan) ]
  }
armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 35, AddMaxCalm 30, AddSpeed 18
               , AddSkills $ EM.singleton AbAlter (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("claw", COrgan), ("snout", COrgan), ("armored skin", COrgan)
               , ("nostril", COrgan), ("eye 2", COrgan)
               , ("animal brain", COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 12, AddMaxCalm 60, AddSpeed 15
               , AddSkills $ EM.singleton AbAlter (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 5", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(3, 3), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 25, AddMaxCalm 60, AddSpeed 15
               , AddSkills $ EM.singleton AbAlter (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom fang", COrgan)
               , ("eye 5", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
komodoDragon = ItemKind  -- bad hearing; regeneration makes it very powerful
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(8, 1), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 16 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan), ("small claw", COrgan)
               , ("speed gland 4", COrgan), ("armored skin", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100), ("scavenger", 20)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 60000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 30 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("jaw", COrgan), ("eye 5", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(6, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 17 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large jaw", COrgan), ("large tail", COrgan), ("claw", COrgan)
               , ("armored skin", COrgan), ("eye 5", COrgan)
               , ("animal brain", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "The Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1 * 10/12, 1000), (2 * 10/12, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ Unique, AddMaxHP 70, AddMaxCalm 60, AddSpeed 25
               , AddSkills $ EM.singleton AbTrigger (-1) ]
                   -- can't switch levels, a miniboss
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "The last of its kind. Blind with rage. Charges at deadly speed."
  , ikit     = [ ("armored skin", COrgan), ("eye 2", COrgan)
               , ("horn", COrgan), ("snout", COrgan)
               , ("animal brain", COrgan) ]
  }

-- * Non-animal animals

beeSwarm = ItemKind
  { isymbol  = 'b'
  , iname    = "bee swarm"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 3), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30
               , AddSkills $ EM.singleton AbAlter (-1)
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("bee sting", COrgan), ("vision 4", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30
               , AddSkills $ EM.singleton AbAlter (-1)
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("sting", COrgan), ("vision 4", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [("animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 3), (10, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 20
               , AddSkills $ EM.fromList (zip [AbWait, AbMelee] [1, 1..]) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("thorn", COrgan), ("armored skin", COrgan)]
  }

-- * Robots

razorwireFence = ItemKind
  { isymbol  = 'f'
  , iname    = "razorwire fence"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 20
               , AddSkills $ EM.fromList (zip [AbWait, AbMelee] [1, 1..])
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
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 50
               , AddSkills $ EM.fromList (zip [AbWait, AbMelee] [1, 1..])
               , AddArmorMelee 50, AddArmorRanged 50 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Marginally intelligent electric shepherd. Originally used in the spaceship's dairy farm and the zoo level."
  , ikit     = [("live wire", COrgan)]
  }
activeFence = ItemKind
  { isymbol  = 'f'
  , iname    = "active fence"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(3, 0), (5, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 20
               , AddSkills $ EM.fromList [(AbWait, 1), (AbProject, 2)]
               , AddArmorMelee 50, AddArmorRanged 50 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Makeshift, mostly non-lethal, autonomous perimeter defense outpost."
  , ikit     = [ ("vision 6", COrgan)
               , ("needle", CInv), ("can of sticky foam", CInv)
               , ("can of sticky foam", CInv), ("can of sticky foam", CInv) ]
  }
steamFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "steam faucet"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 5
               , AddSkills $ EM.fromList (zip [AbWait, AbMelee] [1, 1..])
               , AddArmorMelee 80, AddArmorRanged 80 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A cracked valve on one of the superheated water pipes spreading radially outward from the tokamak level."
  , ikit     = [("boiling vent", COrgan), ("fissure", COrgan)]
  }
biogasFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "biogas faucet"
  , ifreq    = [("robot", 33)]  -- very rare
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(3, 0), (5, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 5
               , AddSkills $ EM.fromList (zip [AbWait, AbMelee] [1, 1..])
               , AddArmorMelee 80, AddArmorRanged 80 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An emergency pressure-release vent on a liquefied biogas pipe."
  , ikit     = [("explosion vent", COrgan), ("fissure", COrgan)]
  }
medbotFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "nano medbot faucet"
  , ifreq    = [("robot", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 50, AddMaxCalm 999, AddSpeed 5
               , AddSkills $ EM.fromList (zip [AbWait, AbMelee] [1, 1..]) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A faucet of a malfunctioning nano medical robot dispenser. Let's hope the medbots are still effective."
  , ikit     = [("nano medbot vent", COrgan), ("fissure", COrgan)]
  }
surveillanceDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "surveillance drone"
  , ifreq    = [("robot", 100), ("horror", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 10)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 2, AddMaxCalm 60, AddSpeed 30
               , AddSkills
                 $ EM.fromList
                 $ zip [AbDisplace, AbMoveItem, AbProject, AbMelee] [-1, -1..]
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A video camera in each room would violate privacy of passengers, hence surveillance drones. Programmed to be easy to fend off, they keep a respectful distance."
  , ikit     = [ ("vision14", COrgan), ("robot brain", COrgan) ]
  }
shepherdDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "shepherd drone"
  , ifreq    = [("robot", 100), ("horror", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 7)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 2, AddMaxCalm 60, AddSpeed 30
               , AddSkills
                 $ EM.fromList
                 $ zip [AbDisplace, AbMoveItem, AbProject] [-1, -1..]
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A shabby drone for bringing cows home."
  , ikit     = [ ("eye 4", COrgan), ("live wire", COrgan)
               , ("robot brain", COrgan) ]
  }
huntingDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "hunting drone"
  , ifreq    = [("robot", 100), ("horror", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(3, 0), (5, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 500
  , iaspects = [ AddMaxHP 2, AddMaxCalm 60, AddSpeed 40
               , AddSkills
                 $ EM.fromList
                 $ zip [AbDisplace, AbMoveItem, AbMelee] [-1, -1..]
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Originally designed for hunting down and putting to sleep stray animals. The sleeping agent has long since dried up."
  , ikit     = [ ("eye 5", COrgan), ("needle", CInv)
               , ("robot brain", COrgan) ]
  }
homeRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "feral home robot"
               -- TODO: name another 'deranged', tertiary imperative: survival
  , ifreq    = [("robot", 100), ("horror", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(1, 20), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.singleton AbProject (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Once a timid household robot, it magnificently adapted to the deadly environment."
  , ikit     = [ ("fist", COrgan), ("eye 2", COrgan), ("nostril", COrgan)
               , ("robot brain", COrgan) ]
  }
wasteRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "waste disposal robot"
  , ifreq    = [ ("robot", 100), ("horror", 100), ("mobile robot", 100)
               , ("construction robot", 1) ]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 15, AddMaxCalm 60, AddSpeed 15 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "You are not in its database, hence you are waste."
  , ikit     = [ ("jaw", COrgan), ("tentacle", COrgan)
               , ("waste container", COrgan), ("armored skin", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan)
               , ("robot brain", COrgan) ]
  }
lightRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "decoration robot"
  , ifreq    = [ ("robot", 100), ("horror", 100), ("mobile robot", 100)
               , ("construction robot", 1) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(3, 1), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 15, AddMaxCalm 60, AddSpeed 30
               , AddSkills $ EM.singleton AbProject 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Interior and exterior decoration robot. Strongly fancies deep reds recently."
  , ikit     = [ ("claw", COrgan), ("tentacle", COrgan), ("spotlight", COrgan)
               , ("armored skin", COrgan), ("eye 5", COrgan)
               , ("robot brain", COrgan) ]
  }
heavyRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "construction robot"
  , ifreq    = [ ("robot", 100), ("horror", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(5, 1), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 800000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.singleton AbProject 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Heavy multi-purpose construction robot. Excels at discharging, dismantling and demolition."
  , ikit     = [ ("large jaw", COrgan), ("claw", COrgan), ("spotlight", COrgan)
               , ("construction hooter", CInv)
               , ("armored skin", COrgan), ("eye 5", COrgan)
               , ("robot brain", COrgan) ]
  }
cleanerRobot = ItemKind
  { isymbol  = 'C'
  , iname    = "The Void Cleaner Robot"
  , ifreq    = [("robot", 100), ("horror", 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(9 * 10/12, 0), (10 * 10/12, 1000), (11 * 10/12, 0)]
                 -- unique, appears at 10 of 12
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ Unique, AddMaxHP 80, AddMaxCalm 60, AddSpeed 18
               , AddSkills $ EM.singleton AbTrigger (-1) ]
                   -- can't switch levels, a miniboss
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A waste disposal robot repaired with parts from a heavy construction robot, including a scaled up goal matrix. The cosmic void is now the only acceptable model of cleanliness."
  , ikit     = [ ("waste container", COrgan), ("boiling vent", COrgan)
               , ("armored skin", COrgan), ("live wire", COrgan)
               , ("jaw", COrgan), ("claw", COrgan), ("armored skin", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan), ("spotlight", COrgan)
               , ("currency", CInv), ("currency", CInv), ("currency", CInv)
               , ("robot brain", COrgan) ]
  }
