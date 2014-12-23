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
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, pilot, engineer, doctor, soldier, sniper, clerk, hairdresser, lawyer, peddler, taxCollector, eye, fastEye, nose, elbow, torsor, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, cleanerRobot]

warrior,    pilot, engineer, doctor, soldier, sniper, clerk, hairdresser, lawyer, peddler, taxCollector, eye, fastEye, nose, elbow, torsor, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, cleanerRobot :: ItemKind

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
               , AddSkills $ EM.fromList [(AbProject, 1), (AbApply, 1)]
               , AddSight 3 ]  -- not via eyes, but feel, hearing, etc.
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("fist", COrgan), ("foot", COrgan), ("eye 4", COrgan)]
  }
pilot = warrior
  { iname    = "pilot" }
engineer = warrior
  { iname    = "engineer" }
doctor = warrior
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
  { isymbol  = 'w'
  , iname    = "beckoning walker"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(4, 6), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.fromList [(AbProject, 1), (AbApply, 1)]
               , AddSight 4 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Walks with a stately dignity. You read death in the slow beckoning gestures of its revolting upper appendages."
  , ikit     = [ ("foot", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("tentacle", COrgan)
               , ("eye 4", COrgan) ]
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
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30
               , AddSight 4 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It bites as blindingly fast as it runs. Or rolls? Or crawls? Also, cuts and pierces."
  , ikit     = [ ("tentacle", COrgan), ("tentacle", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("jaw", COrgan)
               , ("eye 4", COrgan), ("speed gland 10", COrgan) ]
  }
nose = ItemKind
  { isymbol  = 'h'
  , iname    = "tentacled horror"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(4, 5), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 30, AddSpeed 18
               , AddSkills $ EM.fromList [(AbProject, -1), (AbApply, -1)]
               , AddSmell 3 ]  -- depends solely on smell
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A blind, slimy mass of clawing, stinging and burning. You'd think it's mindless, but as soon as it touches your trembling body, it's always one step ahead."
  , ikit     = [ ("small claw", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("thorn", COrgan), ("sting", COrgan) ]
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
                 $ EM.fromList [(AbProject, 1), (AbApply, 1), (AbMelee, -1)]
               , AddSight 15 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It moves in sudden jerks and never makes a noise. Speaks in hard objects hurled at deadly speeds."
  , ikit     = [ ("speed gland 4", COrgan)
               , ("any arrow", CInv), ("any arrow", CInv)
               , ("any arrow", CInv), ("any arrow", CInv) ]
  }
torsor = ItemKind
  { isymbol  = 'M'
  , iname    = "The Maker of Contact"
  , ifreq    = [("alien", 100), ("horror", 100)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(10 * 11 / 12, 0), (10, 1000)]  -- unique
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
               , ("eye 4", COrgan)
               , ("gem", CInv), ("gem", CInv), ("gem", CInv), ("gem", CInv) ]
  }

-- * Animals

-- They need rather strong melee, because they don't use items.
-- Unless/until they level up.

animalSkillMalus :: Skills
animalSkillMalus =
  EM.fromList $ zip [AbDisplace, AbMoveItem, AbProject, AbApply] [-1, -1..]

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
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus
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
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 15, AddMaxCalm 60, AddSpeed 15
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(3, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 25, AddMaxCalm 60, AddSpeed 15
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus
               , AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("venom fang", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan) ]
  }
komodoDragon = ItemKind  -- bad hearing
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(5, 5), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 16
               , AddSkills animalSkillMalus, AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan), ("small claw", COrgan)
               , ("speed gland 4", COrgan), ("armored skin", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(4, 6), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 30
               , AddSkills animalSkillMalus, AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [("jaw", COrgan), ("eye 4", COrgan), ("nostril", COrgan)]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 35, AddMaxCalm 60, AddSpeed 17
               , AddArmorMelee 30, AddArmorRanged 30
               , AddSkills animalSkillMalus, AddSight 3 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = [ ("large jaw", COrgan), ("large tail", COrgan), ("claw", COrgan)
               , ("armored skin", COrgan), ("eye 4", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "The Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("horror", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(10 * 1 / 12, 1000), (10 * 2 / 12, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ Unique, AddMaxHP 70, AddMaxCalm 60, AddSpeed 25
               , AddSkills $ EM.insert AbTrigger (-1) animalSkillMalus ]
                   -- can't switch levels, a miniboss
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "The last of its kind. Blind with rage. Charges at deadly speed."
  , ikit     = [ ("armored skin", COrgan), ("eye 2", COrgan)
               , ("horn", COrgan), ("snout", COrgan) ]
  }

-- * Non-animal animals

hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [("animal", 100), ("horror", 100), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 5, AddMaxCalm 60, AddSpeed 30, AddSight 2
               , AddSkills $ EM.insert AbAlter (-1) animalSkillMalus
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

robotSkillMalus :: Skills
robotSkillMalus = EM.fromList $ zip [AbApply] [-1]

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
  , idesc    = "Marginally intelligent electric shepherd. Originally used in the spaceship's dairy farm and the zoo level."
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
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 20, AddSight 6
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList [(AbWait, 1), (AbProject, 2)]
               , AddArmorMelee 50, AddArmorRanged 50 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Makeshift, mostly non-lethal, autonomous perimeter defense outpost."
  , ikit     = [ ("needle", CInv), ("can of sticky foam", CInv)
               , ("can of sticky foam", CInv), ("can of sticky foam", CInv) ]
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
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 5
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait, AbMelee] [1, 1..])
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
  , irarity  = [(3, 0), (4, 1)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 5
               , AddSkills
                 $ EM.fromDistinctAscList (zip [minBound..maxBound] [-1, -1..])
                   `addSkills` EM.fromList (zip [AbWait] [1, 1..])
               , AddArmorMelee 80, AddArmorRanged 80 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An emergency pressure-release vent on a liquefied biogas pipe."
  , ikit     = [("explosion vent", COrgan)]
  }
shepherdDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "shepherd drone"
  , ifreq    = [("robot", 100), ("horror", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 2)]
  , iverbHit = "thud"
  , iweight  = 1000
  , iaspects = [ AddMaxHP 2, AddMaxCalm 60, AddSpeed 30, AddSight 4
               , AddSkills
                 $ EM.fromList
                 $ zip [AbApply, AbDisplace, AbMoveItem, AbProject] [-1, -1..]
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A shabby drone for bringing cows home."
  , ikit     = [("eye 2", COrgan), ("live wire", COrgan)]
  }
huntingDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "hunting drone"
  , ifreq    = [("robot", 100), ("horror", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(4, 0), (5, 1), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 500
  , iaspects = [ AddMaxHP 2, AddMaxCalm 60, AddSpeed 40, AddSight 4
               , AddSkills
                 $ EM.fromList
                 $ zip [AbApply, AbDisplace, AbMoveItem, AbMelee] [-1, -1..]
               , AddArmorMelee 90, AddArmorRanged 90 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Originally designed for hunting down and putting to sleep stray animals. The sleeping agent has long since dried up."
  , ikit     = [("eye 2", COrgan), ("needle", CInv)]
  }
homeRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "feral home robot"
               -- TODO: name another 'deranged', tertiary imperative: survival
  , ifreq    = [("robot", 100), ("horror", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 10, AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.insert AbProject (-1) robotSkillMalus
               , AddSmell 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Once a timid household robot, it magnificently adapted to the deadly environment."
  , ikit     = [("fist", COrgan), ("eye 2", COrgan)]
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
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 15
               , AddSmell 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "You are not in its database, hence you are waste."
  , ikit     = [ ("jaw", COrgan), ("tentacle", COrgan)
               , ("waste container", COrgan)
               , ("armored skin", COrgan), ("eye 3", COrgan) ]
  }
lightRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "decoration robot"
  , ifreq    = [ ("robot", 100), ("horror", 100), ("mobile robot", 100)
               , ("construction robot", 1) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 8), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ AddMaxHP 20, AddMaxCalm 60, AddSpeed 30
               , AddSkills $ EM.insert AbProject 1 robotSkillMalus
               , AddSight 3, AddLight 3 ]  -- light can't be turned off
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Interior and exterior decoration robot. Strongly fancies deep reds recently."
  , ikit     = [ ("claw", COrgan), ("tentacle", COrgan)
               , ("armored skin", COrgan), ("eye 4", COrgan) ]
  }
heavyRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "construction robot"
  , ifreq    = [ ("robot", 100), ("horror", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 4), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 800000
  , iaspects = [ AddMaxHP 40, AddMaxCalm 60, AddSpeed 20
               , AddSkills $ EM.insert AbProject 1 robotSkillMalus
               , AddSight 3, AddLight 4 ]  -- light can't be turned off
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Heavy multi-purpose construction robot. Excels at discharging, dismantling and demolition."
  , ikit     = [ ("large jaw", COrgan), ("claw", COrgan)
               , ("construction hooter", CInv)
               , ("armored skin", COrgan), ("eye 3", COrgan) ]
  }
cleanerRobot = ItemKind
  { isymbol  = 'C'
  , iname    = "The Void Cleaner Robot"
  , ifreq    = [("robot", 100), ("horror", 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(10 * 9 / 12, 0), (10 * 10 / 12, 1000), (10 * 11 / 12, 0)]
                 -- unique, appears at 10 of 12
  , iverbHit = "thud"
  , iweight  = 80000
  , iaspects = [ Unique, AddMaxHP 80, AddMaxCalm 60, AddSpeed 18
               , AddSkills $ EM.insert AbTrigger (-1) robotSkillMalus
                   -- can't switch levels, a miniboss
               , AddSmell 2, AddLight 4 ]  -- light can't be turned off
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A waste disposal robot repaired with parts from a heavy construction robot, including a scaled up goal matrix. The cosmic void is now the only acceptable model of cleanliness."
  , ikit     = [ ("waste container", COrgan), ("boiling vent", COrgan)
               , ("armored skin", COrgan), ("live wire", COrgan)
               , ("jaw", COrgan), ("claw", COrgan)
               , ("armored skin", COrgan), ("eye 3", COrgan)
               , ("currency", CInv), ("currency", CInv), ("currency", CInv) ]
  }
