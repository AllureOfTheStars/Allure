-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2017 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( actors
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush]
  -- Allure-specific
  ++ [razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, cleanerRobot]

warrior,    warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush :: ItemKind
-- Allure-specific
razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, cleanerRobot :: ItemKind

-- * Hunams

warrior = ItemKind
  { isymbol  = '@'
  , iname    = "mercenary"  -- modified if initial actors in hero faction
  , ifreq    = [("hero", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 80  -- partially from clothes and assumed first aid
               , AddMaxCalm 70, AddSpeed 20, AddNocto 2
               , AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A hardened veteran of combat."
  , ikit     = [ ("fist", COrgan), ("foot", COrgan), ("eye 6", COrgan)
               , ("sapient brain", COrgan) ]
  }
warrior2 = warrior
  { iname    = "pilot"
  -- , idesc    = ""
  }
warrior3 = warrior
  { iname    = "engineer"
  -- , idesc    = ""
   }
warrior4 = warrior
  { iname    = "doctor"
  -- , idesc    = ""
  }
warrior5 = warrior
  { iname    = "hacker"
  -- , idesc    = ""
  }

scout = warrior
  { iname    = "scout"
  , ifreq    = [("scout hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("add sight", CEqp)
                  , ("armor ranged", CEqp)
                  , ("add nocto 1", CInv) ]
  -- , idesc    = ""
  }
ranger = warrior
  { iname    = "ranger"
  , ifreq    = [("ranger hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior ++ [("weak arrow", CInv), ("armor ranged", CEqp)]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [("escapist hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("add sight", CEqp)
                  , ("weak arrow", CInv)  -- mostly for probing
                  , ("armor ranged", CEqp)
                  , ("flask", CInv)
                  , ("light source", CInv)
                  , ("blanket", CInv) ]
  -- , idesc    = ""
  }
ambusher = warrior
  { iname    = "ambusher"
  , ifreq    = [("ambusher hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior  -- dark and numerous, so more kit without exploring
               ++ [ ("ring of opportunity sniper", CEqp)
                  , ("light source", CEqp), ("wooden torch", CInv)
                  , ("weak arrow", CInv), ("any arrow", CSha), ("flask", CSha) ]
  -- , idesc    = ""
  }
soldier = warrior
  { iname    = "soldier"
  , ifreq    = [("soldier hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior ++ [("starting weapon", CEqp)]
  -- , idesc    = ""
  }

civilian = warrior
  { iname    = "clerk"
  , ifreq    = [("civilian", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrBlack]
  -- , idesc    = ""
  }
civilian2 = civilian
  { iname    = "hairdresser"
  -- , idesc    = ""
  }
civilian3 = civilian
  { iname    = "lawyer"
  -- , idesc    = ""
  }
civilian4 = civilian
  { iname    = "peddler"
  -- , idesc    = ""
  }
civilian5 = civilian
  { iname    = "tax collector"
  -- , idesc    = ""
  }

-- * Aliens

-- They have bright colours, because they are not natural.

eye = ItemKind
  { isymbol  = 'w'
  , iname    = "beckoning walker"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 10) ]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(4, 6), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 70, AddSpeed 20, AddNocto 2
               , AddAggression 1
               , AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Walks with a stately dignity. You read death in the slow beckoning gestures of its revolting upper appendages."
  , ikit     = [ ("foot", COrgan), ("tentacle", COrgan)
               , ("eye 6", COrgan)
               , ("sapient brain", COrgan) ]
  }
fastEye = ItemKind
  { isymbol  = 'b'
  , iname    = "crawling biter"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(4, 3), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 5, AddMaxCalm 70, AddSpeed 30, AddNocto 2
               , AddAggression 1
               , AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It bites as blindingly fast as it runs. Or rolls? Or crawls? Also, cuts and pierces."
  , ikit     = [ ("tentacle", COrgan), ("jaw", COrgan)
               , ("eye 3", COrgan), ("speed gland 10", COrgan)
               , ("sapient brain", COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'h'
  , iname    = "tentacled horror"
  , ifreq    = [("monster", 100), ("mobile", 1), ("mobile monster", 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(4, 5), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 30, AddMaxCalm 30, AddSpeed 16, AddNocto 2
               , AddAggression 1
               , AddAbility AbProject (-1), AddAbility AbAlter 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A blind, slimy mass of clawing, stinging and burning. You'd think it's powerless, but as soon as it touches your trembling body, it's always one step ahead."
  , ikit     = [ ("nostril", COrgan), ("small claw", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("thorn", COrgan), ("venom tooth", COrgan)
               , ("sapient brain", COrgan) ]
  }
elbow = ItemKind
  { isymbol  = 's'
  , iname    = "creepy shooter"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(6, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 12, AddMaxCalm 80, AddSpeed 21, AddNocto 2
               , AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter 2, AddAbility AbMelee (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It moves in sudden jerks and never makes a noise. Speaks in hard objects hurled at deadly speeds."
  , ikit     = [ ("speed gland 4", COrgan)
               , ("eye 8", COrgan)
               , ("any arrow", CSha), ("any arrow", CInv)
               , ("weak arrow", CInv), ("weak arrow", CInv)
               , ("sapient brain", COrgan) ]
  }
torsor = ItemKind
  { isymbol  = 'M'
  , iname    = "The Maker of Contact"
  , ifreq    = [("monster", 100), ("mobile", 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(11 * 10/12, 0), (12 * 10/12, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 300, AddMaxCalm 100, AddSpeed 6, AddNocto 2
               , AddAggression 3
               , AddAbility AbProject 2, AddAbility AbApply 1
               , AddAbility AbAlter (-1) ]  -- can't switch levels, the boss
  , ieffects = [Unique]
  , ifeature = [Durable, Identified]
  , idesc    = "The mind, the heart behind it all. Warmth and sympathy pour out through the graceful undulation of tentacles, sharp claws, snapping jaw, grinding teeth and tensing fangs."
  , ikit     = [ ("tentacle", COrgan), ("hooked claw", COrgan)
               , ("large jaw", COrgan)
               , ("sting", COrgan), ("venom fang", COrgan)
               , ("eye 6", COrgan), ("speed gland 4", COrgan)
               , ("gem", CInv), ("gem", CInv), ("gem", CInv), ("gem", CInv)
               , ("sapient brain", COrgan) ]
  }

-- * Animals

-- They need rather strong melee, because they don't use items.
-- Unless/until they level up.

-- They have dull colors, except for yellow, because there is no dull variant.

goldenJackal = ItemKind  -- basically a much smaller and slower hyena
  { isymbol  = 'j'
  , iname    = "golden jackal"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 50) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 12, AddMaxCalm 70, AddSpeed 24, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ ("small jaw", COrgan), ("eye 6", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
griffonVulture = ItemKind
  { isymbol  = 'v'
  , iname    = "griffon vulture"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 30) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 12, AddMaxCalm 80, AddSpeed 22, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
      -- Animals don't have leader, usually, so even if only one of level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo. Random double moves,
      -- on either side, are just too frustrating.
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ ("screeching beak", COrgan)  -- in reality it grunts and hisses
               , ("small claw", COrgan), ("eye 7", COrgan)
               , ("animal brain", COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 4000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 10, AddMaxCalm 30, AddSpeed 22, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ ("scent gland", COrgan)
               , ("small claw", COrgan), ("snout", COrgan)
               , ("nostril", COrgan), ("eye 3", COrgan)
               , ("animal brain", COrgan) ]
  }
armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 10, AddMaxCalm 30, AddSpeed 20, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ ("hooked claw", COrgan), ("snout", COrgan)
               , ("armored skin", COrgan), ("armored skin", COrgan)
               , ("nostril", COrgan), ("eye 3", COrgan)
               , ("animal brain", COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 12, AddMaxCalm 50, AddSpeed 18, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(5, 1), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 25, AddMaxCalm 60, AddSpeed 16, AddNocto 2
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ ("venom fang", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
komodoDragon = ItemKind  -- bad hearing; regeneration makes it very powerful
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(9, 0), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 41, AddMaxCalm 60, AddSpeed 18, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Larger and more aggressive than any other lizard."
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan)
               , ("hooked claw", COrgan), ("speed gland 4", COrgan)
               , ("armored skin", COrgan), ("eye 3", COrgan)
               , ("nostril", COrgan), ("animal brain", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 70, AddSpeed 32, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ ("jaw", COrgan), ("eye 6", COrgan), ("nostril", COrgan)
               , ("animal brain", COrgan) ]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(8, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 41, AddMaxCalm 70, AddSpeed 18, AddNocto 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An armored predator from the dawn of time."
  , ikit     = [ ("large jaw", COrgan), ("large tail", COrgan)
               , ("small claw", COrgan)
               , ("armored skin", COrgan), ("eye 6", COrgan)
               , ("animal brain", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "The Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1 * 10/12, 1000000), (2 * 10/12, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 90, AddMaxCalm 60, AddSpeed 27, AddNocto 2
               , AddAggression 2
               , AddAbility AbAlter (-1) ]  -- can't switch levels, a miniboss
  , ieffects = [Unique]
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
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 8, AddMaxCalm 60
               , AddSpeed 30, AddNocto 2  -- armor in sting
               , AddAbility AbAlter (-2) ]  -- can't use stairs nor doors
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ ("bee sting", COrgan), ("vision 6", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 8, AddMaxCalm 70, AddSpeed 30, AddNocto 2
               , AddAbility AbAlter (-2)  -- can't use stairs nor doors
               , AddArmorMelee 80, AddArmorRanged 40 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ ("sting", COrgan), ("vision 8", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [("animal", 20), ("immobile animal", 30)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 22, AddNocto 2
               , AddAbility AbWait 1, AddAbility AbMelee 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Each branch bears long, curved thorns."
  , ikit     = [("thorn", COrgan), ("armored skin", COrgan)]
  }

-- * Robots, Allure-specific

-- Robots have any colors but only f, d and r letters. Avoid these letters
-- for other factions.

razorwireFence = ItemKind
  { isymbol  = 'f'
  , iname    = "razorwire fence"
  , ifreq    = [("robot", 10), ("immobile robot", 10)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(3 * 10/12, 0), (4 * 10/12, 10)]  -- quickly vanishes at depth
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 20, AddNocto 2
               , AddAbility AbWait 1, AddAbility AbMelee 1
               , AddArmorMelee 40, AddArmorRanged 20 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Must have been bought by previous ship owners to contain the wild animal infestation."
  , ikit     = [("razor", COrgan)]
  }
electricFence = ItemKind
  { isymbol  = 'f'
  , iname    = "electric fence"
  , ifreq    = [("robot", 10), ("immobile robot", 10)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(3 * 10/12, 0), (4 * 10/12, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 10, AddMaxCalm 999, AddSpeed 40, AddNocto 2
               , AddAbility AbWait 1, AddAbility AbMelee 1
               , AddArmorMelee 40, AddArmorRanged 20 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Marginally intelligent electric shepherd. Originally used in orbital dairy farms and planetary zoos."
  , ikit     = [("live wire", COrgan)]
  }
activeFence = ItemKind
  { isymbol  = 'f'
  , iname    = "active fence"
  , ifreq    = [("robot", 30), ("immobile robot", 30)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(5 * 10/12, 0), (6 * 10/12, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 20, AddNocto 2
               , AddAbility AbWait 1, AddAbility AbProject 3
               , AddArmorMelee 40, AddArmorRanged 20 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Makeshift, mostly non-lethal, autonomous perimeter defense outpost."
  , ikit     = [ ("vision 6", COrgan)
               , ("needle", CInv), ("can of sticky foam", CInv) ]
  }
steamFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "steam faucet"
  , ifreq    = [("robot", 30), ("immobile robot", 50)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 3), (5 * 10/12, 3)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 20, AddMaxCalm 999, AddSpeed 10, AddNocto 2
               , AddAbility AbWait 1, AddAbility AbMelee 1
               , AddArmorMelee 40, AddArmorRanged 20 ]  -- hard material
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A cracked valve on one of the superheated water pipes spreading radially outward from the tokamak level."
  , ikit     = [("boiling vent", COrgan), ("boiling fissure", COrgan)]
  }
biogasFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "biogas faucet"
  , ifreq    = [("robot", 10), ("immobile robot", 30)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 10), (5 * 10/12, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 22
               , AddNocto 2, AddShine 3
               , AddAbility AbWait 1, AddAbility AbMelee 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "An emergency pressure-release vent on a smelly biogas pipe."
  , ikit     = [("biogas vent", COrgan), ("biogas fissure", COrgan)]
  }
medbotFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "nano medbot faucet"
  , ifreq    = [("robot", 10), ("immobile robot", 110)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 10), (5 * 10/12, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 30, AddMaxCalm 999, AddSpeed 22
               , AddNocto 2, AddShine 3
               , AddAbility AbWait 1, AddAbility AbMelee 1 ]
  , ieffects = []
  , ifeature = [Durable, Identified]  -- TODO: only heal humans
  , idesc    = "A faucet of a malfunctioning nano medical robot dispenser. Let's hope the medbots are still effective."
  , ikit     = [("medbot vent", COrgan), ("medbot fissure", COrgan)]
  }
surveillanceDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "surveillance drone"
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = []  -- TODO: too boring
  , iverbHit = "thud"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 6, AddMaxCalm 90, AddSpeed 30, AddNocto 2
               , AddAbility AbDisplace (-1), AddAbility AbMoveItem (-1)
               , AddAbility AbProject (-1), AddAbility AbMelee (-1)
               , AddArmorMelee 40, AddArmorRanged 20 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A video camera in each room would violate privacy of passengers, hence surveillance drones. Programmed to be easy to fend off, they keep a respectful distance."
  , ikit     = [ ("vision 14", COrgan), ("robot brain", COrgan) ]
  }
shepherdDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "shepherd drone"
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 7)]
  , iverbHit = "thud"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 3, AddMaxCalm 60, AddSpeed 30, AddNocto 2
               , AddAbility AbDisplace (-1), AddAbility AbMoveItem (-1)
               , AddAbility AbProject (-1)
               , AddArmorMelee 80, AddArmorRanged 40 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "A shabby drone for bringing cows home."
  , ikit     = [ ("eye 4", COrgan), ("live wire", COrgan)
               , ("robot brain", COrgan) ]
  }
huntingDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "hunting drone"
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(3, 0), (5, 2), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 500
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 6, AddMaxCalm 60, AddSpeed 40, AddNocto 2
               , AddAbility AbDisplace (-1), AddAbility AbMoveItem (-1)
               , AddAbility AbMelee (-1)
               , AddArmorMelee 40, AddArmorRanged 20 ]
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
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(1, 20), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 10, AddMaxCalm 30, AddSpeed 20, AddNocto 2
               , AddAbility AbProject (-1) ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Once a timid household robot, now sufficiently adapted to survive in the deadly environment."
  , ikit     = [ ("fist", COrgan), ("eye 2", COrgan), ("nostril", COrgan)
               , ("robot brain", COrgan) ]
  }
wasteRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "waste disposal robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 50) ]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 15, AddMaxCalm 30, AddSpeed 15, AddNocto 2 ]
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
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(3, 1), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 15, AddMaxCalm 60, AddSpeed 30, AddNocto 2
               , AddAbility AbProject 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Interior and exterior decoration robot. Strongly fancies deep reds recently."
  , ikit     = [ ("hooked claw", COrgan), ("tentacle", COrgan)
               , ("spotlight", COrgan), ("armored skin", COrgan)
               , ("eye 5", COrgan), ("robot brain", COrgan) ]
  }
heavyRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "construction robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(8, 0), (10, 10)]
  , iverbHit = "thud"
  , iweight  = 800000
  , idamage  = toDmg 0
  , iaspects = [ AddMaxHP 41, AddMaxCalm 60, AddSpeed 20, AddNocto 2
               , AddAbility AbProject 2 ]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = "Heavy multi-purpose construction robot. Excels at discharging, dismantling and demolition."
  , ikit     = [ ("large jaw", COrgan), ("small claw", COrgan), ("spotlight", COrgan)
               , ("construction hooter", CInv)
               , ("armored skin", COrgan), ("eye 4", COrgan)
               , ("robot brain", COrgan) ]
  }
cleanerRobot = ItemKind
  { isymbol  = 'C'
  , iname    = "The Void Cleaner Robot"
  , ifreq    = [("robot", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(9 * 10/12, 0), (10 * 10/12, 1000), (11 * 10/12, 0)]
                 -- unique, appears at 10 of 12
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = toDmg 0
  , iaspects = [AddMaxHP 120, AddMaxCalm 60, AddSpeed 18, AddNocto 2]
                 -- can't exit the gated level, a miniboss
  , ieffects = [Unique]
  , ifeature = [Durable, Identified]
  , idesc    = "A waste disposal robot repaired with parts from a heavy construction robot, including a scaled up goal matrix. The cosmic void is now the only acceptable model of cleanliness."
  , ikit     = [ ("waste container", COrgan), ("boiling vent", COrgan)
               , ("armored skin", COrgan), ("live wire", COrgan)
               , ("jaw", COrgan), ("hooked claw", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan), ("spotlight", COrgan)
               , ("currency", CInv), ("currency", CInv), ("currency", CInv)
               , ("robot brain", COrgan) ]
  }
