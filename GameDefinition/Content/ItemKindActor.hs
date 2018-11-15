-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
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
import Game.LambdaHack.Common.Container
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Content.ItemKind

actors :: [ItemKind]
actors =
  [warrior, warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, giantOctopus, rhinoceros, beeSwarm, hornetSwarm, thornbush]
  -- Allure-specific
  ++ [razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, weldedRobot, cleanerRobot]

warrior,    warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, komodoDragon, hyena, alligator, giantOctopus, rhinoceros, beeSwarm, hornetSwarm, thornbush :: ItemKind
-- Allure-specific
razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, weldedRobot, cleanerRobot :: ItemKind

-- Note that the actors that appear in the crawl scenario should
-- be generated with at most ordinary ammo. Otherwise, farming them
-- may be rational though boring endeavour. Any exceptions to that
-- should be well thought of. E.g., unique guaranteed items on bosses
-- are safe, just as restricted kinds of weak items.

-- * Hunams

warrior = ItemKind
  { isymbol  = '@'
  , iname    = "mercenary"  -- modified if initial actors in hero faction
  , ifreq    = [("hero", 100), ("crawl hero", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 80  -- partially from clothes and first aid
               , AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20
               , AddSkill SkNocto 2
               , AddSkill SkProject 2
               , AddSkill SkApply 1
               , AddSkill SkAlter 2
               , AddSkill SkOdor 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = ""
  -- , idesc    = "A hardened veteran of combat."
  , ikit     = [ ("fist", COrgan), ("foot", COrgan)
               , ("eye 6", COrgan), ("ear 4", COrgan)
               , ("sapient brain", COrgan) ]
  }
warrior2 = warrior
  { iname    = "pilot"
  -- , idesc    = ""
  }
warrior3 = warrior
  { iname    = "engineer"
  , ifreq    = [("crawl hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior ++ [("currency", CSha)]
  -- , idesc    = ""
  }
warrior4 = warrior
  { iname    = "doctor"
  , ifreq    = [("crawl hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior ++ [("currency", CSha)]
  -- , idesc    = ""
  }
warrior5 = warrior
  { iname    = "hacker"
  , ifreq    = [("crawl hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior ++ [("currency", CSha)]
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
  , ikit     = ikit warrior
               ++ [ ("armor ranged", CEqp)
                  , ("weak arrow", CInv) ]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [("escapist hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("add sight", CEqp)
                  , ("armor ranged", CEqp)
                  , ("weak arrow", CInv)  -- mostly for probing
                  , ("light source", CInv)
                  , ("wooden torch", CInv)
                  , ("blanket", CInv) ]
  -- , idesc    = ""
  }
ambusher = warrior
  { iname    = "ambusher"
  , ifreq    = [("ambusher hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior  -- dark and numerous, so more kit without exploring
               ++ [ ("ring of opportunity sniper", CEqp)
                  , ("any arrow", CSha)
                  , ("weak arrow", CInv)
                  , ("explosive", CSha)
                  , ("light source", CEqp)
                  , ("wooden torch", CInv) ]
  -- , idesc    = ""
  }
soldier = warrior
  { iname    = "soldier"
  , ifreq    = [("soldier hero", 100), ("mobile", 1)]
  , ikit     = ikit warrior
               ++ [ ("starting weapon", CEqp)
                  , ("explosive", CSha) ]
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
  , irarity  = [(3, 0), (4, 10), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject 2, AddSkill SkApply 1, AddSkill SkAlter 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Walks with a stately dignity. You read death in the slow beckoning gestures of its revolting upper appendages."
  , ikit     = [ ("foot", COrgan), ("tentacle", COrgan)
               , ("eye 6", COrgan)
               , ("sapient brain", COrgan) ]  -- no voice, no hearing
  }
fastEye = ItemKind
  { isymbol  = 'b'
  , iname    = "rolling biter"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(3, 0), (4, 3), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 5, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkAlter 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It bites as blindingly fast as it runs. Or rolls? Or crawls? Also, cuts and pierces."
  , ikit     = [ ("tentacle", COrgan), ("jaw", COrgan)
               , ("speed gland 10", COrgan)
               , ("eye 3", COrgan), ("ear 4", COrgan)
               , ("sapient brain", COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'h'
  , iname    = "tentacled horror"
  , ifreq    = [ ("monster", 100), ("mobile", 1), ("mobile monster", 100)
               , ("aquatic", 30), ("aquatic monster", 30) ]  -- likes liquids
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(3, 0), (4, 5), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject (-1), AddSkill SkAlter 2
               , AddSkill SkSwimming 30
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A blind, slimy mass of clawing, stinging and burning. You'd think it's powerless, but as soon as it touches your trembling body, it's always one step ahead."
  , ikit     = [ ("nostril", COrgan), ("small claw", COrgan)
               , ("tentacle", COrgan), ("tentacle", COrgan)
               , ("thorn", COrgan), ("venom tooth", COrgan)
               , ("sapient brain", COrgan) ]  -- no sight nor hearing
  }
elbow = ItemKind
  { isymbol  = 's'
  , iname    = "creepy shooter"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(3, 0), (4, 1), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 21, AddSkill SkNocto 2
               , AddSkill SkProject 2, AddSkill SkApply 1
               , AddSkill SkAlter 2, AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It moves in sudden jerks and never makes a noise. Speaks in hard objects hurled at deadly speeds."
  , ikit     = [ ("speed gland 4", COrgan)
               , ("eye 8", COrgan), ("ear 10", COrgan)
               , ("any arrow", CSha), ("any arrow", CInv)
               , ("weak arrow", CInv), ("weak arrow", CInv)
               , ("sapient brain", COrgan) ]
  }
torsor = ItemKind
  { isymbol  = 'M'
  , iname    = "Maker"
  , ifreq    = [("monster", 100), ("mobile", 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(11 * 10/12, 0), (12 * 10/12, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique, ELabel "of Contact"
               , AddSkill SkMaxHP 300, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 6, AddSkill SkNocto 2
               , AddSkill SkAggression 3
               , AddSkill SkProject 2, AddSkill SkApply 1
               , AddSkill SkAlter 1  -- can't exit the gated level, the boss
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The mind, the heart behind it all. Warmth and sympathy pour out through the graceful undulation of tentacles, sharp claws, snapping jaw, grinding teeth and tensing fangs."
  , ikit     = [ ("tentacle", COrgan), ("hooked claw", COrgan)
               , ("large jaw", COrgan), ("sting", COrgan)
               , ("venom fang", COrgan), ("speed gland 4", COrgan)
               , ("eye 6", COrgan), ("ear 9", COrgan)
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
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 24, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ ("small jaw", COrgan)
               , ("eye 6", COrgan), ("nostril", COrgan), ("ear 9", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
      -- Animals don't have leader, usually, so even if only one of level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo. Random double moves,
      -- on either side, are just too frustrating.
  , ieffects = []
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ ("screeching beak", COrgan)  -- in reality it grunts and hisses
               , ("small claw", COrgan)
               , ("eye 7", COrgan), ("ear 10", COrgan)
               , ("animal brain", COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 7), (5, 1)]
  , iverbHit = "thud"
  , iweight  = 4000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkOdor 5  -- and no smell skill, to let it leave smell
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ ("scent gland", COrgan)
               , ("small claw", COrgan), ("snout", COrgan)
               , ("eye 3", COrgan), ("ear 7", COrgan)
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
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ ("hooked claw", COrgan), ("snout", COrgan)
               , ("armored skin", COrgan), ("armored skin", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
               , ("animal brain", COrgan) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(2, 5), (10, 2)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 50
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 5", COrgan)
               , ("animal brain", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(6, 1), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 25, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ ("venom fang", COrgan), ("rattle", COrgan)
               , ("eye 4", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
               , ("animal brain", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 4)]  -- gets summoned often, so low base rarity
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 32, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ ("jaw", COrgan)
               , ("eye 6", COrgan), ("nostril", COrgan), ("ear 9", COrgan)
               , ("animal brain", COrgan) ]
  }
komodoDragon = ItemKind
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [BrRed]  -- speedy, so bright red
  , icount   = 1
  , irarity  = [(9, 0), (10, 11)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Larger and more aggressive than any other lizard, but as easily recovering from wounds at its lesser cousins."
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan)
               , ("hooked claw", COrgan), ("speed gland 4", COrgan)
               , ("armored skin", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 4", COrgan)
               , ("animal brain", COrgan) ]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("aquatic", 70), ("aquatic animal", 70) ]  -- amphibious
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(9, 0), (10, 10)]  -- extra spawns in water, so lower rarity
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 45, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkSwimming 100  -- swims better than walks
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An armored predator from the dawn of time. You better not get within its reach."
  , ikit     = [ ("huge tail", COrgan), ("large jaw", COrgan)
               , ("small claw", COrgan), ("armored skin", COrgan)
               , ("eye 6", COrgan), ("ear 9", COrgan)
               , ("animal brain", COrgan) ]
  }
giantOctopus = ItemKind
  { isymbol  = 'o'
  , iname    = "giant octopus"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("aquatic", 90), ("aquatic animal", 90) ]  -- weak on land
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 5)]
  , iverbHit = "thud"
  , iweight  = 72000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 14, AddSkill SkMaxCalm 80
               , AddSkill SkSwimming 100  -- swims better than walks
               , AddSkill SkSpeed 27, AddSkill SkNocto 3 -- good night vision
               , AddSkill SkAlter (-2), SetFlag Durable ]  -- can't use stairs nor doors
  , ieffects = []
  , idesc    = "It has eight arms of rage."
  , ikit     = [ ("tentacle", COrgan), ("tentacle", COrgan)
               , ("small beak", COrgan), ("eye 7", COrgan)
               , ("animal brain", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(6 * 10/12, 0), (7 * 10/12, 1000), (8 * 10/12, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 120, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 27, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-1)  -- can't switch levels, a miniboss;
                                        -- also easy to contain with doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The last of its kind. Blind with rage or perhaps due to the postoperative scars. Charges at deadly speed."
  , ikit     = [ ("armored skin", COrgan)
               , ("eye 2", COrgan), ("ear 7", COrgan)
               , ("rhino horn", COrgan), ("snout", COrgan)
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
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2  -- armor in sting
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ ("bee sting", COrgan), ("vision 6", COrgan), ("ear 6", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 4)]  -- should be many, because die after a time
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkMaxHP 8, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ ("sting", COrgan), ("vision 8", COrgan), ("ear 7", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [("animal", 20), ("immobile animal", 40)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 14)]
  , iverbHit = "scrape"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
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
  , irarity  = [(3 * 10/12, 0), (4 * 10/12, 15)]  -- quickly vanishes at depth
  , iverbHit = "scrape"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 30, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Must have been bought by previous ship owners to contain the wild animal infestation."
  , ikit     = [("razor", COrgan)]
  }
electricFence = ItemKind
  { isymbol  = 'f'
  , iname    = "electric fence"
  , ifreq    = [("robot", 10), ("immobile robot", 10)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(3 * 10/12, 0), (4 * 10/12, 15)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 10, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 40, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Marginally intelligent electric shepherd. Originally used in orbital dairy farms and planetary zoos."
  , ikit     = [("live wire", COrgan)]
  }
activeFence = ItemKind
  { isymbol  = 'f'
  , iname    = "active fence"
  , ifreq    = [("robot", 25), ("immobile robot", 30)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(5 * 10/12, 0), (10, 4)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkProject 3  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Makeshift, mostly non-lethal, autonomous perimeter defense outpost."
  , ikit     = [ ("vision 6", COrgan)
               , ("needle", CInv), ("can of sticky foam", CInv) ]
                   -- can of sticky foam is scummable, but on the level
                   -- it appears healing through faucets is much more so,
                   -- so it won't be a significant factor
  }
steamFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "steam faucet"
  , ifreq    = [("robot", 7), ("immobile robot", 15)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 11, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A cracked valve on one of the superheated water pipes spreading radially outward from the tokamak level."
  , ikit     = [("boiling vent", COrgan), ("boiling fissure", COrgan)]
  }
biogasFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "biogas faucet"
  , ifreq    = [("robot", 7), ("immobile robot", 30)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2, AddSkill SkShine 3
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An emergency pressure-release vent on a smelly biogas pipe."
  , ikit     = [("biogas vent", COrgan), ("biogas fissure", COrgan)]
  }
medbotFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "nano medbot faucet"
  , ifreq    = [("robot", 7), ("immobile robot", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2, AddSkill SkShine 3
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A faucet of a malfunctioning nano medical robot dispenser. Let's hope the medbots are still effective."
  , ikit     = [("medbot vent", COrgan), ("medbot fissure", COrgan)]
  }
surveillanceDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "surveillance drone"
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "clank"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 6, AddSkill SkMaxCalm 90
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkDisplace (-1), AddSkill SkMoveItem (-1)
               , AddSkill SkProject (-1), AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
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
  , iverbHit = "clank"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkMaxHP 3, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkDisplace (-1), AddSkill SkMoveItem (-1)
               , AddSkill SkProject (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A shabby drone for bringing cows home."
  , ikit     = [ ("live wire", COrgan), ("eye 4", COrgan), ("ear 6", COrgan)
               , ("robot brain", COrgan) ]
  }
huntingDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "hunting drone"
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(4, 0), (5, 1), (10, 4)]
  , iverbHit = "clank"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 6, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 40, AddSkill SkNocto 2
               , AddSkill SkDisplace (-1), AddSkill SkMoveItem (-1)
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Originally designed for hunting down and putting to sleep stray animals. The sleeping agent has long since dried up."
  , ikit     = [ ("eye 5", COrgan), ("nostril", COrgan), ("ear 8", COrgan)
               , ("needle", CInv)
               , ("robot brain", COrgan) ]
  }
homeRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "feral home robot"
               -- TODO: name another 'deranged', tertiary imperative: survival
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(1, 20), (10, 5)]
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject (-1), AddSkill SkAlter 1  -- doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Once a timid household robot, now sufficiently adapted to survive in the deadly environment."
  , ikit     = [ ("fist", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan), ("ear 4", COrgan)
               , ("robot brain", COrgan) ]
  }
wasteRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "waste disposal robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 50) ]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(5, 8)]  -- gets summoned quite often, so low base rarity
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 15, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "You are not in its database, hence you are waste."
  , ikit     = [ ("jaw", COrgan), ("tentacle", COrgan)
               , ("waste container", COrgan), ("armored skin", COrgan)
               , ("nostril", COrgan)  -- only smell, for variety
               , ("robot brain", COrgan) ]
  }
lightRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "decoration robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(3, 0), (4, 1), (10, 7)]  -- gets summoned often, so low rarity
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 40
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkProject 2, AddSkill SkAlter 2  -- uses all stairs
               , AddSkill SkApply 1  -- apply the hooter
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Interior and exterior decoration robot. Strongly fancies deep reds recently."
  , ikit     = [ ("hooked claw", COrgan), ("tentacle", COrgan)
               , ("spotlight", COrgan), ("armored skin", COrgan)
               , ("eye 5", COrgan), ("ear 8", COrgan)
               , ("robot brain", COrgan)
               , ("construction hooter", CEqp) ]
  }
heavyRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "construction robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(8, 0), (9, 5), (10, 15)]
  , iverbHit = "clank"
  , iweight  = 800000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 41, AddSkill SkMaxCalm 40
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject 2, AddSkill SkAlter 2  -- uses all stairs
               , AddSkill SkApply 1  -- apply the hooter
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Heavy multi-purpose construction robot. Excels at discharging, dismantling and demolition."
  , ikit     = [ ("large jaw", COrgan), ("small claw", COrgan)
               , ("spotlight", COrgan), ("armored skin", COrgan)
               , ("eye 4", COrgan), ("ear 6", COrgan)
               , ("robot brain", COrgan)
               , ("construction hooter", CEqp) ]
  }
weldedRobot = ItemKind
  { isymbol  = 'W'
  , iname    = "Smiling Welded Robot"
  , ifreq    = [("robot", 100), ("immobile robot", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1 * 10/12, 1000), (2 * 10/12, 0)]  -- unique
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 200, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 2  -- provoke to give the blowtorch
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A well-built humanoid luggage unloading robot with a smooth satin silvery skin. Its graceful moves are stunted by a thick irregular weld fastening both its shapely legs to the floor. A whiff of smoke escapes whenever it opens its mouth in a charming toothy smile while brandishing a blowtorch in its trembling hand."
  , ikit     = [ ("mouth vent", COrgan), ("small jaw", COrgan)
               , ("fist", COrgan), ("eye 6", COrgan), ("ear 4", COrgan)
               , ("robot brain", COrgan)
               , ("blowtorch", CEqp), ("crude weld", COrgan) ]
  }
cleanerRobot = ItemKind
  { isymbol  = 'C'
  , iname    = "Void Cleaner Robot"
  , ifreq    = [("robot", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(10 * 10/12, 0), (11 * 10/12, 1000), (12 * 10/12, 0)]
                 -- unique, appears at 11 of 12
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 120, AddSkill SkMaxCalm 40
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkAlter 3
                   -- a miniboss; can remove rubble and ice,
                   -- but can't exit the gated level
               , AddSkill SkApply 1  -- apply the hooter
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A waste disposal robot repaired with parts from a heavy construction robot, including a scaled up goal matrix. The cosmic void is now the only acceptable model of cleanliness."
  , ikit     = [ ("waste container", COrgan), ("boiling vent", COrgan)
               , ("armored skin", COrgan), ("live wire", COrgan)
               , ("jaw", COrgan), ("hooked claw", COrgan), ("spotlight", COrgan)
               , ("eye 2", COrgan), ("nostril", COrgan), ("ear 5", COrgan)
               , ("robot brain", COrgan)
               , ("currency", CInv), ("currency", CInv), ("currency", CInv)
               , ("construction hooter", CEqp) ]
  }
