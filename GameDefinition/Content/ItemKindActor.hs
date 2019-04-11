-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( actors
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

actors :: [ItemKind]
actors =
  [warrior, warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush]
  -- Allure-specific
  ++ [giantOctopus, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, weldedRobot, cleanerRobot]

warrior,    warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, soldier, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush :: ItemKind
-- Allure-specific
giantOctopus, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, weldedRobot, cleanerRobot :: ItemKind

-- Note that the actors that appear in the crawl scenario should
-- be generated with at most ordinary ammo. Otherwise, farming them
-- may be rational though boring endeavour. Any exceptions to that
-- should be well thought of. E.g., unique guaranteed items on bosses
-- are safe, just as restricted kinds of weak items.

-- * Hunams

humanOrgans :: [(GroupName ItemKind, CStore)]
humanOrgans = [ ("fist", COrgan), ("foot", COrgan)
              , ("eye 6", COrgan), ("ear 3", COrgan)
              , ("sapient brain", COrgan) ]
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
  , iaspects = [ AddSkill SkMaxHP 90  -- partially from clothes and first aid
               , AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20
               , AddSkill SkNocto 2
               , AddSkill SkWait 1  -- can lurk
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 2  -- can even apply periodic items
               , AddSkill SkOdor 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = ""
  , ikit     = humanOrgans ++ [("genetic flaw 10", COrgan)]
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
  , ikit     = humanOrgans  -- no flaw
               ++ [ ("add sight", CEqp)
                  , ("armor ranged", CEqp)
                  , ("add nocto 1", CInv) ]
  -- , idesc    = ""
  }
ranger = warrior
  { iname    = "ranger"
  , ifreq    = [("ranger hero", 100), ("mobile", 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ ("armor ranged", CEqp)
                  , ("weak arrow", CInv) ]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [("escapist hero", 100), ("mobile", 1)]
  , ikit     = humanOrgans  -- no flaw
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
  , ikit     = humanOrgans  -- dark and numerous, so more kit without exploring
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
  , ikit     = humanOrgans  -- no flaw
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
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 10), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 16, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Walks with a stately dignity. You read death in the slow beckoning gestures of its revolting upper appendages."
  , ikit     = [ ("foot", COrgan), ("tentacle", COrgan)
               , ("bark", COrgan), ("eye 6", COrgan)
               , ("sapient brain", COrgan) ]  -- no voice, no hearing
  }
fastEye = ItemKind
  { isymbol  = 'b'
  , iname    = "rolling biter"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 3), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 5, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It bites as blindingly fast as it runs. Or rolls? Or crawls? Also, cuts and pierces."
  , ikit     = [ ("jaw", COrgan), ("razor", COrgan), ("horn", COrgan)
               , ("speed gland 10", COrgan)
               , ("eye 3", COrgan), ("ear 3", COrgan)
               , ("sapient brain", COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'h'
  , iname    = "clawing horror"
  , ifreq    = [ ("monster", 100), ("mobile", 1), ("mobile monster", 100)
               , ("aquatic", 30), ("aquatic monster", 30) ]  -- likes liquids
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 5), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject (-1)  -- can't project
               , AddSkill SkSwimming 30
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A blind, slimy mass of loose tissue. You'd think it's powerless, but as soon as it touches your trembling body, clawing, stinging and burning erupts and can't be fended off."
  , ikit     = [ ("small claw", COrgan), ("sting", COrgan)
               , ("venom tooth", COrgan)
               , ("nostril", COrgan)
               , ("sapient brain", COrgan) ]  -- no sight nor hearing
  }
elbow = ItemKind
  { isymbol  = 's'
  , iname    = "creepy shooter"
  , ifreq    = [ ("monster", 100), ("mobile", 1)
               , ("mobile monster", 100), ("scout monster", 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 1), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It moves in sudden jerks and never makes a noise. Speaks in hard objects hurled at deadly speeds."
  , ikit     = [ ("speed gland 5", COrgan)
               , ("eye 6", COrgan), ("ear 8", COrgan)
                   -- too powerful to get stronger sight
               , ("sapient brain", COrgan)
               , ("any arrow", CSha), ("any arrow", CInv)
               , ("weak arrow", CInv), ("weak arrow", CInv) ]
  }
torsor = ItemKind
  { isymbol  = 'M'
  , iname    = "Maker"
  , ifreq    = [("monster", 100), ("mobile", 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(14 * 10/15, 0), (15 * 10/15, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique, ELabel "of Contact"
               , AddSkill SkMaxHP 300, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 10, AddSkill SkNocto 2
               , AddSkill SkAggression 3
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can even use cultural artifacts
               , AddSkill SkAlter (-1)  -- can't exit the gated level; a boss,
                                        -- but can dig rubble, ice
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The mind, the big heart behind it all. Warmth and sympathy pour out through the graceful undulation of tentacles, sharp claws, snapping jaw and dripping fangs."
  , ikit     = [ ("tentacle", COrgan), ("hooked claw", COrgan)
                   -- at least one non-timed
               , ("large jaw", COrgan), ("venom fang", COrgan)
               , ("speed gland 5", COrgan)
               , ("eye 6", COrgan), ("ear 8", COrgan)
               , ("sapient brain", COrgan)
               , ("gem", CInv), ("gem", CInv), ("gem", CInv), ("gem", CInv) ]
  }

-- * Animals

-- They need rather strong melee, because they don't use items.
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
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 24, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ ("small jaw", COrgan)
               , ("eye 6", COrgan), ("nostril", COrgan), ("ear 8", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
  }
griffonVulture = ItemKind
  { isymbol  = 'v'
  , iname    = "griffon vulture"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 30) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 3), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 13000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 80
                   -- enough Calm to summon twice only if not attacked at all;
                   -- loses a lot of sight after summoning
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
      -- Animals don't have leader, usually, so even if only one on level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo. Common random double moves,
      -- on either side, are just too bothersome.
  , ieffects = []
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ ("screeching beak", COrgan)  -- in reality it grunts and hisses
               , ("small claw", COrgan)
               , ("eye 8", COrgan), ("ear 8", COrgan)
                   -- can't shoot, so strong sight is OK
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 8), (5, 1)]
  , iverbHit = "thud"
  , iweight  = 4000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 13, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , AddSkill SkOdor 5  -- and no smell skill, to let it leave smell
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ ("scent gland", COrgan)
               , ("small claw", COrgan), ("snout", COrgan)
               , ("eye 3", COrgan), ("ear 6", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
  }
armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 13, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ ("hooked claw", COrgan), ("snout", COrgan)
               , ("armored skin", COrgan), ("armored skin", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
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
  , iaspects = [ AddSkill SkMaxHP 15, AddSkill SkMaxCalm 50
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ ("venom tooth", COrgan), ("small claw", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [("animal", 100), ("mobile", 1), ("mobile animal", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(5, 1), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 28, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ ("venom fang", COrgan)  -- when on cooldown, it's weaponless
               , ("rattle", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("scavenger", 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 5)]  -- gets summoned often, so low base rarity
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 23, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 32, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ ("jaw", COrgan)
               , ("eye 6", COrgan), ("nostril", COrgan), ("ear 8", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
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
  , iaspects = [ AddSkill SkMaxHP 40, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 17, AddSkill SkNocto 2
               , AddSkill SkAggression 1  -- match the description
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Larger and more aggressive than any other lizard, but as easily recovering from wounds as its lesser cousins."
  , ikit     = [ ("large tail", COrgan), ("jaw", COrgan)
               , ("hooked claw", COrgan)
               , ("speed gland 5", COrgan), ("armored skin", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 3", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]  -- not to wake it up too soon
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
  , iaspects = [ AddSkill SkMaxHP 55, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkSwimming 100  -- swims better than walks
               , AddSkill SkWait 1  -- can sleep despite the genetic flaw
               , AddSkill SkApply 1  -- can eat food despite the genetic flaw
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An armored predator from the dawn of time. You better not get within its reach."
  , ikit     = [ ("huge tail", COrgan), ("large jaw", COrgan)
               , ("small claw", COrgan)
               , ("armored skin", COrgan)
               , ("eye 6", COrgan), ("ear 8", COrgan)
               , ("animal brain", COrgan)
               , ("genetic flaw 10", COrgan) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "Maddened Rhinoceros"
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(7 * 10/15, 0), (8 * 10/15, 1000), (9 * 10/15, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 120, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 27, AddSkill SkNocto 2
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-1)  -- can't use normal stairs nor dig;
                                        -- a weak miniboss
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "The last of its kind. Blind with rage, or perhaps due to the postoperative scars. A huge mass of muscle that charges at deadly speed."
  , ikit     = [ ("rhino horn", COrgan), ("snout", COrgan)
               , ("armored skin", COrgan)
               , ("eye 3", COrgan), ("ear 8", COrgan)
               , ("animal brain", COrgan) ]
  }

-- * Non-animal animals

beeSwarm = ItemKind
  { isymbol  = 'b'
  , iname    = "bee swarm"
  , ifreq    = [("animal", 100), ("mobile", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 3), (10, 4)]
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 8, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2  -- armor in sting
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ ("bee sting", COrgan)  -- weaponless when it's used up
               , ("vision 6", COrgan), ("ear 6", COrgan)
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
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ ("sting", COrgan)  -- when on cooldown, it's weaponless
               , ("vision 6", COrgan), ("ear 6", COrgan)
               , ("insect mortality", COrgan), ("animal brain", COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [("animal", 30), ("immobile animal", 50)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 13)]
  , iverbHit = "scrape"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Each branch bears long, curved thorns."
  , ikit     = [ ("thorn", COrgan)  -- after all run out, it's weaponless
               , ("bark", COrgan) ]
  }

-- * Allure-specific animals

giantOctopus = ItemKind
  { isymbol  = 'o'
  , iname    = "giant octopus"
  , ifreq    = [ ("animal", 100), ("mobile", 1), ("mobile animal", 100)
               , ("aquatic", 90), ("aquatic animal", 90) ]  -- weak on land
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "thud"
  , iweight  = 72000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 17, AddSkill SkMaxCalm 80
               , AddSkill SkSwimming 100  -- swims better than walks
               , AddSkill SkSpeed 27, AddSkill SkNocto 3 -- good night vision
               , AddSkill SkAlter (-2)  -- can't use normal stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It has eight arms of rage."
  , ikit     = [ ("tentacle", COrgan), ("tentacle", COrgan)
               , ("small beak", COrgan)  -- TODO: use when tentacles torn out
               , ("eye 8", COrgan)
                   -- shots not too damaging, so can have strong sight
               , ("animal brain", COrgan)
               , ("genetic flaw 3", COrgan) ]
  }

-- * Robots, Allure-specific

-- Robots have any colors but only f, d and r letters. Avoid these letters
-- for other factions.

razorwireFence = ItemKind
  { isymbol  = 'f'
  , iname    = "razorwire fence"
  , ifreq    = [("robot", 15), ("immobile robot", 10)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 20), (10, 4)]
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
  , ikit     = [("razor", COrgan), ("razor", COrgan)]
  }
electricFence = ItemKind
  { isymbol  = 'f'
  , iname    = "electric fence"
  , ifreq    = [("robot", 15), ("immobile robot", 10)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 15), (10, 5)]
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
  , ifreq    = [("robot", 27), ("immobile robot", 20)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(5 * 10/15, 0), (10, 7)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkWait 1
               , AddSkill SkProject 3  -- no brain, but can lob
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Makeshift, mostly non-lethal, autonomous perimeter defense outpost."
  , ikit     = [ ("vision 6", COrgan)
               , ("needle", CInv), ("can of sticky foam", CInv) ]
                   -- can of sticky foam is exploitable, but it spawns
                   -- reasonably often only on one level and not for
                   -- a long period
  }
steamFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "steam faucet"
  , ifreq    = [("robot", 8), ("immobile robot", 15)]
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
  , ifreq    = [("robot", 8), ("immobile robot", 30)]
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
  , ifreq    = [("robot", 8), ("immobile robot", 100)]
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
  , irarity  = [(1, 3)]
  , iverbHit = "clank"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 6, AddSkill SkMaxCalm 90
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkDisplace (-1)  -- as dumb as an animal
               , AddSkill SkMoveItem (-1)
               , AddSkill SkProject (-1)
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A video camera in each room would violate privacy of passengers, hence surveillance drones. Programmed to be easy to fend off, they keep a respectful distance."
  , ikit     = [ ("vision 16", COrgan), ("robot brain", COrgan) ]
  }
shepherdDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "oversight drone"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 3), (10, 4)]  -- gets summoned often, so low base rarity
  , iverbHit = "clank"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkMaxHP 3, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAlter (-1)  -- can't open doors; roams open spaces
               , AddSkill SkDisplace (-1)  -- as dumb as an animal
               , AddSkill SkMoveItem (-1)
               , AddSkill SkProject (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An tiny airborne robot designed to take construction measurements, synchronize robot workers and report irregularities, if any."
  , ikit     = [ ("live wire", COrgan), ("vision 16", COrgan), ("ear 8", COrgan)
               , ("robot brain", COrgan) ]
  }
huntingDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "hunting drone"
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(4, 0), (5, 5), (10, 8)]
  , iverbHit = "clank"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 6, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 40, AddSkill SkNocto 2
               , AddSkill SkDisplace (-1)  -- almost as dumb as an animal
               , AddSkill SkMoveItem (-1)  -- but can project
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Originally designed for hunting down and putting to sleep stray animals. The sleeping agent must have long since dried up."
  , ikit     = [ ("eye 8", COrgan), ("nostril", COrgan), ("ear 8", COrgan)
                   -- week projectiles, so strong sight OK
               , ("robot brain", COrgan)
               , ("needle", CInv), ("tranquillizer dart", CInv) ]
  }
homeRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "feral home robot"
               -- TODO: name another 'deranged', tertiary imperative: survival
  , ifreq    = [("robot", 100), ("mobile", 100), ("mobile robot", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(1, 15), (10, 5)]
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject (-1)  -- can't project
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Once a timid household robot, now sufficiently adapted to survive in the deadly environment."
  , ikit     = [ ("fist", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 3", COrgan)
               , ("robot brain", COrgan) ]
  }
wasteRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "waste disposal robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(5, 9)]  -- gets summoned often, so low base rarity
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 17, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 15, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "You are not in its database, hence you are waste."
  , ikit     = [ ("tentacle", COrgan)
               , ("nostril", COrgan)  -- only smell, for variety
               , ("robot brain", COrgan)
               , ("waste container", CEqp) ]
  }
lightRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "decoration robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 100) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 6), (10, 6)]
                 -- gets summoned often, so low rarity
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 17, AddSkill SkMaxCalm 40
                   -- can't summon again for a long time;
                   -- loses a lot of sight after summoning
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkAlter 3  -- uses all stairs
               , AddSkill SkApply 1  -- can apply the hooter
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Interior and exterior decoration robot. Strongly fancies deep reds recently."
  , ikit     = [ ("hooked claw", COrgan), ("tentacle", COrgan)
               , ("hull plating", COrgan)
               , ("eye 6", COrgan), ("ear 8", COrgan)
               , ("robot brain", COrgan)
               , ("construction hooter", CEqp) ]
  }
heavyRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "demolition robot"
  , ifreq    = [ ("robot", 100), ("mobile", 100), ("mobile robot", 100)
               , ("construction robot", 70) ]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(7 * 10/15, 0), (8 * 10/15, 4), (10, 13)]
  , iverbHit = "clank"
  , iweight  = 800000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 41, AddSkill SkMaxCalm 40
                   -- can't summon again for a long time;
                   -- loses a lot of sight after summoning
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkAlter 3  -- uses all stairs
               , AddSkill SkApply 1  -- can apply the hooter
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Heavy multi-purpose construction robot. Excels at discharging, dismantling and demolition."
  , ikit     = [ ("large jaw", COrgan), ("small claw", COrgan)
               , ("hull plating", COrgan)
               , ("eye 3", COrgan), ("ear 6", COrgan)
               , ("robot brain", COrgan)
               , ("spotlight", CEqp), ("construction hooter", CEqp) ]
  }
weldedRobot = ItemKind
  { isymbol  = 'W'
  , iname    = "Smiling Welded Robot"
  , ifreq    = [("robot", 100), ("immobile robot", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1 * 10/15, 1000), (2 * 10/15, 0)]  -- unique
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 200, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A well-built humanoid luggage unloading robot with a smooth satin silvery skin. Its graceful moves are stunted by a thick irregular weld fastening both its shapely legs to the floor. A whiff of smoke escapes whenever it opens its mouth in a charming toothy smile while brandishing a blowtorch in its trembling hand."
  , ikit     = [ ("fist", COrgan)
               , ("eye 6", COrgan), ("ear 3", COrgan)
               , ("mouth vent", COrgan)
               , ("robot brain", COrgan)
               , ("crude weld", COrgan), ("blowtorch", CEqp) ]
  }
cleanerRobot = ItemKind
  { isymbol  = 'C'
  , iname    = "Void Cleaner Robot"
  , ifreq    = [("robot", 100), ("mobile", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(11 * 10/15, 0), (12 * 10/15, 1000), (13 * 10/15, 0)]
                 -- unique, appears at 11 of 12
  , iverbHit = "clank"
  , iweight  = 800000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 120, AddSkill SkMaxCalm 40
                   -- can't summon again for a long time;
                   -- loses a lot of sight after summoning
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAggression 1
                   -- can't use normal stairs nor dig; a weak miniboss
               , AddSkill SkApply 1  -- can apply the hooter
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An oversize waste disposal robot repaired with parts from a demolition robot, including a scaled up goal matrix. The cosmic void is now the only acceptable model of cleanliness. The robot's bulky trunk doesn't fit into even the larger lift carriages."
  , ikit     = [ ("live wire", COrgan), ("large jaw", COrgan)
               , ("tentacle", COrgan)
               , ("boiling vent", COrgan), ("hull plating", COrgan)
               , ("eye 3", COrgan), ("nostril", COrgan), ("ear 6", COrgan)
               , ("robot brain", COrgan)
               , ("currency", CInv), ("currency", CInv), ("currency", CInv)
               , ("waste container", CEqp), ("spotlight", CEqp)
               , ("construction hooter", CEqp) ]
  }
