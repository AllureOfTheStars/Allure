-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( -- * Group name patterns
    pattern HERO, pattern SCOUT_HERO, pattern RANGER_HERO, pattern ESCAPIST_HERO, pattern AMBUSHER_HERO, pattern BRAWLER_HERO, pattern SOLDIER_HERO, pattern CIVILIAN, pattern MONSTER, pattern MOBILE_MONSTER, pattern SCOUT_MONSTER, pattern ANIMAL, pattern MOBILE_ANIMAL, pattern IMMOBILE_ANIMAL
  , pattern ADD_SIGHT, pattern ARMOR_RANGED, pattern ADD_NOCTO_1, pattern WEAK_ARROW, pattern LIGHT_MANIPULATION, pattern WOODEN_TORCH, pattern BLANKET, pattern RING_OF_OPPORTUNITY_SNIPER, pattern ANY_ARROW, pattern STARTING_WEAPON, pattern GEM
  , pattern CRAWL_HERO, pattern MERCENARY_HERO, pattern AQUATIC_ANIMAL, pattern AQUATIC_MONSTER, pattern ROBOT, pattern MOBILE_ROBOT, pattern IMMOBILE_ROBOT, pattern CONSTRUCTION_ROBOT
  , pattern COOKED_FOOD, pattern MERCENARY_WEAPON, pattern BULLTEPROOF_VEST, pattern MERCENARY_AMMO, pattern RAW_MEAT_CHUNK, pattern ROASTED_MEAT_CHUNK, pattern NEEDLE, pattern CAN_OF_STICKY_FOAM, pattern TRANQUILIZER_DART, pattern WASTE_CONTAINER, pattern CONSTRUCTION_HOOTER, pattern SPOTLIGHT, pattern BLOWTORCH
  , -- * Content
    actors
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindOrgan
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

pattern HERO, SCOUT_HERO, RANGER_HERO, ESCAPIST_HERO, AMBUSHER_HERO, BRAWLER_HERO, SOLDIER_HERO, CIVILIAN, MONSTER, MOBILE_MONSTER, SCOUT_MONSTER, ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL :: GroupName ItemKind

pattern ADD_SIGHT, ARMOR_RANGED, ADD_NOCTO_1, WEAK_ARROW, LIGHT_MANIPULATION, WOODEN_TORCH, BLANKET, RING_OF_OPPORTUNITY_SNIPER, ANY_ARROW, STARTING_WEAPON, GEM :: GroupName ItemKind

pattern CRAWL_HERO, MERCENARY_HERO, AQUATIC_ANIMAL, AQUATIC_MONSTER, ROBOT, MOBILE_ROBOT, IMMOBILE_ROBOT, CONSTRUCTION_ROBOT :: GroupName ItemKind

pattern COOKED_FOOD, MERCENARY_WEAPON, BULLTEPROOF_VEST, MERCENARY_AMMO, RAW_MEAT_CHUNK, ROASTED_MEAT_CHUNK, NEEDLE, CAN_OF_STICKY_FOAM, TRANQUILIZER_DART, WASTE_CONTAINER, CONSTRUCTION_HOOTER, SPOTLIGHT, BLOWTORCH :: GroupName ItemKind

-- ** Common
pattern HERO = GroupName "hero"
pattern SCOUT_HERO = GroupName "scout hero"
pattern RANGER_HERO = GroupName "ranger hero"
pattern ESCAPIST_HERO = GroupName "escapist hero"
pattern AMBUSHER_HERO = GroupName "ambusher hero"
pattern BRAWLER_HERO = GroupName "brawler hero"
pattern SOLDIER_HERO = GroupName "soldier hero"
pattern CIVILIAN = GroupName "civilian"
pattern MONSTER = GroupName "monster"
pattern MOBILE_MONSTER = GroupName "mobile monster"
pattern SCOUT_MONSTER = GroupName "scout monster"
pattern ANIMAL = GroupName "animal"
pattern MOBILE_ANIMAL = GroupName "mobile animal"
pattern IMMOBILE_ANIMAL = GroupName "immobile animal"

-- ** Allure-specific
pattern CRAWL_HERO = GroupName "crawl hero"
pattern MERCENARY_HERO = GroupName "mercenary hero"
pattern AQUATIC_ANIMAL = GroupName "aquatic animal"
pattern AQUATIC_MONSTER = GroupName "aquatic monster"
pattern ROBOT = GroupName "robot"
pattern MOBILE_ROBOT = GroupName "mobile robot"
pattern IMMOBILE_ROBOT = GroupName "immobile robot"
pattern CONSTRUCTION_ROBOT = GroupName "construction robot"

-- ** Common
pattern ADD_SIGHT = GroupName "add sight"
pattern ARMOR_RANGED = GroupName "armor ranged"
pattern ADD_NOCTO_1 = GroupName "add nocto 1"
pattern WEAK_ARROW = GroupName "weak arrow"
pattern LIGHT_MANIPULATION = GroupName "light manipulation"
pattern WOODEN_TORCH = GroupName "wooden torch"
pattern BLANKET = GroupName "blanket"
pattern RING_OF_OPPORTUNITY_SNIPER = GroupName "ring of opportunity sniper"
pattern ANY_ARROW = GroupName "any arrow"
pattern STARTING_WEAPON = GroupName "starting weapon"
pattern GEM = GroupName "gem"

-- ** Allure-specific
pattern COOKED_FOOD = GroupName "cooked food"
pattern MERCENARY_WEAPON = GroupName "mercenary weapon"
pattern BULLTEPROOF_VEST = GroupName "bulletproof vest"
pattern MERCENARY_AMMO = GroupName "mercenary ammo"
pattern RAW_MEAT_CHUNK = GroupName "raw meat chunk"
pattern ROASTED_MEAT_CHUNK = GroupName "roasted meat chunk"
pattern NEEDLE = GroupName "needle"
pattern CAN_OF_STICKY_FOAM = GroupName "can of sticky foam"
pattern TRANQUILIZER_DART = GroupName "tranquillizer dart"
pattern WASTE_CONTAINER = GroupName "waste container"
pattern CONSTRUCTION_HOOTER = GroupName "construction hooter"
pattern SPOTLIGHT = GroupName "spotlight"
pattern BLOWTORCH = GroupName "blowtorch"

-- * Content

actors :: [ItemKind]
actors =
  [warrior, warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, brawler, soldier, mercenary, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush]
  -- Allure-specific
  ++ [giantOctopus, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, weldedRobot, cleanerRobot]

warrior,    warrior2, warrior3, warrior4, warrior5, scout, ranger, escapist, ambusher, brawler, soldier, mercenary, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, rhinoceros, beeSwarm, hornetSwarm, thornbush :: ItemKind
-- Allure-specific
giantOctopus, razorwireFence, electricFence, activeFence, steamFaucet, biogasFaucet, medbotFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, lightRobot, heavyRobot, weldedRobot, cleanerRobot :: ItemKind

-- Note that the actors that appear in the crawl scenario should
-- be generated with at most ordinary ammo. Otherwise, farming them
-- may be rational though boring endeavour. Any exceptions to that
-- should be well thought of. E.g., unique guaranteed items on bosses
-- are safe, just as restricted kinds of weak items.

-- * Hunams

humanOrgans :: [(GroupName ItemKind, CStore)]
humanOrgans = [ (FIST, COrgan), (FOOT, COrgan)
              , (EYE_6, COrgan), (EAR_3, COrgan)
              , (SAPIENT_BRAIN, COrgan)
              , (ANIMAL_STOMACH, COrgan), (HUNGRY, COrgan) ]
warrior = ItemKind
  { isymbol  = '@'
  , iname    = "mercenary"  -- modified if initial actors in hero faction
  , ifreq    = [(HERO, 100), (CRAWL_HERO, 100), (MOBILE, 1)]
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
  , ikit     = humanOrgans ++ [(GENETIC_FLAW_10, COrgan)]
  }
warrior2 = warrior
  { iname    = "pilot"
  -- , idesc    = ""
  }
warrior3 = warrior
  { iname    = "engineer"
  , ifreq    = [(CRAWL_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior ++ [(S_CURRENCY, CStash), (COOKED_FOOD, CStash)]
  -- , idesc    = ""
  }
warrior4 = warrior
  { iname    = "doctor"
  , ifreq    = [(CRAWL_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior3
  -- , idesc    = ""
  }
warrior5 = warrior
  { iname    = "hacker"
  , ifreq    = [(CRAWL_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit warrior3
  -- , idesc    = ""
  }

scout = warrior
  { iname    = "scout"
  , ifreq    = [(SCOUT_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (ADD_SIGHT, CEqp)
                  , (ARMOR_RANGED, CEqp)
                  , (ADD_NOCTO_1, CStash) ]
  -- , idesc    = ""
  }
ranger = warrior
  { iname    = "ranger"
  , ifreq    = [(RANGER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash) ]
  -- , idesc    = ""
  }
escapist = warrior
  { iname    = "escapist"
  , ifreq    = [(ESCAPIST_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (ADD_SIGHT, CEqp)
                  , (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash)  -- mostly for probing
                  , (LIGHT_MANIPULATION, CStash)
                  , (WOODEN_TORCH, CStash)
                  , (BLANKET, CStash) ]
  -- , idesc    = ""
  }
ambusher = warrior
  { iname    = "ambusher"
  , ifreq    = [(AMBUSHER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- dark and numerous, so more kit without exploring
               ++ [ (RING_OF_OPPORTUNITY_SNIPER, CEqp)
                  , (ANY_ARROW, CStash)
                  , (WEAK_ARROW, CStash)
                  , (EXPLOSIVE, CStash)
                  , (LIGHT_MANIPULATION, CEqp)
                  , (WOODEN_TORCH, CStash) ]
  -- , idesc    = ""
  }
brawler = warrior
  { iname    = "brawler"
  , ifreq    = [(BRAWLER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [(STARTING_WEAPON, CEqp)]
  -- , idesc    = ""
  }
soldier = brawler
  { iname    = "soldier"
  , ifreq    = [(SOLDIER_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit brawler
               ++ [(EXPLOSIVE, CStash)]
  -- , idesc    = ""
  }
mercenary = brawler
  { iname    = "mercenary"
  , ifreq    = [(MERCENARY_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (MERCENARY_WEAPON, CEqp)
                  , (BULLTEPROOF_VEST, CEqp)
                  , (MERCENARY_AMMO, CStash)
                  , (EXPLOSIVE, CStash) ]
  -- , idesc    = ""
  }

civilian = warrior
  { iname    = "clerk"
  , ifreq    = [(CIVILIAN, 100), (MOBILE, 1)]
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
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 10) ]
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
  , ikit     = [ (FOOT, COrgan), (TENTACLE, COrgan)
               , (BARK, COrgan), (EYE_6, COrgan)
               , (SAPIENT_BRAIN, COrgan) ]  -- no voice, no hearing
  }
fastEye = ItemKind
  { isymbol  = 'b'
  , iname    = "rolling biter"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 60) ]
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
  , ikit     = [ (JAW, COrgan), (RAZOR, COrgan), (HORN, COrgan)
               , (SPEED_GLAND_10, COrgan)
               , (EYE_3, COrgan), (EAR_3, COrgan)
               , (SAPIENT_BRAIN, COrgan) ]
  }
nose = ItemKind  -- depends solely on smell
  { isymbol  = 'h'
  , iname    = "clawing horror"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1), (MOBILE_MONSTER, 100)
               , (AQUATIC, 30), (AQUATIC_MONSTER, 30) ]  -- likes liquids
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
  , ikit     = [ (SMALL_CLAW, COrgan), (STING, COrgan)
               , (VENOM_TOOTH, COrgan)
               , (NOSTRIL, COrgan)
               , (SAPIENT_BRAIN, COrgan) ]  -- no sight nor hearing
  }
elbow = ItemKind
  { isymbol  = 's'
  , iname    = "creepy shooter"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 30) ]
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
  , ikit     = [ (SPEED_GLAND_5, COrgan)
               , (EYE_6, COrgan), (EAR_8, COrgan)
                   -- too powerful to get stronger sight
               , (SAPIENT_BRAIN, COrgan)
               , (ANY_ARROW, CStash), (ANY_ARROW, CStash)
               , (WEAK_ARROW, CStash), (WEAK_ARROW, CStash) ]
  }
torsor = ItemKind
  { isymbol  = 'M'
  , iname    = "Maker"
  , ifreq    = [(MONSTER, 100), (MOBILE, 1)]
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
  , ikit     = [ (TENTACLE, COrgan), (HOOKED_CLAW, COrgan)
                   -- at least one non-timed
               , (LARGE_JAW, COrgan), (VENOM_FANG, COrgan)
               , (SPEED_GLAND_5, COrgan)
               , (EYE_6, COrgan), (EAR_8, COrgan)
               , (SAPIENT_BRAIN, COrgan)
               , (GEM, CStash), (GEM, CStash)
               , (GEM, CStash), (GEM, CStash) ]
  }

-- * Animals

-- They need rather strong melee, because they don't use items.
-- They have dull colors, except for yellow, because there is no dull variant.

goldenJackal = ItemKind  -- basically a much smaller and slower hyena
  { isymbol  = 'j'
  , iname    = "golden jackal"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 50) ]
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
  , ikit     = [ (SMALL_JAW, COrgan)
               , (EYE_6, COrgan), (NOSTRIL, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan) ]
  }
griffonVulture = ItemKind
  { isymbol  = 'v'
  , iname    = "griffon vulture"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 30) ]
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
  , ikit     = [ (SCREECHING_BEAK, COrgan)  -- in reality it grunts and hisses
               , (SMALL_CLAW, COrgan)
               , (EYE_8, COrgan), (EAR_8, COrgan)
                   -- can't shoot, so strong sight is OK
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan) ]
  }
skunk = ItemKind
  { isymbol  = 's'
  , iname    = "hog-nosed skunk"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
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
  , ikit     = [ (SCENT_GLAND, COrgan)
               , (SMALL_CLAW, COrgan), (SNOUT, COrgan)
               , (EYE_3, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan) ]
  }
armadillo = ItemKind
  { isymbol  = 'a'
  , iname    = "giant armadillo"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
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
  , ikit     = [ (HOOKED_CLAW, COrgan), (SNOUT, COrgan)
               , (ARMORED_SKIN, COrgan), (ARMORED_SKIN, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan)
               , (RAW_MEAT_CHUNK, CEqp) ]
  }
gilaMonster = ItemKind
  { isymbol  = 'g'
  , iname    = "Gila monster"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
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
  , ikit     = [ (VENOM_TOOTH, COrgan), (SMALL_CLAW, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan)  -- small reptile, hungers slowly
               , (GENETIC_FLAW_3, COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = 's'
  , iname    = "rattlesnake"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
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
  , ikit     = [ (VENOM_FANG, COrgan)  -- when on cooldown, it's weaponless
               , (RATLLE, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_6, COrgan)
               , (ANIMAL_BRAIN, COrgan)  -- small reptile, hungers slowly
               , (GENETIC_FLAW_3, COrgan)
               , (RAW_MEAT_CHUNK, CEqp) ]
  }
hyena = ItemKind
  { isymbol  = 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 20) ]
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
  , ikit     = [ (JAW, COrgan)
               , (EYE_6, COrgan), (NOSTRIL, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan) ]
  }
komodoDragon = ItemKind
  { isymbol  = 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
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
  , ikit     = [ (LARGE_TAIL, COrgan), (JAW, COrgan)
               , (HOOKED_CLAW, COrgan)
               , (SPEED_GLAND_5, COrgan), (ARMORED_SKIN, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_3, COrgan)
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan)  -- not to wake it up too soon
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
  }
alligator = ItemKind
  { isymbol  = 'a'
  , iname    = "alligator"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (AQUATIC, 70), (AQUATIC_ANIMAL, 70) ]  -- amphibious
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
  , ikit     = [ (HUGE_TAIL, COrgan), (LARGE_JAW, COrgan)
               , (SMALL_CLAW, COrgan)
               , (ARMORED_SKIN, COrgan)
               , (EYE_6, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_10, COrgan)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
  }
rhinoceros = ItemKind
  { isymbol  = 'R'
  , iname    = "Maddened Rhinoceros"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1)]
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
  , ikit     = [ (RHINO_HORN, COrgan), (SNOUT, COrgan)
               , (ARMORED_SKIN, COrgan)
               , (EYE_3, COrgan), (EAR_8, COrgan)
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
  }

-- * Non-animal animals

beeSwarm = ItemKind
  { isymbol  = 'b'
  , iname    = "bee swarm"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1)]
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
  , ikit     = [ (BEE_STING, COrgan)  -- weaponless when it's used up
               , (VISION_6, COrgan), (EAR_6, COrgan)
               , (INSECT_MORTALITY, COrgan), (ANIMAL_BRAIN, COrgan) ]
  }
hornetSwarm = ItemKind
  { isymbol  = 'h'
  , iname    = "hornet swarm"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
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
  , ikit     = [ (STING, COrgan)  -- when on cooldown, it's weaponless
               , (VISION_6, COrgan), (EAR_6, COrgan)
               , (INSECT_MORTALITY, COrgan), (ANIMAL_BRAIN, COrgan) ]
  }
thornbush = ItemKind
  { isymbol  = 't'
  , iname    = "thornbush"
  , ifreq    = [(ANIMAL, 30), (IMMOBILE_ANIMAL, 50)]
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
  , ikit     = [ (THORN, COrgan)  -- after all run out, it's weaponless
               , (BARK, COrgan) ]
  }

-- * Allure-specific animals

giantOctopus = ItemKind
  { isymbol  = 'o'
  , iname    = "giant octopus"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (AQUATIC, 90), (AQUATIC_ANIMAL, 90) ]  -- weak on land
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
  , ikit     = [ (TENTACLE, COrgan), (TENTACLE, COrgan)
               , (SMALL_BEAK, COrgan)  -- TODO: use when tentacles torn out
               , (EYE_8, COrgan)
                   -- shots not too damaging, so can have strong sight
               , (ANIMAL_BRAIN, COrgan), (ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
 }

-- * Robots, Allure-specific

-- Robots have any colors but only f, d and r letters. Avoid these letters
-- for other factions.

razorwireFence = ItemKind
  { isymbol  = 'f'
  , iname    = "razorwire fence"
  , ifreq    = [(ROBOT, 15), (IMMOBILE_ROBOT, 10)]
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
  , ikit     = [(RAZOR, COrgan), (RAZOR, COrgan)]
  }
electricFence = ItemKind
  { isymbol  = 'f'
  , iname    = "electric fence"
  , ifreq    = [(ROBOT, 15), (IMMOBILE_ROBOT, 10)]
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
  , ikit     = [(LIVE_WIRE, COrgan)]
  }
activeFence = ItemKind
  { isymbol  = 'f'
  , iname    = "active fence"
  , ifreq    = [(ROBOT, 27), (IMMOBILE_ROBOT, 20)]
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
  , ikit     = [ (VISION_6, COrgan)
               , (NEEDLE, CStash), (CAN_OF_STICKY_FOAM, CStash) ]
                   -- can of sticky foam is exploitable, but it spawns
                   -- reasonably often only on one level and not for
                   -- a long period
  }
steamFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "steam faucet"
  , ifreq    = [(ROBOT, 8), (IMMOBILE_ROBOT, 15)]
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
  , ikit     = [(BOILING_VENT, COrgan), (BOILING_FISSURE, COrgan)]
  }
biogasFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "biogas faucet"
  , ifreq    = [(ROBOT, 8), (IMMOBILE_ROBOT, 30)]
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
  , ikit     = [(BIOGAS_VENT, COrgan), (BIOGAS_FISSURE, COrgan)]
  }
medbotFaucet = ItemKind
  { isymbol  = 'f'
  , iname    = "nano medbot faucet"
  , ifreq    = [(ROBOT, 8), (IMMOBILE_ROBOT, 100)]
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
  , ikit     = [(MEDBOT_VENT, COrgan), (MEDBOT_FISSUE, COrgan)]
  }
surveillanceDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "surveillance drone"
  , ifreq    = [(ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)]
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
  , ikit     = [ (VISION_16, COrgan), (ROBOT_BRAIN, COrgan) ]
  }
shepherdDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "oversight drone"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 100) ]
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
  , ikit     = [ (LIVE_WIRE, COrgan), (VISION_16, COrgan), (EAR_8, COrgan)
               , (ROBOT_BRAIN, COrgan) ]
  }
huntingDrone = ItemKind
  { isymbol  = 'd'
  , iname    = "hunting drone"
  , ifreq    = [(ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)]
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
  , ikit     = [ (EYE_8, COrgan), (NOSTRIL, COrgan), (EAR_8, COrgan)
                   -- week projectiles, so strong sight OK
               , (ROBOT_BRAIN, COrgan)
               , (NEEDLE, CStash), (TRANQUILIZER_DART, CStash) ]
  }
homeRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "feral home robot"
               -- TODO: name another 'deranged', tertiary imperative: survival
  , ifreq    = [(ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)]
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
  , ikit     = [ (FIST, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_3, COrgan)
               , (ROBOT_BRAIN, COrgan) ]
  }
wasteRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "waste disposal robot"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 100) ]
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
  , ikit     = [ (TENTACLE, COrgan)
               , (NOSTRIL, COrgan)  -- only smell, for variety
               , (ROBOT_BRAIN, COrgan)
               , (WASTE_CONTAINER, CEqp) ]
  }
lightRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "decoration robot"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 100) ]
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
  , ikit     = [ (HOOKED_CLAW, COrgan), (TENTACLE, COrgan)
               , (HULL_PLATING, COrgan)
               , (EYE_6, COrgan), (EAR_8, COrgan)
               , (ROBOT_BRAIN, COrgan)
               , (CONSTRUCTION_HOOTER, CEqp) ]
  }
heavyRobot = ItemKind
  { isymbol  = 'r'
  , iname    = "demolition robot"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 70) ]
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
  , ikit     = [ (LARGE_JAW, COrgan), (SMALL_CLAW, COrgan)
               , (HULL_PLATING, COrgan)
               , (EYE_3, COrgan), (EAR_6, COrgan)
               , (ROBOT_BRAIN, COrgan)
               , (SPOTLIGHT, CEqp), (CONSTRUCTION_HOOTER, CEqp) ]
  }
weldedRobot = ItemKind
  { isymbol  = 'W'
  , iname    = "Smiling Welded Robot"
  , ifreq    = [(ROBOT, 100), (IMMOBILE_ROBOT, 100)]
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
  , ikit     = [ (FIST, COrgan)
               , (EYE_6, COrgan), (EAR_3, COrgan)
               , (MOUTH_VENT, COrgan)
               , (ROBOT_BRAIN, COrgan)
               , (CRUDE_WELD, COrgan), (BLOWTORCH, CEqp) ]
  }
cleanerRobot = ItemKind
  { isymbol  = 'C'
  , iname    = "Void Cleaner Robot"
  , ifreq    = [(ROBOT, 100), (MOBILE, 1)]
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
  , ikit     = [ (LIVE_WIRE, COrgan), (LARGE_JAW, COrgan)
               , (TENTACLE, COrgan)
               , (BOILING_VENT, COrgan), (HULL_PLATING, COrgan)
               , (EYE_3, COrgan), (NOSTRIL, COrgan), (EAR_6, COrgan)
               , (ROBOT_BRAIN, COrgan)
               , (S_CURRENCY, CStash)
               , (S_CURRENCY, CStash)
               , (S_CURRENCY, CStash)
               , (WASTE_CONTAINER, CEqp), (SPOTLIGHT, CEqp)
               , (CONSTRUCTION_HOOTER, CEqp) ]
  }
