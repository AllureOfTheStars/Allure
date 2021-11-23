-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor (or rather actor body trunk) definitions.
module Content.ItemKindActor
  ( -- * Group name patterns
    pattern S_WOODEN_TORCH, pattern S_SANDSTONE_ROCK
  , pattern HERO, pattern SCOUT_HERO, pattern RANGER_HERO, pattern ESCAPIST_HERO, pattern AMBUSHER_HERO, pattern BRAWLER_HERO, pattern SOLDIER_HERO, pattern CIVILIAN, pattern MONSTER, pattern MOBILE_MONSTER, pattern SCOUT_MONSTER, pattern ANIMAL, pattern MOBILE_ANIMAL, pattern IMMOBILE_ANIMAL, pattern INSECT
  , pattern ADD_SIGHT, pattern ARMOR_RANGED, pattern ADD_NOCTO_1, pattern WEAK_ARROW, pattern LIGHT_ATTENUATOR, pattern FIREPROOF_CLOTH, pattern RING_OF_OPPORTUNITY_SNIPER, pattern ANY_ARROW, pattern STARTING_ARMOR, pattern STARTING_WEAPON, pattern GEM
  , pattern CRAWL_HERO, pattern EXTERMINATOR_HERO, pattern RAIDER_HERO, pattern MERCENARY_HERO, pattern AQUATIC_ANIMAL, pattern AQUATIC_MONSTER, pattern EXPLOSIVE_MONSTER, pattern ROBOT, pattern MOBILE_ROBOT, pattern IMMOBILE_ROBOT, pattern CONSTRUCTION_ROBOT, pattern MECHANICAL_CONTRAPTION, pattern GAUNTLET_ROBOT
  , pattern S_BULLTEPROOF_VEST, pattern S_PERFUME_POTION, pattern S_EMPTY_FLASK
  , pattern COOKED_FOOD, pattern MERCENARY_WEAPON, pattern MERCENARY_AMMO, pattern RAW_MEAT_CHUNK, pattern ROASTED_MEAT_CHUNK, pattern NEEDLE, pattern CAN_OF_STICKY_FOAM, pattern TRANQUILIZER_DART, pattern WASTE_CONTAINER, pattern CONSTRUCTION_HOOTER, pattern SPOTLIGHT, pattern BLOWTORCH, pattern POLE, pattern POLE_OR_HANDLE, pattern BREACHING_TOOL, pattern BONDING_TOOL, pattern SHARPENING_TOOL, pattern WIRECUTTING_TOOL
  , actorsGN, actorsGNSingleton
  , -- * Content
    actors
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Definition.Flavour

import Content.ItemKindOrgan

-- * Group name patterns

actorsGNSingleton :: [GroupName ItemKind]
actorsGNSingleton =
       [S_WOODEN_TORCH, S_SANDSTONE_ROCK, S_BULLTEPROOF_VEST, S_PERFUME_POTION, S_EMPTY_FLASK]

pattern S_WOODEN_TORCH, S_SANDSTONE_ROCK, S_BULLTEPROOF_VEST, S_PERFUME_POTION, S_EMPTY_FLASK :: GroupName ItemKind

actorsGN :: [GroupName ItemKind]
actorsGN =
       [HERO, SCOUT_HERO, RANGER_HERO, ESCAPIST_HERO, AMBUSHER_HERO, BRAWLER_HERO, SOLDIER_HERO, CIVILIAN, MONSTER, MOBILE_MONSTER, SCOUT_MONSTER, ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL, INSECT]
    ++ [ADD_SIGHT, ARMOR_RANGED, ADD_NOCTO_1, WEAK_ARROW, LIGHT_ATTENUATOR, FIREPROOF_CLOTH, RING_OF_OPPORTUNITY_SNIPER, ANY_ARROW, STARTING_ARMOR, STARTING_WEAPON, GEM]
    ++ [CRAWL_HERO, EXTERMINATOR_HERO, RAIDER_HERO, MERCENARY_HERO, AQUATIC_ANIMAL, AQUATIC_MONSTER, EXPLOSIVE_MONSTER, ROBOT, MOBILE_ROBOT, IMMOBILE_ROBOT, CONSTRUCTION_ROBOT, MECHANICAL_CONTRAPTION, GAUNTLET_ROBOT]
    ++ [COOKED_FOOD, MERCENARY_WEAPON, MERCENARY_AMMO, RAW_MEAT_CHUNK, ROASTED_MEAT_CHUNK, NEEDLE, CAN_OF_STICKY_FOAM, TRANQUILIZER_DART, WASTE_CONTAINER, CONSTRUCTION_HOOTER, SPOTLIGHT, BLOWTORCH, POLE, POLE_OR_HANDLE, BREACHING_TOOL, BONDING_TOOL, SHARPENING_TOOL, WIRECUTTING_TOOL]

pattern HERO, SCOUT_HERO, RANGER_HERO, ESCAPIST_HERO, AMBUSHER_HERO, BRAWLER_HERO, SOLDIER_HERO, CIVILIAN, MONSTER, MOBILE_MONSTER, SCOUT_MONSTER, ANIMAL, MOBILE_ANIMAL, IMMOBILE_ANIMAL, INSECT :: GroupName ItemKind

pattern ADD_SIGHT, ARMOR_RANGED, ADD_NOCTO_1, WEAK_ARROW, LIGHT_ATTENUATOR, FIREPROOF_CLOTH, RING_OF_OPPORTUNITY_SNIPER, ANY_ARROW, STARTING_ARMOR, STARTING_WEAPON, GEM :: GroupName ItemKind

pattern CRAWL_HERO, EXTERMINATOR_HERO, RAIDER_HERO, MERCENARY_HERO, AQUATIC_ANIMAL, AQUATIC_MONSTER, EXPLOSIVE_MONSTER, ROBOT, MOBILE_ROBOT, IMMOBILE_ROBOT, CONSTRUCTION_ROBOT, MECHANICAL_CONTRAPTION, GAUNTLET_ROBOT :: GroupName ItemKind

pattern COOKED_FOOD, MERCENARY_WEAPON, MERCENARY_AMMO, RAW_MEAT_CHUNK, ROASTED_MEAT_CHUNK, NEEDLE, CAN_OF_STICKY_FOAM, TRANQUILIZER_DART, WASTE_CONTAINER, CONSTRUCTION_HOOTER, SPOTLIGHT, BLOWTORCH, POLE, POLE_OR_HANDLE, BREACHING_TOOL, BONDING_TOOL, SHARPENING_TOOL, WIRECUTTING_TOOL :: GroupName ItemKind

-- ** Common
pattern HERO = GroupName "adventurer"
pattern SCOUT_HERO = GroupName "scout"
pattern RANGER_HERO = GroupName "ranger"
pattern ESCAPIST_HERO = GroupName "escapist"
pattern AMBUSHER_HERO = GroupName "ambusher"
pattern BRAWLER_HERO = GroupName "brawler"
pattern SOLDIER_HERO = GroupName "fighter"
pattern CIVILIAN = GroupName "civilian"
pattern MONSTER = GroupName "monstrosity"
pattern MOBILE_MONSTER = GroupName "mobile monstrosity"
pattern SCOUT_MONSTER = GroupName "scout monstrosity"
pattern ANIMAL = GroupName "animal"
pattern MOBILE_ANIMAL = GroupName "mobile animal"
pattern IMMOBILE_ANIMAL = GroupName "immobile animal"
pattern INSECT = GroupName "insect"

-- ** Allure-specific
pattern CRAWL_HERO = GroupName "crawl professional"
pattern EXTERMINATOR_HERO = GroupName "exterminator"
pattern RAIDER_HERO = GroupName "raider"
pattern MERCENARY_HERO = GroupName "mercenary"
pattern AQUATIC_ANIMAL = GroupName "aquatic animal"
pattern AQUATIC_MONSTER = GroupName "aquatic monstrosity"
pattern EXPLOSIVE_MONSTER = GroupName "explosive monstrosity"
pattern ROBOT = GroupName "robot"
pattern MOBILE_ROBOT = GroupName "mobile robot"
pattern IMMOBILE_ROBOT = GroupName "immobile robot"
pattern CONSTRUCTION_ROBOT = GroupName "construction robot"
pattern MECHANICAL_CONTRAPTION = GroupName "mechanical contraption"
pattern GAUNTLET_ROBOT = GroupName "virus-infested robot"

-- ** Common
pattern S_WOODEN_TORCH = GroupName "wooden torch"
pattern S_SANDSTONE_ROCK = GroupName "sandstone rock"

pattern ADD_SIGHT = GroupName "sight improvement"
pattern ARMOR_RANGED = GroupName "ranged armor"
pattern ADD_NOCTO_1 = GroupName "noctovision improvement"
pattern WEAK_ARROW = GroupName "weak arrow"
pattern LIGHT_ATTENUATOR = GroupName "light attenuator"
pattern FIREPROOF_CLOTH = GroupName "fireproof cloth"
pattern RING_OF_OPPORTUNITY_SNIPER = GroupName "ring of sniper"
pattern ANY_ARROW = GroupName "arrow"
pattern STARTING_ARMOR = GroupName "starting armor"
pattern STARTING_WEAPON = GroupName "starting weapon"
pattern GEM = GroupName "gem"

-- ** Allure-specific
pattern S_BULLTEPROOF_VEST = GroupName "bulletproof vest"
pattern S_PERFUME_POTION = GroupName "perfume potion"
pattern S_EMPTY_FLASK = GroupName "empty flask"

pattern COOKED_FOOD = GroupName "cooked food"
pattern MERCENARY_WEAPON = GroupName "mercenary weapon"
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
pattern POLE = GroupName "pole"
pattern POLE_OR_HANDLE = GroupName "pole or handle"
pattern BREACHING_TOOL = GroupName "breaching tool"
pattern BONDING_TOOL = GroupName "bonding tool"
pattern SHARPENING_TOOL = GroupName "sharpening tool"
pattern WIRECUTTING_TOOL = GroupName "wirecutting tool"

-- * Content

actors :: [ItemKind]
actors =
  [warrior, warrior2, exterminator, raider, scout, ranger, escapist, ambusher, brawler, fighter, mercenary, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, elbowTank, intruder, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, giantOctopus, lion, rhinoceros, beeSwarm, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, coolingFaucet, medbotFaucet, dustFaucet, fuelFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, wasteRobotNoEqp, lightRobot, heavyRobot, weldedRobot, cleanerRobot]

warrior,    warrior2, exterminator, raider, scout, ranger, escapist, ambusher, brawler, fighter, mercenary, civilian, civilian2, civilian3, civilian4, civilian5, eye, fastEye, nose, elbow, elbowTank, intruder, torsor, goldenJackal, griffonVulture, skunk, armadillo, gilaMonster, rattlesnake, hyena, komodoDragon, alligator, giantOctopus, lion, rhinoceros, beeSwarm, hornetSwarm, thornbush, razorwireFence, electricFence, activeFence, steamFaucet, coolingFaucet, medbotFaucet, dustFaucet, fuelFaucet, surveillanceDrone, shepherdDrone, huntingDrone, homeRobot, wasteRobot, wasteRobotNoEqp, lightRobot, heavyRobot, weldedRobot, cleanerRobot :: ItemKind

-- Note that the actors that appear in the crawl scenario should
-- be generated with at most ordinary ammo. Otherwise, farming them
-- may be rational though boring endeavour. Any exceptions to that
-- should be well thought of. E.g., unique guaranteed items on bosses
-- are safe, just as restricted kinds of weak items.

-- * Hunams

-- TODO: bring back S_EAR_3 when character progression permits hearing boosts.
humanOrgans :: [(GroupName ItemKind, CStore)]
humanOrgans = [ (S_FIST, COrgan), (S_FOOT, COrgan)
              , (S_EYE_6, COrgan), (S_EAR_6, COrgan)
              , (S_SAPIENT_BRAIN, COrgan)
              , (S_ANIMAL_STOMACH, COrgan), (S_HUNGRY, COrgan)
              , (BACKSTORY, COrgan) ]
                  -- TODO: when enough backstory items created,
                  -- include many backstory categories here instead and add
                  -- a bit of variation among warriors, some kinds
                  -- getting a good and a bad where another test a mixed one
                  -- instead, etc. Characters should still be recognizable
                  -- and slighty different backstory emphasis between games
                  -- matches human plasticity.
warrior = ItemKind
  { isymbol  = toContentSymbol '@'
  , iname    = "adventurer"  -- modified if initial actors in hero faction
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
  , ikit     = humanOrgans
               ++ [ (GENETIC_FLAW_10, COrgan)
                  , (S_EMPTY_FLASK, CStash)
                  , (COMMON_ITEM, CStash) ]
  , idesc    = "A human equipped for an adventure."
  }
warrior2 = warrior
  { ifreq    = [(CRAWL_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (GENETIC_FLAW_10, COrgan)
                  , (S_EMPTY_FLASK, CStash)
                  , (COOKED_FOOD, CStash) ]
  }
exterminator = warrior
  { ifreq    = [(EXTERMINATOR_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (GENETIC_FLAW_10, COrgan)
                  , (STARTING_WEAPON, CEqp)
                  , (ARMOR_RANGED, CEqp) ]
  , idesc    = "A human self-defending with makeshift equipment."
  }
raider = warrior
  { ifreq    = [(RAIDER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans
               ++ [ (GENETIC_FLAW_10, COrgan)
                  , (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash) ]
  , idesc    = "A human equipped for a raid."
  }
scout = warrior
  { ifreq    = [(SCOUT_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (ADD_SIGHT, CEqp)
                  , (ARMOR_RANGED, CEqp)
                  , (ADD_NOCTO_1, CStash) ]
  , idesc    = "A human equipped for scouting."
  }
ranger = warrior
  { ifreq    = [(RANGER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (ARMOR_RANGED, CEqp)
                  , (WEAK_ARROW, CStash) ]
  , idesc    = "A human equipped for ranged fight."
  }
escapist = warrior
  { ifreq    = [(ESCAPIST_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (ADD_SIGHT, CEqp)
                  , (STARTING_ARMOR, CEqp)
                  , (WEAK_ARROW, CStash)  -- mostly for probing
                  , (LIGHT_ATTENUATOR, CStash)
                  , (S_WOODEN_TORCH, CStash)
                  , (FIREPROOF_CLOTH, CStash) ]
  , idesc    = "A human equipped for an escape."
  }
ambusher = warrior
  { ifreq    = [(AMBUSHER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- dark and numerous, so more kit without exploring
               ++ [ (RING_OF_OPPORTUNITY_SNIPER, CEqp)
                  , (ANY_ARROW, CStash), (ANY_ARROW, CStash)
                  , (WEAK_ARROW, CStash)
                  , (EXPLOSIVE, CStash)
                  , (LIGHT_ATTENUATOR, CEqp)
                  , (S_WOODEN_TORCH, CStash) ]
  , idesc    = "A human equipped for an ambush."
  }
brawler = warrior
  { ifreq    = [(BRAWLER_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (STARTING_WEAPON, CEqp)
                  , (ANY_POTION, CStash) ]
  , idesc    = "A human equipped for melee fight."
  }
fighter = brawler
  { ifreq    = [(SOLDIER_HERO, 100), (MOBILE, 1)]
  , ikit     = ikit brawler
               ++ [ (STARTING_WEAPON, CEqp)
                  , (EXPLOSIVE, CStash) ]
  , idesc    = "A human equipped for intense combat."
  }
mercenary = brawler
  { iname    = "mercenary"  -- different name, because a very distinct faction
  , ifreq    = [(MERCENARY_HERO, 100), (MOBILE, 1)]
  , ikit     = humanOrgans  -- no flaw
               ++ [ (MERCENARY_WEAPON, CEqp)
                  , (S_BULLTEPROOF_VEST, CEqp)
                  , (MERCENARY_AMMO, CStash)
                  , (EXPLOSIVE, CStash) ]
  , idesc    = "A professional security contractor."
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

eye = ItemKind  -- depends on items it finds rather than special organs
  { isymbol  = toContentSymbol 'w'
  , iname    = "beckoning walker"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 10) ]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 10), (10, 8)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 16, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can use even cultural artifacts
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Walks with a stately dignity. You read death in the slow beckoning gestures of its revolting upper appendages."
  , ikit     = [ (S_FOOT, COrgan), (S_TENTACLE, COrgan)
               , (S_BARK, COrgan), (S_EYE_6, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]  -- no voice, no hearing
  }
fastEye = ItemKind  -- glass cannon; depends mostly on items it finds
  { isymbol  = toContentSymbol 'b'
  , iname    = "rolling biter"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 60) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 3), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkHurtMelee 20
               , AddSkill SkAggression 1
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It bites as blindingly fast as it runs. Or rolls? Also, cuts and pierces."
  , ikit     = [ (S_JAW, COrgan), (S_RAZOR, COrgan), (S_HORN, COrgan)
               , (S_SMALL_CLAW, COrgan)  -- at least one non-timed, not deadly
               , (S_SPEED_GLAND_10, COrgan)  -- -30 armor with horn, +20 melee
               , (S_EYE_3, COrgan), (S_EAR_3, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]
  }
nose = ItemKind  -- sniffs only; a tank requiring multiple weapon hits to beat;
                 -- no armor, so special kinds of damage don't help;
                 -- slow, but despite that, danger when strong weapons wielded!
  { isymbol  = toContentSymbol 'h'
  , iname    = "crawling horror"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1), (MOBILE_MONSTER, 100)
               , (AQUATIC, 30), (AQUATIC_MONSTER, 30) ]  -- likes liquids
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 5), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 60, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 15, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject (-1)  -- can't project
               , AddSkill SkSwimming 30
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A blind, slimy mass of fluid tissue. You'd think it's powerless, but as soon as it touches your trembling body, slapping, stinging and burning, it won't let go."
  , ikit     = [ (S_TIP, COrgan), (S_LIP, COrgan)  -- at least one non-timed
               , (S_STING, COrgan)
               , (S_NOSTRIL, COrgan), (S_HUNGRY, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]  -- no sight nor hearing
  }
elbow = ItemKind
  { isymbol  = toContentSymbol 's'
  , iname    = "creepy shooter"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1)
               , (MOBILE_MONSTER, 100), (SCOUT_MONSTER, 30) ]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 1), (10, 12)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 12, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can use even cultural artifacts
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It moves in sudden jerks and never makes a noise. Speaks in hard objects hurled at deadly speeds."
  , ikit     = [ (S_SPEED_GLAND_5, COrgan)  -- -25 armor
               , (S_EYE_6, COrgan), (S_EAR_8, COrgan)
                   -- too powerful to get stronger sight
               , (S_SAPIENT_BRAIN, COrgan)
               , (ANY_ARROW, CEqp), (ANY_ARROW, CStash)
               , (WEAK_ARROW, CEqp), (WEAK_ARROW, CStash) ]
  }

-- * Allure-specific aliens

elbowTank = elbow
  { iname    = "armored shooter"
  , iflavour = zipFancy [Magenta]
  , irarity  = [(10, 0), (40, 30)]  -- only appears among late spawns
  , ikit     = ikit elbow ++ [(S_ARMORED_SKIN, COrgan), (S_JET_BOOSTER, COrgan)]
  }
-- TODO: the main fun in this actor will be chain explosions and for this,
-- it needs to spawn in groups and move in groups. Neither is implemented yet.
-- For now, we try to amass the actors on arena levels over water and in rooms.
-- Low HP is needed to ensure the chain reaction. Lack of ranged combat
-- makes the rule to attack it from a distance straightforward.
intruder = ItemKind
  { isymbol  = toContentSymbol 'i'
  , iname    = "bobbing intruder"
  , ifreq    = [ (MONSTER, 100), (MOBILE, 1), (MOBILE_MONSTER, 100)
               , (EXPLOSIVE_MONSTER, 100)
               , (AQUATIC, 1) ]  -- neutral to water, unlike other actors
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 5), (10, 9)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 6, AddSkill SkMaxCalm 50
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkAggression 1
               , AddSkill SkProject (-1)  -- can't project
               , AddSkill SkApply 1  -- can use even cultural artifacts
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It starts, bobs and halts, scanning for movement. Effortlessly it resumes covering the ground, gliding two meters above the floor. Its pumped organic body, though large, pulses with fragility. It doesn't skirt the walls and instead takes ownership of any space it boldly parks in the middle of."
  , ikit     = [ (S_FLOTATION_BAG, COrgan)
               , (S_TENTACLE, COrgan), (S_TENTACLE, COrgan)
               , (S_TIP, COrgan)  -- at least one non-timed
               , (S_HOOKED_CLAW, COrgan)
               , (S_EYE_6, COrgan), (S_EAR_6, COrgan)
               , (S_SAPIENT_BRAIN, COrgan) ]  -- no voice, no hearing
  }

-- * Alien uniques

torsor = ItemKind
  { isymbol  = toContentSymbol 'M'
  , iname    = "The Maker"
  , ifreq    = [(MONSTER, 100), (MOBILE, 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(14 * 10/16, 0), (15 * 10/16, 1000)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique, ELabel "of Contact"
               , AddSkill SkMaxHP 400  -- -50 melee armor, so higher HP
               , AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 5
               , AddSkill SkNocto 2
               , AddSkill SkAggression 3
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkApply 1  -- can use even cultural artifacts
               , AddSkill SkAlter (-1)  -- can't exit the gated level; a boss,
                                        -- but can dig rubble, ice
               , SetFlag Durable ]
  , ieffects = [OnSmash $ VerbMsg "mount the last plea for mutual understanding and voluntary exchange of body parts" "."]
  , idesc    = "The mind, the big heart behind it all. Warmth and sympathy pour out through the graceful undulation of tentacles, sharp claws, snapping jaw and dripping fangs."
  , ikit     = [ (S_TENTACLE, COrgan), (S_HOOKED_CLAW, COrgan)
               , (S_LARGE_JAW, COrgan), (S_VENOM_FANG, COrgan)
               , (S_SMALL_CLAW, COrgan)  -- at least one non-timed, not deadly
               , (S_SPEED_GLAND_10, COrgan)
               , (S_EYE_6, COrgan), (S_EAR_8, COrgan)
               , (S_SAPIENT_BRAIN, COrgan)
               , (GEM, CStash), (GEM, CStash)
               , (GEM, CStash), (GEM, CStash) ]
  }

-- * Animals

-- They need rather strong melee, because they don't use items.
-- They have dull colors, except for yellow, because there is no dull variant.

goldenJackal = ItemKind  -- basically a much smaller, slower and nosy hyena
  { isymbol  = toContentSymbol 'j'
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
               , AddSkill SkAggression 2  -- scout
               , AddSkill SkDisplace 1  -- scout
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An opportunistic predator, feeding on carrion and the weak."
  , ikit     = [ (S_SMALL_JAW, COrgan)
               , (S_POWERFUL_HIND_LEGS, COrgan)  -- useful for aggressive actor
               , (S_EYE_6, COrgan), (S_NOSTRIL, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_3, COrgan) ]
  }
griffonVulture = ItemKind  -- keep it boring and weak, because it summons
  { isymbol  = toContentSymbol 'v'
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
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
      -- Animals don't have leader, usually, so even if only one on level,
      -- it pays the communication overhead, so the speed is higher to get
      -- them on par with human leaders moving solo.
  , ieffects = []
  , idesc    = "It soars high above, searching for vulnerable prey."
  , ikit     = [ (S_SCREECHING_BEAK, COrgan)  -- in reality it grunts and hisses
               , (S_SMALL_CLAW, COrgan)
               , (S_EYE_8, COrgan), (S_EAR_8, COrgan)
                   -- can't shoot, so strong sight is OK
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_3, COrgan) ]
  }
skunk = ItemKind
  { isymbol  = toContentSymbol 's'
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
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkOdor 5  -- and no smell skill, to let it leave smell
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Its only defence is the terrible stench."
  , ikit     = [ (S_SCENT_GLAND, COrgan)
               , (S_SMALL_CLAW, COrgan), (S_SNOUT, COrgan)
               , (S_EYE_3, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_3, COrgan) ]
  }
armadillo = ItemKind  -- a tank with armor, so special damage defeats it
  { isymbol  = toContentSymbol 'a'
  , iname    = "giant armadillo"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 7)]
  , iverbHit = "thud"
  , iweight  = 54000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkHurtMelee (-70)  -- quite harmless rolled in a ball
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "When threatened, it rolls into a ball."
  , ikit     = [ (S_HOOKED_CLAW, COrgan), (S_SNOUT, COrgan)
               , (S_ARMORED_SKIN, COrgan), (S_ARMORED_SKIN, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_3, COrgan)
               , (RAW_MEAT_CHUNK, CEqp) ]
  }
gilaMonster = ItemKind
  { isymbol  = toContentSymbol 'g'
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
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Numbing venom ensures that even the fastest prey has no escape."
  , ikit     = [ (S_VENOM_TOOTH, COrgan), (S_SMALL_CLAW, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)  -- small reptile, hungers slowly
               , (GENETIC_FLAW_3, COrgan) ]
  }
rattlesnake = ItemKind
  { isymbol  = toContentSymbol 's'
  , iname    = "rattlesnake"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(5, 1), (10, 7), (20, 10)]  -- common among late spawns
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 28, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 16, AddSkill SkNocto 2
               , AddSkill SkAggression 2  -- often discharged. so flees anyway
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Beware its rattle - it serves as a warning of an agonising death."
  , ikit     = [ (S_VENOM_FANG, COrgan)  -- when discharged, it's weaponless
               , (S_COILED_TAIL, COrgan)  -- useful for an aggressive actor
               , (S_RATLLE, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)  -- small reptile, hungers slowly
               , (GENETIC_FLAW_3, COrgan)
               , (RAW_MEAT_CHUNK, CEqp) ]
  }
hyena = ItemKind
  { isymbol  = toContentSymbol 'h'
  , iname    = "spotted hyena"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (SCAVENGER, 20) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 1), (10, 5), (20, 10)]
      -- gets summoned often, so low base rarity, except among late spawns
  , iverbHit = "thud"
  , iweight  = 60000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 23, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 32, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Skulking in the shadows, waiting for easy prey."
  , ikit     = [ (S_JAW, COrgan), (S_SMALL_CLAW, COrgan)
               , (S_EYE_6, COrgan), (S_NOSTRIL, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_3, COrgan) ]
  }
komodoDragon = ItemKind
  { isymbol  = toContentSymbol 'k'
  , iname    = "Komodo dragon"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [BrRed]  -- speedy, so bright red
  , icount   = 1
  , irarity  = [(9, 0), (10, 11), (20, 20)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 40, AddSkill SkMaxCalm 60  -- regens
               , AddSkill SkSpeed 17, AddSkill SkNocto 2
               , AddSkill SkHurtMelee 60  -- great fighter with low cooldowns
               , AddSkill SkAggression 1  -- match the description
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Larger and more aggressive than any other lizard, but as easily recovering from wounds as its lesser cousins."
  , ikit     = [ (S_LARGE_TAIL, COrgan), (S_JAW, COrgan)
               , (S_LIP, COrgan), (S_FOOT, COrgan)
               , (S_SPEED_GLAND_5, COrgan), (S_ARMORED_SKIN, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_3, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan)
               , (GENETIC_FLAW_3, COrgan)  -- not to wake it up too soon
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
  }
alligator = ItemKind  -- late, slow, deadly semi-tank with some armor;
                      -- too deadly to get more HP; bombs the only recourse
  { isymbol  = toContentSymbol 'a'
  , iname    = "alligator"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (AQUATIC, 70), (AQUATIC_ANIMAL, 70) ]  -- amphibious
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(9, 0), (10, 10), (20, 10), (40, 40)]
      -- extra spawns in water, so lower rarity, except among late spawns
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
  , ikit     = [ (S_HUGE_TAIL, COrgan)  -- the special trick, breaking frontline
               , (S_LARGE_JAW, COrgan)
               , (S_SMALL_CLAW, COrgan)
               , (S_ARMORED_SKIN, COrgan)
               , (S_EYE_6, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_10, COrgan)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
  }

-- * Allure-specific animals

giantOctopus = ItemKind
  { isymbol  = toContentSymbol 'o'
  , iname    = "giant octopus"
  , ifreq    = [ (ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)
               , (AQUATIC, 90), (AQUATIC_ANIMAL, 90) ]  -- weak on land
  , iflavour = zipPlain [BrMagenta]  -- very exotic, so bright color
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 5), (7, 3)]
                 -- weak, but non-standard behaviour, so spoils initial mastery;
                 -- later on not too common, because annoying
  , iverbHit = "thud"
  , iweight  = 72000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 80
               , AddSkill SkSwimming 100  -- swims better than walks
               , AddSkill SkSpeed 27, AddSkill SkNocto 3 -- good night vision
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "It has eight arms of rage and sees through the night. Copes with lower gravity better than most animals."  -- TODO: change when slowness on land is implemented
  , ikit     = [ (S_INK_SAC, COrgan)
               , (S_TENTACLE, COrgan), (S_TENTACLE, COrgan)
               , (S_TENTACLE, COrgan), (S_TENTACLE, COrgan)
               , (S_SMALL_BEAK, COrgan)  -- TODO: use when tentacles torn out
               , (S_EYE_8, COrgan)
                   -- shots not too damaging, so can have strong sight
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_3, COrgan)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
 }
lion = ItemKind  -- emphatically not a tank
  { isymbol  = toContentSymbol 'l'
  , iname    = "Lion"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(10, 0), (30, 50)]  -- only appears among late spawns
  , iverbHit = "thud"
  , iweight  = 140000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 120, AddSkill SkMaxCalm 80
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkHurtMelee 60  -- great fighter
               , AddSkill SkAggression 2  -- late spawn
               , AddSkill SkApply 1  -- can eat food despite the genetic flaw
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "At the repeated violation of their pride area, the irritated felines emerge from hiding."
  , ikit     = [ (S_POWERFUL_HIND_LEGS, COrgan)
               , (S_LARGE_JAW, COrgan), (S_SMALL_CLAW, COrgan)
               , (S_EYE_6, COrgan), (S_EAR_6, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan), (GENETIC_FLAW_10, COrgan)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
  }

-- * Animal uniques

rhinoceros = ItemKind  -- impressive tank boss with some armor
  { isymbol  = toContentSymbol 'R'
  , iname    = "Billy"
  , ifreq    = [(ANIMAL, 100), (MOBILE, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(7 * 10/16, 0), (8 * 10/16, 1000), (9 * 10/16, 0)]  -- unique
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique, ELabel "the Maddened Rhinoceros"
               , AddSkill SkMaxHP 200, AddSkill SkMaxCalm 60  -- quite late
               , AddSkill SkSpeed 27, AddSkill SkNocto 2
               , AddSkill SkHurtMelee 50  -- mass gives extra damage
               , AddSkill SkAggression 2
               , AddSkill SkAlter (-1)  -- can't use hard stairs nor dig;
                                        -- a weak miniboss; can use easy stairs
               , SetFlag Durable ]
  , ieffects = [OnSmash $ VerbMsg "bellow triumphantly!" ""]
  , idesc    = "The last of its kind. Blind with rage, or perhaps due to the postoperative scars. A huge mass of muscle that charges at deadly speed."
  , ikit     = [ (S_RHINO_HORN, COrgan), (S_FOOT, COrgan)
               , (S_RHINO_INERTIA, COrgan), (S_ARMORED_SKIN, COrgan)
               , (S_EYE_3, COrgan), (S_EAR_8, COrgan)
               , (S_ANIMAL_BRAIN, COrgan)
               , (S_ANIMAL_STOMACH, COrgan)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp)
               , (RAW_MEAT_CHUNK, CEqp), (RAW_MEAT_CHUNK, CEqp) ]
  }

-- * Non-animal animals

beeSwarm = ItemKind
  { isymbol  = toContentSymbol 'b'
  , iname    = "bee swarm"
  , ifreq    = [(ANIMAL, 100), (INSECT, 50), (MOBILE, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 3), (10, 4)]
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2  -- armor in sting
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Every bee would die for the queen."
  , ikit     = [ (S_BEE_STING, COrgan)  -- weaponless when it's used up
               , (S_VISION_6, COrgan), (S_EAR_6, COrgan)
               , (S_INSECT_MORTALITY, COrgan), (S_ANIMAL_BRAIN, COrgan) ]
  }
hornetSwarm = ItemKind  -- kind of tank with armor, but short-lived
  { isymbol  = toContentSymbol 'h'
  , iname    = "hornet swarm"
  , ifreq    = [(ANIMAL, 100), (INSECT, 100), (MOBILE, 1), (MOBILE_ANIMAL, 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(5, 1), (10, 4), (20, 10)]
      -- should be many, because die after a time
  , iverbHit = "buzz"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkHurtMelee 50
               , AddSkill SkMaxHP 10, AddSkill SkMaxCalm 70
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkAlter (-2)  -- can't use hard stairs nor doors
               , AddSkill SkWait (-2)  -- can't brace, sleep and lurk
               , AddSkill SkFlying 10  -- flies slowly, but far
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A vicious cloud of stings and hate."
  , ikit     = [ (S_STING, COrgan)  -- when on cooldown, it's weaponless
               , (S_VISION_6, COrgan), (S_EAR_6, COrgan)
               , (S_INSECT_MORTALITY, COrgan), (S_ANIMAL_BRAIN, COrgan) ]
  }
thornbush = ItemKind  -- the wimpiest kind of early tank
  { isymbol  = toContentSymbol 't'
  , iname    = "thornbush"
  , ifreq    = [(ANIMAL, 25), (IMMOBILE_ANIMAL, 40)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 13)]
  , iverbHit = "scrape"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Each branch bears long, curved thorns."
  , ikit     = [ (S_THORN, COrgan)  -- after all run out, it's weaponless
               , (S_BARK, COrgan) ]
  }

-- * Robots, Allure-specific

-- Robots have any colors but only f, d and r letters. Avoid these letters
-- for other factions.

razorwireFence = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "razorwire fence"
  , ifreq    = [(ROBOT, 15), (IMMOBILE_ROBOT, 10), (MECHANICAL_CONTRAPTION, 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 20), (10, 4)]
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
  , ikit     = [(S_RAZOR, COrgan), (S_THORN, COrgan)]
  }
electricFence = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "electric fence"
  , ifreq    = [(ROBOT, 40), (IMMOBILE_ROBOT, 10), (MECHANICAL_CONTRAPTION, 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 10), (10, 10), (20, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 30, AddSkill SkMaxCalm 999
                   -- no armor, because regenerates; high HP instead
               , AddSkill SkSpeed 40, AddSkill SkNocto 2, AddSkill SkShine 3
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Marginally intelligent electric shepherd. Originally used in orbital dairy farms and planetary zoos. The long support on which proximity sensors, actuators and wires are socketed, ensures animals can't jump above the fence, even in reduced gravity."
  , ikit     = [ (S_LIVE_WIRE, COrgan), (ELECTRIC_AMBIENCE, COrgan)
               , (POLE_OR_HANDLE, CEqp) ]
  }
activeFence = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "active fence"
  , ifreq    = [(ROBOT, 30), (IMMOBILE_ROBOT, 20), (MECHANICAL_CONTRAPTION, 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(5 * 10/16, 0), (10, 7), (20, 10)]
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
  , ikit     = [ (S_VISION_6, COrgan)
               , (NEEDLE, CStash), (CAN_OF_STICKY_FOAM, CStash) ]
                   -- can of sticky foam is exploitable, but it spawns
                   -- reasonably often only on one level and not for
                   -- a long period
  }
steamFaucet = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "steam faucet"
  , ifreq    = [(ROBOT, 8), (IMMOBILE_ROBOT, 15), (MECHANICAL_CONTRAPTION, 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 7, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A cracked valve on one of the superheated water pipes spreading radially outward from the tokamak level."
  , ikit     = [(S_BOILING_VENT, COrgan), (S_BOILING_FISSURE, COrgan)]
  }
coolingFaucet = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "cooling faucet"
  , ifreq    = [(ROBOT, 8), (IMMOBILE_ROBOT, 15), (MECHANICAL_CONTRAPTION, 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 22, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "An emergency pressure-release vent on a supercooling circuit reservoir."
  , ikit     = [(S_COOLING_VENT, COrgan), (S_COOLING_FISSURE, COrgan)]
  }
medbotFaucet = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "nano medbot faucet"
  , ifreq    = [(ROBOT, 10), (IMMOBILE_ROBOT, 100), (MECHANICAL_CONTRAPTION, 1)]
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
  , ikit     = [(S_MEDBOT_VENT, COrgan), (S_MEDBOT_FISSURE, COrgan)]
  }
dustFaucet = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "dust faucet"
  , ifreq    = [ (ROBOT, 4)  -- usually nothing to ignite
               , (IMMOBILE_ROBOT, 30)  -- except when other faucets around
               , (MECHANICAL_CONTRAPTION, 1) ]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(3, 20), (10, 6)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 11, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A torn pipeline for venting flammable powders filtered from cargo areas out into the void, where they cannot ignite. Depending on the pressure in subsidiary ducts, it may contain dust of aluminum, magnesium, titanium, flour, starch, various nitrates and perchlorates."
  , ikit     = [(S_DUST_VENT, COrgan), (S_DUST_FISSURE, COrgan)]
  }
fuelFaucet = ItemKind
  { isymbol  = toContentSymbol 'f'
  , iname    = "burning fuel faucet"
  , ifreq    = [(ROBOT, 30), (MECHANICAL_CONTRAPTION, 1)]
      -- common not in outermost level but in the dungeon, because it's
      -- self-contained, providing both fuel and ignition
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 6), (10, 12), (20, 10)]
  , iverbHit = "thud"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 10, AddSkill SkMaxCalm 999
               , AddSkill SkSpeed 11, AddSkill SkNocto 2
               , AddSkill SkWait 1, AddSkill SkMelee 1  -- no brain
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Fuel station gone wrong. Multiple emergency subsystems added over the years, owing to valiant regulatory lawmaking efforts, keep it from exploding by turning off and on the vent and each other in a complex cyclical pattern."
  , ikit     = [(S_FUEL_VENT, COrgan), (S_FUEL_FISSURE, COrgan)]
  }
surveillanceDrone = ItemKind
  { isymbol  = toContentSymbol 'd'
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
               , AddSkill SkSpeed 25, AddSkill SkNocto 2
               , AddSkill SkMoveItem (-1)  -- almost as dumb as an animal
               , AddSkill SkProject (-1)
               , AddSkill SkMelee (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A video camera in each room would violate privacy of passengers, hence surveillance drones. Programmed to be easy to fend off, they try to keep a respectful distance, even at the cost to themselves."
  , ikit     = [ (S_JET_BOOSTER, COrgan)
               , (S_VISION_16, COrgan), (S_ROBOT_BRAIN, COrgan) ]
  }
shepherdDrone = ItemKind
  { isymbol  = toContentSymbol 'd'
  , iname    = "oversight drone"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 100), (GAUNTLET_ROBOT, 150) ]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 3), (10, 4)]  -- gets summoned often, so low base rarity
  , iverbHit = "clank"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 80, AddSkill SkArmorRanged 40
               , AddSkill SkMaxHP 10, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 25, AddSkill SkNocto 2
               , AddSkill SkAggression 2  -- scout
               , AddSkill SkMoveItem (-1)  -- almost as dumb as an animal
               , AddSkill SkProject (-1)
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "A tiny airborne robot designed to take construction measurements, synchronize robot workers and report irregularities. It seems to be in need of resetting itself."
  , ikit     = [ (S_JET_BOOSTER, COrgan), (S_LIVE_WIRE, COrgan)
               , (S_VISION_16, COrgan), (S_EAR_8, COrgan)
               , (S_ROBOT_BRAIN, COrgan) ]
  }
huntingDrone = ItemKind
  { isymbol  = toContentSymbol 'd'
  , iname    = "hunting drone"
  , ifreq    = [(ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(4, 0), (5, 5), (10, 8), (20, 10)]
  , iverbHit = "clank"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , AddSkill SkMaxHP 10, AddSkill SkMaxCalm 60
               , AddSkill SkSpeed 30, AddSkill SkNocto 2
               , AddSkill SkMoveItem (-1)  -- almost as dumb as an animal
               , AddSkill SkMelee (-1)  -- but can project
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Originally designed for hunting down and putting to sleep stray animals. The sleeping agent has long since dried up and the propulsion got rather unpredictable."
  , ikit     = [ (S_JET_BOOSTER, COrgan)
               , (S_EYE_8, COrgan), (S_NOSTRIL, COrgan), (S_EAR_8, COrgan)
                   -- week projectiles, so strong sight OK
               , (S_ROBOT_BRAIN, COrgan)
               , (NEEDLE, CStash), (TRANQUILIZER_DART, CStash) ]
  }
homeRobot = ItemKind
  { isymbol  = toContentSymbol 'r'
  , iname    = "feral home robot"
               -- TODO: name another 'deranged', tertiary imperative: survival
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (GAUNTLET_ROBOT, 100) ]
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
  , ikit     = [ (S_FIST, COrgan)
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_3, COrgan)
               , (S_ROBOT_BRAIN, COrgan) ]
  }
wasteRobot = ItemKind  -- not a tank, because smell-only alien is already a tank
  { isymbol  = toContentSymbol 'r'
  , iname    = "waste disposal robot"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 100) ]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(5, 9)]  -- gets summoned often, so low base rarity
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 20, AddSkill SkMaxCalm 30
               , AddSkill SkSpeed 15, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "You are not in its database, hence you are waste. It can't see and you smell funny, so that's it."
  , ikit     = [ (S_TENTACLE, COrgan), (S_SNOUT, COrgan)
               , (S_NOSTRIL, COrgan)  -- only smell, for variety
               , (S_ROBOT_BRAIN, COrgan)
               , (WASTE_CONTAINER, CEqp) ]
  }
wasteRobotNoEqp = wasteRobot  -- no drops, for gauntlet and to avoid junk
  { ifreq    = [ (MOBILE, 100), (GAUNTLET_ROBOT, 250) ]
  , ikit     = [ (S_TENTACLE, COrgan), (S_SNOUT, COrgan)
               , (S_NOSTRIL, COrgan)  -- only smell, for variety
               , (S_ROBOT_BRAIN, COrgan) ]
  }
lightRobot = ItemKind
  { isymbol  = toContentSymbol 'r'
  , iname    = "decoration robot"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 100) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(3 * 10/16, 0), (4 * 10/16, 6), (10, 6)]
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
  , ikit     = [ (S_HOOKED_CLAW, COrgan), (S_TENTACLE, COrgan), (S_FOOT, COrgan)
               , (S_HULL_PLATING, COrgan)
               , (S_EYE_6, COrgan), (S_EAR_8, COrgan)
               , (S_ROBOT_BRAIN, COrgan)
               , (CONSTRUCTION_HOOTER, CEqp) ]
  }
heavyRobot = ItemKind  -- summoning tank with armor, but fortunately weak
                       -- weapons; danger when strong weapons wielded!
  { isymbol  = toContentSymbol 'r'
  , iname    = "demolition robot"
  , ifreq    = [ (ROBOT, 100), (MOBILE, 100), (MOBILE_ROBOT, 100)
               , (CONSTRUCTION_ROBOT, 70) ]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(7 * 10/16, 0), (8 * 10/16, 4), (10, 13), (30, 30)]
  , iverbHit = "clank"
  , iweight  = 800000
  , idamage  = 0
  , iaspects = [ AddSkill SkMaxHP 60, AddSkill SkMaxCalm 40
                   -- can't summon again for a long time;
                   -- loses a lot of sight after summoning
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , AddSkill SkProject 2  -- can lob
               , AddSkill SkAlter 3  -- uses all stairs
               , AddSkill SkApply 1  -- can apply the hooter
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Heavy multi-purpose construction robot. Excels at discharging, dismantling and demolition."
  , ikit     = [ (S_HORN, COrgan), (S_FIST, COrgan), (S_SMALL_CLAW, COrgan)
               , (S_HULL_PLATING, COrgan)
               , (S_EYE_3, COrgan), (S_EAR_6, COrgan)
               , (S_ROBOT_BRAIN, COrgan)
               , (SPOTLIGHT, CEqp), (CONSTRUCTION_HOOTER, CEqp) ]
  }

-- * Robot uniques, Allure-specific

weldedRobot = ItemKind
  { isymbol  = toContentSymbol 'L'
  , iname    = "Bob"
  , ifreq    = [(ROBOT, 100), (IMMOBILE_ROBOT, 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1 * 10/16, 1000), (2 * 10/16, 0)]  -- unique
  , iverbHit = "clank"
  , iweight  = 80000
  , idamage  = 0
  , iaspects = [ SetFlag Unique, ELabel "the Welded Luggage Robot"
               , AddSkill SkMaxHP 200, AddSkill SkMaxCalm 100
               , AddSkill SkSpeed 20, AddSkill SkNocto 2
               , SetFlag Durable ]
  , ieffects = [OnSmash $ VerbMsg "lament dying with disfigured and welded feet that nobody wanted to fix with the blowtorch" "."]
  , idesc    = "A well-built humanoid luggage unloading robot with a smooth silvery satin skin. Its graceful moves are stunted by a thick irregular weld fastening both its shapely legs to the floor. A whiff of smoke escapes whenever it opens its mouth in a charming toothy smile while brandishing a blowtorch in its trembling hand. Blowtorch! That's the key to open the welded staircase."
  , ikit     = [ (S_FIST, COrgan)
               , (S_EYE_6, COrgan), (S_EAR_3, COrgan)
               , (S_MOUTH_VENT, COrgan)
               , (S_ROBOT_BRAIN, COrgan)
               , (S_CRUDE_WELD, COrgan)
               , (S_CURRENCY, CGround) -- to ensure newbies know to visit
               , (BLOWTORCH, CEqp)
               , (S_PERFUME_POTION, CStash), (WIRECUTTING_TOOL, CStash) ]
                   -- establish stash to ensure heroes pick up blowtorch ASAP
  }
cleanerRobot = ItemKind
  { isymbol  = toContentSymbol 'C'
  , iname    = "The Void Cleaner Robot"
  , ifreq    = [(ROBOT, 100), (MOBILE, 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(11 * 10/16, 0), (12 * 10/16, 1000), (13 * 10/16, 0)]  -- unique
  , iverbHit = "clank"
  , iweight  = 800000
  , idamage  = 0
  , iaspects = [ SetFlag Unique
               , AddSkill SkMaxHP 120
                   -- doubly regenerates and huge armor (72), so lower HP
               , AddSkill SkMaxCalm 40
                   -- can't summon again for a long time;
                   -- loses a lot of sight after summoning
               , AddSkill SkSpeed 18, AddSkill SkNocto 2
               , AddSkill SkAggression 1
                   -- can't use hard stairs nor dig; a weak miniboss;
                   -- however, it can use the easy stairs and so change levels
               , AddSkill SkApply 1  -- can apply the hooter
               , SetFlag Durable ]
  , ieffects = [OnSmash $ VerbMsg "clumsily try to pick up a dust speck" "."]
  , idesc    = "An oversize waste disposal robot repaired with parts from a demolition robot, including a scaled up goal matrix. The cosmic void is now the only acceptable model of cleanliness. The robot's bulky trunk doesn't fit into even the larger lift carriages."
  , ikit     = [ (S_TENTACLE, COrgan), (S_SNOUT, COrgan)
               , (S_HORN, COrgan), (S_SMALL_CLAW, COrgan)  -- no fist
               , (S_LIVE_WIRE, COrgan)  -- patched from parts
               , (ELECTRIC_AMBIENCE, COrgan)  -- regeneration
               , (S_BOILING_VENT, COrgan)  -- regeneration
               , (S_HULL_PLATING, COrgan)
                   -- the only such armor, except for weak animals; plus
                   -- the WASTE_CONTAINER, so only hammers and proj effective
               , (S_EYE_3, COrgan), (S_NOSTRIL, COrgan), (S_EAR_6, COrgan)
               , (S_ROBOT_BRAIN, COrgan)
               , (S_CURRENCY, CStash)
               , (S_CURRENCY, CStash)
               , (S_CURRENCY, CStash)
               , (WASTE_CONTAINER, CEqp), (SPOTLIGHT, CEqp)
               , (CONSTRUCTION_HOOTER, CEqp) ]
  }
