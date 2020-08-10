-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2020 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Definitions of items embedded in map tiles.
module Content.ItemKindEmbed
  ( -- * Group name patterns
    pattern SCRATCH_ON_WALL, pattern OBSCENE_PICTOGRAM, pattern SUBTLE_FRESCO, pattern SIGNAGE, pattern SMALL_FIRE, pattern SMALL_FIRE_5, pattern BIG_FIRE, pattern FROST, pattern RUBBLE, pattern DOORWAY_TRAP_UNKNOWN, pattern DOORWAY_TRAP, pattern STAIRS_UP, pattern STAIRS_DOWN, pattern ESCAPE, pattern STAIRS_TRAP_UP, pattern STAIRS_TRAP_DOWN, pattern LECTERN, pattern SHALLOW_WATER, pattern STRAIGHT_PATH, pattern FROZEN_GROUND
  , pattern S_SANDSTONE_ROCK
  , pattern STAIRS_UP_OUTDOOR, pattern STAIRS_DOWN_OUTDOOR, pattern ABANDONED_CACHE, pattern JEWELRY_DISPLAY_TRAP, pattern BLACK_STARRY_SKY, pattern DISENGAGED_DOCKING_GEAR, pattern RUINED_FIRST_AID_KIT, pattern FIRE_FIGHTING_GEAR, pattern DISPLAY_3D, pattern CRACKED_FLUE, pattern BLOOD_ON_WALL, pattern DEPOSIT_BOX, pattern JEWELRY_CASE, pattern EDIBLE_PLANT_RIPE, pattern STAIRS_TRAP_DOWN_OIL, pattern DOOR_TRAP_PUSH, pattern LIFT_UP, pattern LIFT_DOWN, pattern LIFT_TRAP, pattern SHUTTLE_HARDWARE, pattern OIL_PUDDLE, pattern DECONTAMINATION_CHAMBER, pattern BARREL_CONTENTS, pattern WORKSHOP_BENCH
  , pattern MUSEAL, pattern EDIBLE_PLANT, pattern FIRE_FIGHTING_ITEM, pattern STEEL_SCRAP, pattern HANDLE, pattern HANDLE_AND_STEEL, pattern POLE_AND_STEEL, pattern SPACESUIT_PART, pattern THICK_CLOTH, pattern PERFUME, pattern STARTING_HAMMER, pattern CLOTH_RAG
  , pattern S_ENCHANCED_BERRY, pattern S_COOKED_BERRY, pattern S_FRAYED_FUNGUS, pattern S_COOKED_FUNGUS, pattern S_THIC_LEAF, pattern S_COOKED_LEAF, pattern S_RECONFIGURED_FRUIT, pattern S_COOKED_FRUIT, pattern S_FRAGRANT_HERB, pattern S_COOKED_HERB, pattern S_DULL_FLOWER, pattern S_COOKED_FLOWER, pattern S_SPICY_BARK, pattern S_COOKED_BARK, pattern S_PUMPKIN, pattern S_COOKED_PUMPKIN, pattern S_REFRIGERATION_COIL, pattern S_DOUSED_WOODEN_TORCH, pattern S_DOUSED_OIL_LAMP, pattern S_OIL_LAMP, pattern S_ROSE_WATER_FLASK, pattern S_WATER_FLASK, pattern S_SPACESUIT_JACKET, pattern S_SPACESUIT_TROUSERS, pattern S_SPACESUIT_GLOVE, pattern S_SPACESUIT_HELMET, pattern S_SPACESUIT_BOOT, pattern S_SPACESUIT, pattern S_SPACESUIT_TORN, pattern S_HARPOON_CARGO, pattern S_HARPOON_SHARP, pattern S_SHIELD_BLUNT, pattern S_SHIELD_SHARP, pattern S_SHORT_BLUNT_HAMMER, pattern S_LONG_BLUNT_HAMMER, pattern S_SHORT_SHARP_HAMMER, pattern S_LONG_SHARP_HAMMER, pattern S_CLEAVER, pattern S_DAGGER, pattern S_RAPIER_BLUNT, pattern S_RAPIER_SHARP, pattern S_POLE_CLEAVER, pattern S_LONG_SPEAR, pattern S_SHORT_CLUB, pattern S_LONG_CLUB, pattern S_CROWBAR, pattern S_FIRE_AXE, pattern S_POLL_AXE, pattern S_HALBERD_BLUNT, pattern S_HALBERD_SHARP, pattern S_STAFF, pattern S_PIPE, pattern S_SHARPENED_PIPE
  , embedsGNSingleton, embedsGN
  , -- * Content
    embeds
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

embedsGNSingleton :: [GroupName ItemKind]
embedsGNSingleton =
     S_SANDSTONE_ROCK
     : [S_ENCHANCED_BERRY, S_COOKED_BERRY, S_FRAYED_FUNGUS, S_COOKED_FUNGUS, S_THIC_LEAF, S_COOKED_LEAF, S_RECONFIGURED_FRUIT, S_COOKED_FRUIT, S_FRAGRANT_HERB, S_COOKED_HERB, S_DULL_FLOWER, S_COOKED_FLOWER, S_SPICY_BARK, S_COOKED_BARK, S_PUMPKIN, S_COOKED_PUMPKIN, S_REFRIGERATION_COIL, S_DOUSED_WOODEN_TORCH, S_DOUSED_OIL_LAMP, S_OIL_LAMP, S_ROSE_WATER_FLASK, S_WATER_FLASK, S_SPACESUIT_JACKET, S_SPACESUIT_TROUSERS, S_SPACESUIT_GLOVE, S_SPACESUIT_HELMET, S_SPACESUIT_BOOT, S_SPACESUIT, S_SPACESUIT_TORN, S_HARPOON_CARGO, S_HARPOON_SHARP, S_SHIELD_BLUNT, S_SHIELD_SHARP, S_SHORT_BLUNT_HAMMER, S_LONG_BLUNT_HAMMER, S_SHORT_SHARP_HAMMER, S_LONG_SHARP_HAMMER, S_CLEAVER, S_DAGGER, S_RAPIER_BLUNT, S_RAPIER_SHARP, S_POLE_CLEAVER, S_LONG_SPEAR, S_SHORT_CLUB, S_LONG_CLUB, S_CROWBAR, S_FIRE_AXE, S_POLL_AXE, S_HALBERD_BLUNT, S_HALBERD_SHARP, S_STAFF, S_PIPE, S_SHARPENED_PIPE]

pattern S_SANDSTONE_ROCK :: GroupName ItemKind

pattern S_ENCHANCED_BERRY, S_COOKED_BERRY, S_FRAYED_FUNGUS, S_COOKED_FUNGUS, S_THIC_LEAF, S_COOKED_LEAF, S_RECONFIGURED_FRUIT, S_COOKED_FRUIT, S_FRAGRANT_HERB, S_COOKED_HERB, S_DULL_FLOWER, S_COOKED_FLOWER, S_SPICY_BARK, S_COOKED_BARK, S_PUMPKIN, S_COOKED_PUMPKIN, S_REFRIGERATION_COIL, S_DOUSED_WOODEN_TORCH, S_DOUSED_OIL_LAMP, S_OIL_LAMP, S_ROSE_WATER_FLASK, S_WATER_FLASK, S_SPACESUIT_JACKET, S_SPACESUIT_TROUSERS, S_SPACESUIT_GLOVE, S_SPACESUIT_HELMET, S_SPACESUIT_BOOT, S_SPACESUIT, S_SPACESUIT_TORN, S_HARPOON_CARGO, S_HARPOON_SHARP, S_SHIELD_BLUNT, S_SHIELD_SHARP, S_SHORT_BLUNT_HAMMER, S_LONG_BLUNT_HAMMER, S_SHORT_SHARP_HAMMER, S_LONG_SHARP_HAMMER, S_CLEAVER, S_DAGGER, S_RAPIER_BLUNT, S_RAPIER_SHARP, S_POLE_CLEAVER, S_LONG_SPEAR, S_SHORT_CLUB, S_LONG_CLUB, S_CROWBAR, S_FIRE_AXE, S_POLL_AXE, S_HALBERD_BLUNT, S_HALBERD_SHARP, S_STAFF, S_PIPE, S_SHARPENED_PIPE :: GroupName ItemKind

embedsGN :: [GroupName ItemKind]
embedsGN =
       [SCRATCH_ON_WALL, OBSCENE_PICTOGRAM, SUBTLE_FRESCO, SIGNAGE, SMALL_FIRE, SMALL_FIRE_5, BIG_FIRE, FROST, RUBBLE, DOORWAY_TRAP_UNKNOWN, DOORWAY_TRAP, STAIRS_UP, STAIRS_DOWN, ESCAPE, STAIRS_TRAP_UP, STAIRS_TRAP_DOWN, LECTERN, SHALLOW_WATER, STRAIGHT_PATH, FROZEN_GROUND]
    ++ [STAIRS_UP_OUTDOOR, STAIRS_DOWN_OUTDOOR, ABANDONED_CACHE, JEWELRY_DISPLAY_TRAP, BLACK_STARRY_SKY, DISENGAGED_DOCKING_GEAR, RUINED_FIRST_AID_KIT, FIRE_FIGHTING_GEAR, DISPLAY_3D, CRACKED_FLUE, BLOOD_ON_WALL, DEPOSIT_BOX, JEWELRY_CASE, EDIBLE_PLANT_RIPE, STAIRS_TRAP_DOWN_OIL, DOOR_TRAP_PUSH, LIFT_UP, LIFT_DOWN, LIFT_TRAP, SHUTTLE_HARDWARE, OIL_PUDDLE, DECONTAMINATION_CHAMBER, BARREL_CONTENTS, WORKSHOP_BENCH]
    ++ [MUSEAL, EDIBLE_PLANT, FIRE_FIGHTING_ITEM, STEEL_SCRAP, HANDLE, HANDLE_AND_STEEL, POLE_AND_STEEL, SPACESUIT_PART, THICK_CLOTH, PERFUME, STARTING_HAMMER, CLOTH_RAG]

pattern SCRATCH_ON_WALL, OBSCENE_PICTOGRAM, SUBTLE_FRESCO, SIGNAGE, SMALL_FIRE, SMALL_FIRE_5, BIG_FIRE, FROST, RUBBLE, DOORWAY_TRAP_UNKNOWN, DOORWAY_TRAP, STAIRS_UP, STAIRS_DOWN, ESCAPE, STAIRS_TRAP_UP, STAIRS_TRAP_DOWN, LECTERN, SHALLOW_WATER, STRAIGHT_PATH, FROZEN_GROUND :: GroupName ItemKind

pattern STAIRS_UP_OUTDOOR, STAIRS_DOWN_OUTDOOR, ABANDONED_CACHE, JEWELRY_DISPLAY_TRAP, BLACK_STARRY_SKY, DISENGAGED_DOCKING_GEAR, RUINED_FIRST_AID_KIT, FIRE_FIGHTING_GEAR, DISPLAY_3D, CRACKED_FLUE, BLOOD_ON_WALL, DEPOSIT_BOX, JEWELRY_CASE, EDIBLE_PLANT_RIPE, STAIRS_TRAP_DOWN_OIL, DOOR_TRAP_PUSH, LIFT_UP, LIFT_DOWN, LIFT_TRAP, SHUTTLE_HARDWARE, OIL_PUDDLE, DECONTAMINATION_CHAMBER, BARREL_CONTENTS, WORKSHOP_BENCH :: GroupName ItemKind

pattern MUSEAL, EDIBLE_PLANT, FIRE_FIGHTING_ITEM, STEEL_SCRAP, HANDLE, HANDLE_AND_STEEL, POLE_AND_STEEL, SPACESUIT_PART, THICK_CLOTH, PERFUME, STARTING_HAMMER, CLOTH_RAG :: GroupName ItemKind

pattern SCRATCH_ON_WALL = GroupName "scratch on wall"
pattern OBSCENE_PICTOGRAM = GroupName "obscene pictogram"
pattern SUBTLE_FRESCO = GroupName "subtle fresco"
pattern SIGNAGE = GroupName "signage"
pattern SMALL_FIRE = GroupName "small fire"
pattern SMALL_FIRE_5 = GroupName "small fire embers"
pattern BIG_FIRE = GroupName "big fire"
pattern FROST = GroupName "frozen mass"
pattern RUBBLE = GroupName "rubble"
pattern DOORWAY_TRAP_UNKNOWN = GroupName "doorway trap unknown"
pattern DOORWAY_TRAP = GroupName "doorway trap"
pattern STAIRS_UP = GroupName "stairs up"
pattern STAIRS_DOWN = GroupName "stairs down"
pattern ESCAPE = GroupName "escape"
pattern STAIRS_TRAP_UP = GroupName "stairs trap up"
pattern STAIRS_TRAP_DOWN = GroupName "stairs trap down"
pattern LECTERN = GroupName "lectern"
pattern SHALLOW_WATER = GroupName "shallow water"
pattern STRAIGHT_PATH = GroupName "straight path"
pattern FROZEN_GROUND = GroupName "frozen ground"

pattern S_SANDSTONE_ROCK = GroupName "sandstone rock"

-- ** Allure-specific
pattern STAIRS_UP_OUTDOOR = GroupName "stairs outdoor up"
pattern STAIRS_DOWN_OUTDOOR = GroupName "stairs outdoor down"
pattern ABANDONED_CACHE = GroupName "abandoned cache"
pattern JEWELRY_DISPLAY_TRAP = GroupName "jewelry display trap"
pattern BLACK_STARRY_SKY = GroupName "black starry sky"
pattern DISENGAGED_DOCKING_GEAR = GroupName "disengaged docking gear"
pattern RUINED_FIRST_AID_KIT = GroupName "ruined first aid kit"
pattern FIRE_FIGHTING_GEAR = GroupName "fire fighting gear"
pattern DISPLAY_3D = GroupName "3D display"
pattern CRACKED_FLUE = GroupName "cracked flue"
pattern BLOOD_ON_WALL = GroupName "blood on wall"
pattern DEPOSIT_BOX = GroupName "deposit box"
pattern JEWELRY_CASE = GroupName "reinforced glass case"
pattern EDIBLE_PLANT_RIPE = GroupName "edible vegetation"
pattern STAIRS_TRAP_DOWN_OIL = GroupName "oil staircase trap"
pattern DOOR_TRAP_PUSH = GroupName "weak door frame"
pattern LIFT_UP = GroupName "lift up"
pattern LIFT_DOWN = GroupName "lift down"
pattern LIFT_TRAP = GroupName "lift trap"
pattern SHUTTLE_HARDWARE = GroupName "shuttle hardware"
pattern OIL_PUDDLE = GroupName "oil puddle"
pattern DECONTAMINATION_CHAMBER = GroupName "decontamination chamber"
pattern BARREL_CONTENTS = GroupName "barrel contents"
pattern WORKSHOP_BENCH = GroupName "workshop bench"

pattern MUSEAL = GroupName "museal item"
pattern EDIBLE_PLANT = GroupName "edible plant"
pattern FIRE_FIGHTING_ITEM = GroupName "fire fighting item"
pattern STEEL_SCRAP = GroupName "steel scrap"
pattern HANDLE = GroupName "handle"
pattern HANDLE_AND_STEEL = GroupName "steel on a handle"
pattern POLE_AND_STEEL = GroupName "steel on a pole"
pattern SPACESUIT_PART = GroupName "spacesuit part"
pattern THICK_CLOTH = GroupName "thick cloth"
pattern PERFUME = GroupName "perfume"
pattern STARTING_HAMMER = GroupName "starting hammer"
pattern CLOTH_RAG = GroupName "cloth rag"

pattern S_ENCHANCED_BERRY = GroupName "enhanced berry"
pattern S_COOKED_BERRY = GroupName "cooked berry"
pattern S_FRAYED_FUNGUS = GroupName "frayed fungus"
pattern S_COOKED_FUNGUS = GroupName "cooked fungus"
pattern S_THIC_LEAF = GroupName "thick leaf"
pattern S_COOKED_LEAF = GroupName "cooked leaf"
pattern S_RECONFIGURED_FRUIT = GroupName "reconfigured fruit"
pattern S_COOKED_FRUIT = GroupName "cooked fruit"
pattern S_FRAGRANT_HERB = GroupName "fragrant herb"
pattern S_COOKED_HERB = GroupName "cooked herb"
pattern S_DULL_FLOWER = GroupName "dull flower"
pattern S_COOKED_FLOWER = GroupName "cooked flower"
pattern S_SPICY_BARK = GroupName "spicy bark"
pattern S_COOKED_BARK = GroupName "cooked bark"
pattern S_PUMPKIN = GroupName "pumpkin"
pattern S_COOKED_PUMPKIN = GroupName "cooked pumpkin"

pattern S_DOUSED_WOODEN_TORCH = GroupName "doused wooden torch"
pattern S_DOUSED_OIL_LAMP = GroupName "doused oil lamp"
pattern S_OIL_LAMP = GroupName "oil lamp"
pattern S_REFRIGERATION_COIL = GroupName "refrigeration coil"
pattern S_ROSE_WATER_FLASK = GroupName "rose water flask"
pattern S_WATER_FLASK = GroupName "water flask"
pattern S_SPACESUIT_JACKET = GroupName "spacesuit jacket"
pattern S_SPACESUIT_TROUSERS = GroupName "pair of space trousers"
pattern S_SPACESUIT_GLOVE = GroupName "spacesuit glove"
pattern S_SPACESUIT_HELMET = GroupName "spacesuit helmet"
pattern S_SPACESUIT_BOOT = GroupName "spacesuit boot"
pattern S_SPACESUIT = GroupName "spacesuit"
pattern S_SPACESUIT_TORN = GroupName "torn spacesuit"
pattern S_HARPOON_CARGO = GroupName "blunt harpoon"
pattern S_HARPOON_SHARP = GroupName "sharp harpoon"
pattern S_SHIELD_BLUNT = GroupName "blunted shield"
pattern S_SHIELD_SHARP = GroupName "spiked shield"
pattern S_SHORT_BLUNT_HAMMER = GroupName "sledgehammer"
pattern S_LONG_BLUNT_HAMMER = GroupName "maul"
pattern S_SHORT_SHARP_HAMMER = GroupName "sharp sledgehammer"
pattern S_LONG_SHARP_HAMMER = GroupName "sharp maul"
pattern S_CLEAVER = GroupName "cleaver"
pattern S_DAGGER = GroupName "dagger"
pattern S_RAPIER_BLUNT = GroupName "blunt rapier"
pattern S_RAPIER_SHARP = GroupName "sharp rapier"
pattern S_POLE_CLEAVER = GroupName "pole cleaver"
pattern S_LONG_SPEAR = GroupName "long spear"
pattern S_SHORT_CLUB = GroupName "short club"
pattern S_LONG_CLUB = GroupName "long club"
pattern S_CROWBAR = GroupName "crowbar"
pattern S_FIRE_AXE = GroupName "fire axe"
pattern S_POLL_AXE = GroupName "poll axe"
pattern S_HALBERD_BLUNT = GroupName "blunt halberd"
pattern S_HALBERD_SHARP = GroupName "sharp halberd"
pattern S_STAFF = GroupName "staff"
pattern S_PIPE = GroupName "pipe"
pattern S_SHARPENED_PIPE = GroupName "sharpened pipe"

-- * Content

embeds :: [ItemKind]
embeds =
  [scratchOnWall, obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signageExit, signageEmbed, signageMerchandise, fireSmall, fireSmall5, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, stairsTrapUp, stairsTrapDown, lectern, shallowWater, straightPath, frozenGround]
  -- Allure-specific
  ++ [stairsUpOutdoor, stairsDownOutdoor, blackStarrySky, disengagedDocking, desertedAirlock, ruinedFirstAidKit, fireFightingGear, fireFightingGearIntact, wall3dBillboard, crackedFlue, bloodOnWall, bloodOnWall2, bloodOnWall3, depositBox, depositBoxSummonHero, depositBoxSummonMonster, jewelryCase, ediblePlantRipe, stairsTrapDownOil, doorTrapPush, liftUp, liftDown, liftTrap, liftTrap2, liftTrap3, shuttleHardware, machineOil, crudeWeld, decontaminator, barrelFuel, barrelFertilizer, barrelOxidizer, barrelOil, barrelNitrogen, workshopBench, signageExitLuggage, signageEmbedLuggage, signageMerchandiseLuggage]

scratchOnWall,    obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signageExit, signageEmbed, signageMerchandise, fireSmall, fireSmall5, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, stairsTrapUp, stairsTrapDown, lectern, shallowWater, straightPath, frozenGround :: ItemKind
-- Allure-specific
stairsUpOutdoor,       stairsDownOutdoor, blackStarrySky, disengagedDocking, desertedAirlock, ruinedFirstAidKit, fireFightingGear, fireFightingGearIntact, wall3dBillboard, crackedFlue, bloodOnWall, bloodOnWall2, bloodOnWall3, depositBox, depositBoxSummonHero, depositBoxSummonMonster, jewelryCase, ediblePlantRipe, stairsTrapDownOil, doorTrapPush, liftUp, liftDown, liftTrap, liftTrap2, liftTrap3, shuttleHardware, machineOil, crudeWeld, decontaminator, barrelFuel, barrelFertilizer, barrelOxidizer, barrelOil, barrelNitrogen, workshopBench, signageExitLuggage, signageEmbedLuggage, signageMerchandiseLuggage :: ItemKind

-- Make sure very few walls are substantially useful, e.g., caches,
-- and none that are secret. Otherwise the player will spend a lot of time
-- bumping walls, which is boring compared to fights or dialogues
-- and ever worse, the player will bump all secret walls, wasting time
-- and foregoing the fun of guessing how to find entrance to a disjoint part
-- of the level by bumping the least number of secret walls.
scratchOnWall = ItemKind
  { isymbol  = '?'
  , iname    = "claw mark"
  , ifreq    = [(SCRATCH_ON_WALL, 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "scratch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ VerbMsg "start making sense of the scratches"
               , Detect DetectHidden 3 ]
  , idesc    = "A seemingly random series of scratches, carved deep into the wall."
  , ikit     = []
  }
obscenePictogram = ItemKind
  { isymbol  = '*'
  , iname    = "repulsing graffiti"
  , ifreq    = [(OBSCENE_PICTOGRAM, 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "infuriate"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 7, SetFlag Durable]
  , ieffects = [ VerbMsg "enter inexplicable rage at a glimpse of the inscrutable graffiti"
               , RefillCalm (-20)
               , OneOf [ toOrganGood S_STRENGTHENED (3 + 1 `d` 2)
                       , CreateItem Nothing CGround S_SANDSTONE_ROCK timerNone ]
               ]
  , idesc    = ""  -- alien writing? or runaway robot AI?
  , ikit     = []
  }
subtleFresco = ItemKind
  { isymbol  = '*'
  , iname    = "subtle mural"
  , ifreq    = [(SUBTLE_FRESCO, 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 10, SetFlag Durable]
  , ieffects = [ VerbMsg "feel refreshed by the subtle fresco"
               , toOrganGood S_FAR_SIGHTED (5 + 1 `d` 2) ]
  , idesc    = "Expensive yet tasteful."
  , ikit     = []
  }
treasureCache = ItemKind
  { isymbol  = 'o'
  , iname    = "set"
  , ifreq    = [(ABANDONED_CACHE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [ELabel "of odds and ends", SetFlag Durable]
  , ieffects = [CreateItem Nothing CGround COMMON_ITEM timerNone]
  , idesc    = "If this stash is hidden, it's in plain sight. Or, more probably, it's just tucked aside so that it doesn't get in the way. Whomever worked there, apparently failed to return and retrieve his belongings."
  , ikit     = []
  }
reliefMsg :: Effect
reliefMsg = VerbMsg "sigh with relief when nothing explodes in your face"
treasureCacheTrap = ItemKind
  { isymbol  = '^'
  , iname    = "anti-theft protection"
  , ifreq    = [(JEWELRY_DISPLAY_TRAP, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "taint"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [OneOf [ toOrganBad S_BLIND (10 + 1 `d` 10)
                      , RefillCalm (-99)
                      , Explode S_FOCUSED_CONCUSSION
                      , reliefMsg, reliefMsg ]]
  , idesc    = "A display of such kingly trinkets warrants an autonomous guarding device. The precaution is particularly understandable if some of the merchandise is capable of instantly frying video monitoring equipment across the hall."
  , ikit     = []
  }
signageExit = ItemKind
  { isymbol  = '?'
  , iname    = "sticker"
  , ifreq    = [(SIGNAGE, 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 0), (2, 1)]
  , iverbHit = "whack"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [Detect DetectExit 100]  -- low tech, hence fully operational
  , idesc    = "Mandatory emergency exit information in low-tech form."
                 -- This is a rare tile so use it to convey some more backstory.
  , ikit     = []
  }
signageEmbed = signageExit
  { iname    = "notice"
  , ifreq    = [(SIGNAGE, 100)]
  , iflavour = zipPlain [BrBlue]
  , ieffects = [Detect DetectEmbed 12]  -- low tech, hence fully operational
  , idesc    = "Detailed schematics for the maintenance crew."
                 -- This is a rare tile so use it to convey some more backstory.
  }
signageMerchandise = signageExit
  { iname    = "shop list"
  , ifreq    = [(SIGNAGE, 100)]
  , iflavour = zipPlain [BrGreen]
  , ieffects = [Detect DetectLoot 20]  -- high tech, so slightly confused
  , idesc    = "A list of nearby commercial outlets, constantly updated by tracking merchandise not registered as passenger property. Customers are kindly requeted to refrain from littering in this heavily monitored public area."
  }
fireSmall = ItemKind
  { isymbol  = 'o'
  , iname    = "tiny fire"
  , ifreq    = [(SMALL_FIRE, 1), (FIRE_SOURCE, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [ELabel "of roasting", SetFlag Durable]
  , ieffects = [ Burn 1, Explode S_SINGLE_SPARK
               , OnCombine roastEffect ]
  , idesc    = "A few embers and wisps of flame, glowing brightly."
  , ikit     = []
  }
fireSmall5 = fireSmall
  { iname    = "small fire"  -- whenever a lot of mass to burn, e.g., bush, oil
  , ifreq    = [(SMALL_FIRE_5, 1), (FIRE_SOURCE, 1)]
  , ieffects = [ Burn 1, Explode S_SINGLE_SPARK
               , OnCombine roastEffect5 ]
  }
fireBig = fireSmall
  { isymbol  = '0'
  , iname    = "big fire"
  , ifreq    = [(BIG_FIRE, 1), (FIRE_SOURCE, 1)]
  , iaspects = [ELabel "of immolation", SetFlag Durable]
  , ieffects = [ Burn 2
               , CreateItem Nothing CGround S_WOODEN_TORCH timerNone
               , Explode S_SPARK ]
  , idesc    = "Glowing with light and warmth."
  , ikit     = []
  }
frost = ItemKind
  { isymbol  = '^'
  , iname    = "frozen mass"
  , ifreq    = [(FROST, 1), (COLD_SOURCE, 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ Burn 1  -- sensory ambiguity between hot and cold
               , RefillCalm 20  -- cold reason
               , PushActor (ThrowMod 400 10 1) ]  -- slippery ice
  , idesc    = "Intricate patterns of shining ice. Too voluminous to be thawed, but fragile enough to be shattered."
  , ikit     = []
  }
rubble = ItemKind
  { isymbol  = '&'
  , iname    = "rubble"
  , ifreq    = [(RUBBLE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bury"
  , iweight  = 100000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [OneOf [ Explode S_FOCUSED_GLASS_HAIL
                      , Summon MOBILE_ANIMAL $ 1 `dL` 2
                      , toOrganNoTimer S_POISONED
                      , CreateItem Nothing CGround ANY_ARROW timerNone
                      , CreateItem Nothing CGround STARTING_WEAPON timerNone
                      , reliefMsg, reliefMsg, reliefMsg
                      , reliefMsg, reliefMsg, reliefMsg ]]
  , idesc    = "Broken chunks of foam concrete, glass and torn and burned equipment."
  , ikit     = []
  }
doorwayTrapTemplate = ItemKind
  { isymbol  = '+'
  , iname    = "doorway trap"
  , ifreq    = [(DOORWAY_TRAP_UNKNOWN, 1), (DOORWAY_TRAP, 0)]
      -- the void group needed to pick the item for tile triggering
      -- even when not yet identified
  , iflavour = zipPlain brightCol
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "cripple"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [PresentAs DOORWAY_TRAP_UNKNOWN]
      -- not Durable, springs at most once
  , ieffects = []
  , idesc    = "Just turn the handle..."
  , ikit     = []
  }
doorwayTrap1 = doorwayTrapTemplate
  { ifreq    = [(DOORWAY_TRAP, 50)]
  , ieffects = [toOrganBad S_BLIND $ (1 `dL` 4) * 5]
  -- , idesc    = ""  -- TODO: once physical mechanism decided, also add ways
                      -- to disarm it, using up some items for that
  }
doorwayTrap2 = doorwayTrapTemplate
  { ifreq    = [(DOORWAY_TRAP, 25)]
  , ieffects = [toOrganBad S_SLOWED $ (1 `dL` 4) * 10]
  -- , idesc    = ""
  }
doorwayTrap3 = doorwayTrapTemplate
  { ifreq    = [(DOORWAY_TRAP, 25)]
  , ieffects = [toOrganBad S_WEAKENED $ (1 `dL` 4) * 10 ]
  -- , idesc    = ""
  }
stairsUp = ItemKind
  { isymbol  = '<'
  , iname    = "flight"
  , ifreq    = [(STAIRS_UP, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"  -- the verb is only used when the item hits,
                        -- not when it's applied otherwise, e.g., from tile
  , iweight  = 100000
  , idamage  = 0
  , iaspects = [ELabel "of steps", SetFlag Durable]
  , ieffects = [Ascend True]
  , idesc    = "Stairs that rise towards the spaceship core. It takes slightly more effort to climb than descend, but in lowered gravity it's almost instantaneous in either case."
  , ikit     = []
  }
stairsDown = stairsUp
  { isymbol  = '>'
  , ifreq    = [(STAIRS_DOWN, 1)]
  , ieffects = [Ascend False]
  , idesc    = "Stairs that descend towards the outer ring. Narrow enough that only one person can comfortably use them at a time, but short enough that the whole team may climb down in quick succession."
  }
escape = stairsUp
  { isymbol  = 'E'
  , iname    = "way"
  , ifreq    = [(ESCAPE, 1)]
  , iflavour = zipPlain [BrGreen]
  , iaspects = [SetFlag Durable]
  , ieffects = [Escape]
  , idesc    = "May this nightmare have an end?"
                 -- generic escape, so the text should be too;
                 -- for moon outdoors, spaceship, everywhere
  }
stairsTrapUp = ItemKind
  { isymbol  = '^'
  , iname    = "staircase trap"
  , ifreq    = [(STAIRS_TRAP_UP, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "buffet"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ VerbMsgFail "be caught in decompression blast"
               , Teleport $ 3 + 1 `dL` 10 ]
  , idesc    = ""
  , ikit     = []
  }
-- Needs to be separate from stairsTrapUp, to make sure the item is
-- registered after up stairs (not only after down stairs)
-- so that effects are invoked in the proper order and, e.g., teleport works.
stairsTrapDown = stairsTrapUp
  { ifreq    = [(STAIRS_TRAP_DOWN, 1)]
  , iflavour = zipPlain [Blue]
  , iverbHit = "open up under"
  , ieffects = [ VerbMsgFail "fall down the stairwell"
               , toOrganGood S_DRUNK (20 + 1 `d` 5) ]
  , idesc    = "A treacherous slab, to teach those who are too proud."
  }
lectern = ItemKind
  { isymbol  = '?'
  , iname    = "VR harness"
  , ifreq    = [(LECTERN, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "immerse"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ OneOf [ CreateItem Nothing CGround ANY_SCROLL timerNone
                       , Detect DetectAll 20
                       , Paralyze $ (1 `dL` 6) * 10
                       , toOrganGood S_DRUNK (20 + 1 `d` 5) ]
               , Explode STORY_TELLING ]
  , idesc    = ""
  , ikit     = []
  }
shallowWater = ItemKind
  { isymbol  = '~'
  , iname    = "shallow water"
  , ifreq    = [(SHALLOW_WATER, 1)]  -- may be too shallow to be source
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "impede"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ParalyzeInWater 2, OnCombine waterEffect]
  , idesc    = "Slows down movement. Essential when sharpening weapons."
  , ikit     = []
  }
straightPath = ItemKind
  { isymbol  = '.'
  , iname    = "straight path"
  , ifreq    = [(STRAIGHT_PATH, 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "propel"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [InsertMove 2]
  , idesc    = ""
  , ikit     = []
  }
frozenGround = ItemKind
  { isymbol  = '.'
  , iname    = "shade"
  , ifreq    = [(FROZEN_GROUND, 1)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 10  -- very thick ice and refreezes, but not too large and boring
  , irarity  = [(1, 1)]
  , iverbHit = "betray"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [ELabel "of ice"]
                 -- no Durable or some items would be impossible to pick up
  , ieffects = [PushActor (ThrowMod 400 10 1)]
  , idesc    = ""
  , ikit     = []
  }

-- * Allure-specific

stairsUpOutdoor = stairsUp
  { ifreq    = [(STAIRS_UP_OUTDOOR, 1)]
  , idesc    = "Stairs that rise towards the sky. It takes slightly more effort to climb than descend, but in the low gravity it's almost instantaneous in either case."
  }
stairsDownOutdoor = stairsDown
  { ifreq    = [(STAIRS_DOWN_OUTDOOR, 1)]
  , idesc    = "Stairs that descend towards the underground. Narrow enough that only one person can comfortably use them at a time, but short enough that the whole team may climb down in quick succession."
  }
blackStarrySky = ItemKind
  { isymbol  = ' '
  , iname    = "black starry sky"
  , ifreq    = [(BLACK_STARRY_SKY, 1)]
  , iflavour = zipPlain [Black]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "awe"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ VerbMsg "look into the void and it looks back"
               , RefillCalm (-5) ]
  , idesc    = "Occasionally a planet or the Sun zips by, but is unable to disperse the darkness. The black starscape constantly rotates. The frantic dance is silent, muted, indifferent. There is not even a hint of vibration, just the sense of heaviness and dizziness."  -- appears only on 100% flavour tiles (both floor and walls on some levels), useless and trivial to notice, so the writeup can be longer; who am I kidding, I can't make myself write condensed prose
  , ikit     = []
  }
disengagedDocking = ItemKind
  { isymbol  = '>'
  , iname    = "disengaged docking gear"
  , ifreq    = [(DISENGAGED_DOCKING_GEAR, 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "disappoint"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = []
  , idesc    = "After a short examination it's clear this is not the airlock you arrived through. In fact, this airlock has no space boat attached at all. Many fine small craft were originally docked with such sockets and clamps, but after the spaceship spontaneously deorbited Neptune, a lot of them were seen jettisoned and drifting astern. What a waste. It seems the decks up, closer to the ship's core, have not been purged of shuttles as thoroughly.\nThe airlock still works, but to get out onto the outer hull surface, you'd need a spacesuit with an air tank."
  , ikit     = []
  }
desertedAirlock = ItemKind
  { isymbol  = '>'
  , iname    = "initial entrance"
  , ifreq    = [(DISENGAGED_DOCKING_GEAR, 10000)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1), (2, 0)]
  , iverbHit = "worry"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Unique, SetFlag Durable]
  , ieffects = []
  , idesc    = "You recognize the inner airlock door smeared with guano in a familiar pattern, but you can't see the shuttle you left engaged to the airlock clamps outside. That's a chilling realization: your craft was among those shed by the spaceship. Now you have to find another lifeboat, likely on a deck high up towards the spaceship core, which you were happy to be blocked from, but to which you now have to break through.\nMoreover, something's gone through your supplies, chewing, tearing and scattering most of them. Given that your stay just got prolonged, the shortage of food is almost as alarming as the scarcity of nano first aid vials that could patch the wounds from your recent struggles, until you are able to undergo a proper slow reconstruction in a hospital."
  , ikit     = [ (COOKED_FOOD, CGround), (COOKED_FOOD, CGround)
               , (S_SPACESUIT_TORN, CGround)
               , (S_SHORT_BLUNT_HAMMER, CGround)
               , (S_CROWBAR, CGround) ]
  }
ruinedFirstAidKit = ItemKind
  { isymbol  = '?'
  , iname    = "ruined first aid kit"
  , ifreq    = [(RUINED_FIRST_AID_KIT, 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "prick"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ VerbMsg "inspect a tattered CPR instruction soaked in a residue of oily drugs"
               , OneOf [ toOrganNoTimer S_SLOW_RESISTANT
                       , toOrganNoTimer S_POISON_RESISTANT
                       , toOrganGood S_DRUNK (20 + 1 `d` 5) ]
               , CreateItem Nothing CGround NEEDLE timerNone ]
  , idesc    = ""  -- regulations require; say HP not regenerated in the game; mention how to regain HP
  , ikit     = []
  }
fireFightingGear = ItemKind
  { isymbol  = '?'
  , iname    = "fire fighting gear"
  , ifreq    = [(FIRE_FIGHTING_GEAR, 1), (WATER_SOURCE, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "douse"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []  -- not Durable, springs at most once
  , ieffects = [ VerbMsg "disassemble and sort through the broken and leaking gear, taking away the least decrepit item"
               , CreateItem Nothing CGround FIRE_FIGHTING_ITEM timerNone ]
  , idesc    = "In addition to remains of firefighting tools, it contains a fire hydrant displaying old scars from being used in a hurry."  -- regulations require; hint that terrain can be ignited and doused
  , ikit     = []
  }
fireFightingGearIntact = ItemKind
  { isymbol  = '?'
  , iname    = "fire fighting set"
  , ifreq    = [(FIRE_FIGHTING_GEAR, 1), (WATER_SOURCE, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "douse"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [SetFlag Unique]  -- not Durable, springs at most once
  , ieffects = [ VerbMsg "disassemble and sort through the old gear, taking away the least decrepit items"
               , CreateItem Nothing CGround FIRE_FIGHTING_ITEM timerNone
               , CreateItem Nothing CGround S_FIRE_AXE timerNone ]
  , idesc    = "This cabinet has not been broken open and used, so it contains a complete assortment of fire fighting implements. It also contains a fire hydrant in pristine condition."  -- regulations require; hint that terrain can be ignited and doused
  , ikit     = []
  }
wall3dBillboard = ItemKind
  { isymbol  = '*'
  , iname    = "3D display"
  , ifreq    = [(DISPLAY_3D, 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "push"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 3, SetFlag Durable]
  , ieffects = [ VerbMsg "make it cough up a wobbly standalone hologram once more"
               , OneOf [ Explode ADVERTISEMENT
                       , Explode STORY_TELLING ] ]
  , idesc    = "One can still make out excited moves of bleached shapes."
  , ikit     = []
  }
crackedFlue = ItemKind
  { isymbol  = '|'
  , iname    = "cracked flue"
  , ifreq    = [(CRACKED_FLUE, 1)]  -- TODO: ("methane source", 1)?
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "blow"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 10, SetFlag Durable]
  , ieffects = [ VerbMsg "imagine the fragrance of roasted food wafting through the flue from upstairs"
               , toOrganGood S_KEEN_SMELLING (3 + 1 `d` 2) ]
  , idesc    = "The pipes ring with tumultuous echoes. Whenever you convince yourself it's an uneven updraft singing through the cracks, the noise suddenly stops, then picks up with a roar. Is there a fight over the food on some upper deck or are you just hungry?"
  , ikit     = []
  }
bloodOnWall = ItemKind
  { isymbol  = '.'
  , iname    = "blotch"
  , ifreq    = [(BLOOD_ON_WALL, 60)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1 `d` 6
  , irarity  = [(1, 1)]
  , iverbHit = "stain"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ELabel "of red fluid"]
  , ieffects = [RefillCalm (-5)]
  , idesc    = ""  -- probably enough said
  , ikit     = []
  }
bloodOnWall2 = bloodOnWall
  { ifreq    = [(BLOOD_ON_WALL, 30)]
  , iflavour = zipPlain [BrGreen]
  , iaspects = [ELabel "of green fluid"]
  , ieffects = [RefillCalm (-10)]
  }
bloodOnWall3 = bloodOnWall
  { ifreq    = [(BLOOD_ON_WALL, 10)]
  , iflavour = zipPlain [BrBlue]
  , iaspects = [ELabel "of blue fluid"]
  , ieffects = [RefillCalm (-15)]
  }
depositBox = treasureCache
  { iname    = "intact deposit box"
  , ifreq    = [(DEPOSIT_BOX, 60)]
  , iaspects = [SetFlag Durable]
  , ieffects = [CreateItem Nothing CGround COMMON_ITEM timerNone]
                 -- can't be VALUABLE or template items generated
  , idesc    = "The reports of intact deposit boxes in the ship's safes have been greatly exaggerated, but there are still a few with glittering gems and gold, just waiting to be taken. Whomever looted these halls wasn't thorough or, judging from the damage to some of the boxes, was in an extreme hurry."
  }
depositBoxSummonHero = treasureCache
  { iname    = "noisy large deposit box"
  , ifreq    = [(DEPOSIT_BOX, 30)]
  , iflavour = zipFancy [Blue]
  , iaspects = [SetFlag Unique]
  , ieffects = [Summon HERO 1, CreateItem Nothing CGround MUSEAL timerNone]
  , idesc    = "Judging from the frantic sounds, this deposit cell with a time lock has trapped something big recently."
  }
depositBoxSummonMonster = treasureCache
  { iname    = "throbbing deposit box"
  , ifreq    = [(DEPOSIT_BOX, 10)]
  , iflavour = zipFancy [Red]
  , iaspects = []
  , ieffects = [ Summon MOBILE_MONSTER 1
               , CreateItem Nothing CGround TREASURE timerNone ]
  , idesc    = "Judging from the incessant noises, this deposit cell with a time lock has trapped something recently."
  }
jewelryCase = treasureCache
  { iname    = "reinforced glass case"
  , ifreq    = [(JEWELRY_CASE, 1)]
  , iaspects = [SetFlag Durable]
  , ieffects = [CreateItem Nothing CGround ANY_JEWELRY timerNone]
  , idesc    = "The customers of these shops must have been extremely well off, judging from abundance and quality of the jewelry, often extremely valuable in each of the artistic, material and nanotechnology aspects. Outer Solar System trips are expensive, but they offer unique trade and investment opportunities. Many deals are of the kind that can only be negotiated in a sealed room out of reach of satellites and screened by both parties. Among the jewelry are portable versions of such screening hardware --- in a truly breathtaking package."
  }
ediblePlantRipe = treasureCache
  { iname    = "edible vegetation"
  , ifreq    = [(EDIBLE_PLANT_RIPE, 1)]
  , iflavour = zipPlain [Green]
  , iaspects = [SetFlag Durable]
  , ieffects = [CreateItem Nothing CGround EDIBLE_PLANT timerNone]
  , idesc    = ""
  }
stairsTrapDownOil = stairsTrapUp
  { ifreq    = [(STAIRS_TRAP_DOWN_OIL, 1)]
  , iflavour = zipPlain [Brown]
  , iverbHit = "cause a chaotic skid"
  , ieffects = [ VerbMsgFail "tumble down and shoot out of the stairwell"
               , PushActor (ThrowMod 400 100 1)]  -- 4 steps, 2 turns
  , idesc    = ""
  }
doorTrapPush = stairsTrapUp
  { isymbol  = '+'
  , iname    = "weak door frame"
  , ifreq    = [(DOOR_TRAP_PUSH, 1)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "give in"
  , ieffects = [ VerbMsgFail "fly inwards after the crashed open doors"
               , PushActor (ThrowMod 400 100 1)]  -- 4 steps, 2 turns
  , idesc    = ""
  }
liftUp = stairsUp
  { iname    = "carriage"
  , iflavour = zipPlain [Blue]
  , ifreq    = [(LIFT_UP, 1)]
  , iaspects = [SetFlag Durable]
  , idesc    = ""  -- describe inner levels of the ship
  }
liftDown = stairsDown
  { iname    = "carriage"
  , iflavour = zipPlain [Blue]
  , ifreq    = [(LIFT_DOWN, 1)]
  , iaspects = [SetFlag Durable]
  , idesc    = ""  -- describe outer levels of the ship
  }
liftTrap = stairsTrapUp
  { iname    = "elevator trap"  -- hat tip to US heroes
  , ifreq    = [(LIFT_TRAP, 100)]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "squeeze"
  , ieffects = [ VerbMsgFail "be crushed by the sliding doors"
               , DropItem maxBound 1 CEqp STARTING_WEAPON, Paralyze 10 ]
  , idesc    = ""
  }
liftTrap2 = liftTrap
  { ifreq    = [(LIFT_TRAP, 50)]
  , iflavour = zipFancy [BrMagenta]
  , iverbHit = "choke"
  , ieffects = [ VerbMsgFail "inhale the gas lingering inside the cab"
               , toOrganBad S_SLOWED $ (1 `dL` 4) * 10 ]
  , idesc    = ""
  }
liftTrap3 = liftTrap
  { ifreq    = [(LIFT_TRAP, 50)]
  , iflavour = zipFancy [BrBlue]
  , iverbHit = "shock"
  , ieffects = [ VerbMsgFail "be electrocuted upon touching the control pad"
               , Discharge 5 $ 80 - 1 `d` 40 ]
  , idesc    = ""
  }
shuttleHardware = ItemKind
  { isymbol  = '#'
  , iname    = "shuttle hardware"
  , ifreq    = [(SHUTTLE_HARDWARE, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "resist"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = []
  , idesc    = "While the hull of the spacecraft is intact, the flight hardware that normally lines the walls seems broken, worn out and often missing. This shuttle was probably scavenged for spare parts to repair other craft and it's unlikely that anything of use remains. This was the common \"taxi\" kind, fit only for lunar and orbital courier duties and single family trips. It's relatively cheap to operate, because no permanent airlock needs to be leased. Instead, the craft is brought through a large airlock to a dry-dock and serviced and even stored inside."
  , ikit     = []
  }
machineOil = ItemKind
  { isymbol  = '~'
  , iname    = "oil layer"
  , ifreq    = [(OIL_PUDDLE, 1), (OIL_SOURCE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 5  -- not durable, wears off
  , irarity  = [(1, 1)]
  , iverbHit = "oil"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []
  , ieffects = [PushActor (ThrowMod 600 10 1), OnCombine oilEffect]
                  -- the high speed represents gliding rather than flying
                  -- and so no need to lift actor's weight off the ground;
                  -- low linger comes from abrupt halt over normal surface
  , idesc    = "Slippery run out, probably from a life support equipment or vehicle engine."
  , ikit     = []
  }
crudeWeld = ItemKind  -- this is also an organ
  { isymbol  = '_'
  , iname    = "crude weld"
  , ifreq    = [(S_CRUDE_WELD, 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "weld"
  , iweight  = 3000
  , idamage  = 0
  , iaspects = [AddSkill SkMove (-5), AddSkill SkDisplace (-1), SetFlag Durable]
  , ieffects = [Explode S_SPARK]
  , idesc    = "Such a superfluous mass of molten metal was layered on with messy welds that no amount of kicking nor hammering has any effect. A heavy duty cutting tool would be required or skilled thermal cycling. Whomever did the melding may still be around, but convincing him to yield his fiery implements may be a dangerous endeavour."
  , ikit     = []
  }
decontaminator = ItemKind
  { isymbol  = 'O'
  , iname    = "decontamination chamber"
  , ifreq    = [(DECONTAMINATION_CHAMBER, 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "cleanse"
  , iweight  = 500000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [ DropItem 1 1 COrgan GENETIC_FLAW
               , DropItem maxBound maxBound CEqp COMMON_ITEM
               , DropItem maxBound maxBound CStash COMMON_ITEM
               , DropItem maxBound maxBound CEqp CRAWL_ITEM
               , DropItem maxBound maxBound CStash CRAWL_ITEM
               , DropItem maxBound maxBound CEqp TREASURE
               , DropItem maxBound maxBound CStash TREASURE
                   -- With movable shared stash location this puzzle now has
                   -- more solutions, including one for a lone wolf.
               , toOrganGood S_ROSE_SMELLING (20 + 1 `d` 5)
               ]
  , idesc    = "The area is under quarantine. No departure is permitted without decontamination. Personal belongings are to be decontaminated separately."
  , ikit     = []
  }
barrelFuel = ItemKind
  { isymbol  = 'b'
  , iname    = "fuel"
  , ifreq    = [(BARREL_CONTENTS, 20), (OIL_SOURCE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "block"
  , iweight  = 100000
  , idamage  = 0
  , iaspects = []
  , ieffects = [Explode S_FOCUSED_BURNING_OIL_3, OnCombine oilEffect]
  , idesc    = ""
  , ikit     = []
  }
barrelFertilizer = barrelFuel
  { iname    = "fertilizer"
  , ifreq    = [(BARREL_CONTENTS, 30), (FIRE_SOURCE, 1)]
  , iflavour = zipPlain [Red]
  , ieffects = [Explode S_FOCUSED_FRAGMENTATION, OnCombine roastEffect5]
                 -- no S_FOCUSED_CONCUSSION; a barrel would destroy the ship;
                 -- no water barrel either, basins and running water in taps;
                 -- no VIOLENT variants of the blasts or bumping a lone
                 -- barrel would be safe (as it's safe with fuel barrel now)
  , idesc    = ""
  }
barrelOxidizer = barrelFuel
  { iname    = "oxidizer"
  , ifreq    = [(BARREL_CONTENTS, 20), (FIRE_SOURCE, 1)]
  , iflavour = zipPlain [BrWhite]
  , ieffects = [Explode S_FOCUSED_FLASH, OnCombine roastEffect5]
  , idesc    = ""
  }
barrelOil = barrelFuel
  { iname    = "lubricant oil"
  , ifreq    = [(BARREL_CONTENTS, 20), (OIL_SOURCE, 1)]
  , iflavour = zipPlain [Brown]
  , ieffects = [Explode S_MELEE_PROTECTIVE_BALM, OnCombine oilEffect]
                 -- beneficial, so OK not to affect the triggering actor
  , idesc    = ""
  }
barrelNitrogen = barrelFuel
  { iname    = "liquid nitrogen"
  , ifreq    = [(BARREL_CONTENTS, 40), (COLD_SOURCE, 1)]
  , iflavour = zipPlain [BrBlack]
  , ieffects = [Explode S_FOCUSED_SLOWNESS_MIST]
                 -- may exploit to harm foes; watch out for friends
  , idesc    = ""
  }
workshopBench = ItemKind
  { isymbol  = 'b'
  , iname    = "bench"
  , ifreq    = [(WORKSHOP_BENCH, 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bury"
  , iweight  = 100000
  , idamage  = 0
  , iaspects = [SetFlag Durable]
  , ieffects = [OnCombine workshopEffect]
  , idesc    = "A sturdy table with an anvil, a vice and an overhang of pipes, tubes, wires and probes. Place the components and tools adjacent to the table, stand over them and craft by trying to modify the bench with the 'M' command."
  , ikit     = []
  }
signageExitLuggage = signageExit
  { irarity  = [(1, 1), (2, 0)]
  , idesc    = idesc signageExit
               <+> "If exits blocked, luggage claim area marked with 'L' is the concentration point."
  }
signageEmbedLuggage = signageEmbed
  { irarity  = [(1, 1), (2, 0)]
  , idesc    = idesc signageEmbed
               <+> "Keys and tools are held in the luggage claim booth marked with 'L'."
  }
signageMerchandiseLuggage = signageMerchandise
  { irarity  = [(1, 1), (2, 0)]
  , idesc    = idesc signageMerchandise
               <+> "Queries and complaints can be filed to the port officer and luggage attendant this way under the 'L' sign."
  }

combineEffect :: Text -> [( [(Int, GroupName ItemKind)]
                          , [(Int, GroupName ItemKind)]
                          , [(Int, GroupName ItemKind)] )]
              -> Effect
combineEffect msg ass =
  let createOne :: (Int, GroupName ItemKind) -> Effect
      createOne (count, grp) = CreateItem (Just count) CGround grp timerNone
      createList :: [(Int, GroupName ItemKind)] -> Effect
      createList = SeqEffect . map createOne
      cookOne :: ( [(Int, GroupName ItemKind)]
                 , [(Int, GroupName ItemKind)]
                 , [(Int, GroupName ItemKind)] )
              -> Effect
      cookOne (tools, raw, cooked) =
        ConsumeItems tools raw  -- either all destroyed or none
        `AndEffect`  -- either destroy and create, or none
        createList cooked
      f :: ( [(Int, GroupName ItemKind)]
           , [(Int, GroupName ItemKind)]
           , [(Int, GroupName ItemKind)] )
        -> Effect
        -> Effect
      f roolsRawCooked eff = cookOne roolsRawCooked `OrEffect` eff
      initial = VerbMsgFail msg  -- noop; emits @UseId@ to correctly abort
  in foldr f initial ass

oilEffect :: Effect
oilEffect = combineEffect "have nothing to oil"
              [ ( [], [(1, S_STAFF), (1, THICK_CLOTH)]
                , [(1, S_DOUSED_WOODEN_TORCH)] )
              , ( [], [(1, S_EMPTY_FLASK), (1, THICK_CLOTH)]
                , [(1, S_DOUSED_OIL_LAMP)] ) ]

roastEffect :: Effect
roastEffect = combineEffect "have nothing to roast"
              $ extraRoastAssocs
                ++ map (\(raw, cooked) ->
                          ([], [(1, raw)], [(1, cooked)])) cookingAssocs

roastEffect5 :: Effect
roastEffect5 = combineEffect "have nothing to roast"
               $ extraRoastAssocs
                 ++ concatMap (\(raw, cooked) ->
                                 [ ([], [(5, raw)], [(5, cooked)])
                                 , ([], [(4, raw)], [(4, cooked)])
                                 , ([], [(3, raw)], [(3, cooked)])
                                 , ([], [(2, raw)], [(2, cooked)])
                                 , ([], [(1, raw)], [(1, cooked)])
                                 ])
                              cookingAssocs

waterEffect :: Effect
waterEffect = combineEffect "lack a sharpening tool or a weapon to sharpen or an item to fill with water"
              $ map (\(tools, raw, cooked) ->
                      (tools, [(1, raw)], [(1, cooked)])) sharpeningAssocs
                ++ extraWaterAssocs

workshopEffect :: Effect
workshopEffect = combineEffect "have not enough tools and components"
                               workshopAssocs

cookingAssocs :: [(GroupName ItemKind, GroupName ItemKind)]
cookingAssocs =
  [ (RAW_MEAT_CHUNK, ROASTED_MEAT_CHUNK)
  , (S_ENCHANCED_BERRY, S_COOKED_BERRY)
  , (S_FRAYED_FUNGUS, S_COOKED_FUNGUS)
  , (S_THIC_LEAF, S_COOKED_LEAF)
  , (S_RECONFIGURED_FRUIT, S_COOKED_FRUIT)
  , (S_FRAGRANT_HERB, S_COOKED_HERB)
  , (S_DULL_FLOWER, S_COOKED_FLOWER)
  , (S_SPICY_BARK, S_COOKED_BARK)
  , (S_PUMPKIN, S_COOKED_PUMPKIN)
  ]

extraRoastAssocs :: [( [(Int, GroupName ItemKind)]
                     , [(Int, GroupName ItemKind)]
                     , [(Int, GroupName ItemKind)] )]
extraRoastAssocs =
  [ ([], [(1, S_DOUSED_WOODEN_TORCH)], [(1, S_WOODEN_TORCH)])
  , ([], [(1, S_DOUSED_OIL_LAMP)], [(1, S_OIL_LAMP)])
  , ([], [(1, WASTE_CONTAINER)], [(1, S_REFRIGERATION_COIL)])
  ]

sharpeningAssocs :: [( [(Int, GroupName ItemKind)]
                     , GroupName ItemKind
                     , GroupName ItemKind )]
sharpeningAssocs =
  [ ([(1, SHARPENING_TOOL)], S_HARPOON_CARGO, S_HARPOON_SHARP)
  , ([(1, SHARPENING_TOOL), (1, BREACHING_TOOL)], S_PIPE, S_SHARPENED_PIPE)
  , ([(1, SHARPENING_TOOL)], S_SHIELD_BLUNT, S_SHIELD_SHARP)
  , ([(2, SHARPENING_TOOL)], S_SHORT_BLUNT_HAMMER, S_SHORT_SHARP_HAMMER)
  , ([(2, SHARPENING_TOOL)], S_LONG_BLUNT_HAMMER, S_LONG_SHARP_HAMMER)
  , ([(2, SHARPENING_TOOL)], S_CLEAVER, S_DAGGER)
  , ([(1, SHARPENING_TOOL)], S_RAPIER_BLUNT, S_RAPIER_SHARP)
  , ([(2, SHARPENING_TOOL)], S_POLE_CLEAVER, S_LONG_SPEAR)
  , ([(1, SHARPENING_TOOL)], S_HALBERD_BLUNT, S_HALBERD_SHARP)
  ]

extraWaterAssocs :: [( [(Int, GroupName ItemKind)]
                     , [(Int, GroupName ItemKind)]
                     , [(Int, GroupName ItemKind)] )]
extraWaterAssocs =
  [ ( [(1, PERFUME)], [(5, S_EMPTY_FLASK)], [(5, S_ROSE_WATER_FLASK)] )
  , ( [], [(1, S_EMPTY_FLASK)], [(1, S_WATER_FLASK)] )
  ]

workshopAssocs :: [( [(Int, GroupName ItemKind)]
                   , [(Int, GroupName ItemKind)]
                   , [(Int, GroupName ItemKind)] )]
workshopAssocs =
  [ ( [(1, BONDING_TOOL)], [(1, POLE), (1, S_SHORT_BLUNT_HAMMER)]
    , [(1, S_LONG_BLUNT_HAMMER), (1, HANDLE)] )
  , ( [(1, BONDING_TOOL)], [(1, POLE), (1, S_SHORT_SHARP_HAMMER)]
    , [(1, S_LONG_SHARP_HAMMER), (1, HANDLE)] )
  , ( [(1, BONDING_TOOL)], [(1, POLE), (1, S_FIRE_AXE)]
    , [(1, S_POLL_AXE), (1, HANDLE)] )
  , ( [(2, BONDING_TOOL)], [(1, POLE), (1, S_CLEAVER)]
    , [(1, S_POLE_CLEAVER)] )
  , ( [(2, BONDING_TOOL)], [(1, POLE), (1, S_DAGGER)]
    , [(1, S_LONG_SPEAR)] )
  , ( [], [(1, S_STAFF), (1, STEEL_SCRAP)], [(1, S_SHORT_CLUB)] )
  , ( [], [(1, POLE), (2, STEEL_SCRAP)], [(1, S_LONG_CLUB)] )
  -- Perfect the perfection. Comes after the productive rules.
  , ( [(1, BONDING_TOOL)], [(1, S_LONG_SHARP_HAMMER)]
    , [(1, S_LONG_SHARP_HAMMER)] )
  , ( [(1, BONDING_TOOL)], [(1, S_POLL_AXE)], [(1, S_POLL_AXE)] )
  , ( [(1, BONDING_TOOL)], [(1, S_LONG_SPEAR)], [(1, S_LONG_SPEAR)] )
  , ( [(3, BONDING_TOOL)]
    , [ (1, S_SPACESUIT_JACKET), (1, S_SPACESUIT_TROUSERS)
      , (2, S_SPACESUIT_GLOVE), (1, S_SPACESUIT_HELMET)
      , (2, S_SPACESUIT_BOOT) ]
    , [(1, S_SPACESUIT)] )
  -- Recipes that destroy more useful things than other recipes come last.
  , ( [(1, WIRECUTTING_TOOL)], [(1, S_SPACESUIT_TORN)]
    , [(1, SPACESUIT_PART), (1, SPACESUIT_PART), (1, CLOTH_RAG)] )
  , ( [(1, PERFUME)], [(1, WATER_SOURCE), (5, S_EMPTY_FLASK)]
    , [(6, S_ROSE_WATER_FLASK)] )  -- the extra container is from water source
  , ( [(1, BREACHING_TOOL)], [(1, POLE_AND_STEEL)]  -- harder to dismantle
    , [(1, POLE), (1, STEEL_SCRAP)] )
  , ([], [(1, HANDLE_AND_STEEL)], [(1, HANDLE), (1, STEEL_SCRAP)])
  ]
