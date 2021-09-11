-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor organ definitions.
module Content.ItemKindOrgan
  ( -- * Group name patterns
    pattern S_FIST, pattern S_FOOT, pattern S_HOOKED_CLAW, pattern S_SMALL_CLAW, pattern S_SNOUT, pattern S_SMALL_JAW, pattern S_JAW, pattern S_LARGE_JAW, pattern S_ANTLER, pattern S_HORN, pattern S_RHINO_HORN, pattern S_TENTACLE, pattern S_TIP, pattern S_LIP, pattern S_THORN, pattern S_BOILING_FISSURE, pattern S_BEE_STING, pattern S_STING, pattern S_VENOM_TOOTH, pattern S_VENOM_FANG, pattern S_SCREECHING_BEAK, pattern S_LARGE_TAIL, pattern S_HUGE_TAIL, pattern S_ARMORED_SKIN, pattern S_BARK, pattern S_NOSTRIL, pattern S_RATLLE, pattern S_INSECT_MORTALITY, pattern S_SAPIENT_BRAIN, pattern S_ANIMAL_BRAIN, pattern S_SCENT_GLAND, pattern S_BOILING_VENT, pattern S_EYE_3, pattern S_EYE_6, pattern S_EYE_8, pattern S_VISION_6, pattern S_VISION_12, pattern S_VISION_16, pattern S_EAR_3, pattern S_EAR_6, pattern S_EAR_8, pattern S_SPEED_GLAND_5, pattern S_SPEED_GLAND_10
  , pattern SCAVENGER, pattern ALCOHOL
  , pattern S_ANIMAL_STOMACH, pattern S_HUNGRY, pattern S_RAZOR, pattern S_FLOTATION_BAG, pattern S_INK_SAC, pattern S_POWERFUL_HIND_LEGS, pattern S_COILED_TAIL, pattern S_JET_BOOSTER, pattern S_RHINO_INERTIA, pattern S_SMALL_BEAK, pattern S_LIVE_WIRE, pattern S_COOLING_VENT, pattern S_COOLING_FISSURE, pattern S_MEDBOT_VENT, pattern S_MEDBOT_FISSURE, pattern S_DUST_VENT, pattern S_DUST_FISSURE, pattern S_FUEL_VENT, pattern S_FUEL_FISSURE, pattern S_ROBOT_BRAIN, pattern S_HULL_PLATING, pattern S_MOUTH_VENT, pattern S_CRUDE_WELD
  , pattern ELECTRIC_AMBIENCE, pattern GENETIC_FLAW_3, pattern GENETIC_FLAW_10, pattern GENETIC_FLAW, pattern BACKSTORY, pattern BACKSTORY_FLUFF, pattern BACKSTORY_GOOD, pattern BACKSTORY_BAD, pattern BACKSTORY_MIXED, pattern BACKSTORY_NEUTRAL
  , organsGNSingleton, organsGN
  , -- * Content
    organs
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindBlast
import Content.ItemKindTemporary
import Content.RuleKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

organsGNSingleton :: [GroupName ItemKind]
organsGNSingleton =
       [S_FIST, S_FOOT, S_HOOKED_CLAW, S_SMALL_CLAW, S_SNOUT, S_SMALL_JAW, S_JAW, S_LARGE_JAW, S_ANTLER, S_HORN, S_RHINO_HORN, S_TENTACLE, S_TIP, S_LIP, S_THORN, S_BOILING_FISSURE, S_BEE_STING, S_STING, S_VENOM_TOOTH, S_VENOM_FANG, S_SCREECHING_BEAK, S_LARGE_TAIL, S_HUGE_TAIL, S_ARMORED_SKIN, S_BARK, S_NOSTRIL, S_RATLLE, S_INSECT_MORTALITY, S_SAPIENT_BRAIN, S_ANIMAL_BRAIN, S_SCENT_GLAND, S_BOILING_VENT, S_EYE_3, S_EYE_6, S_EYE_8, S_VISION_6, S_VISION_12, S_VISION_16, S_EAR_3, S_EAR_6, S_EAR_8, S_SPEED_GLAND_5, S_SPEED_GLAND_10]
    ++ [S_ANIMAL_STOMACH, S_HUNGRY, S_RAZOR, S_FLOTATION_BAG, S_INK_SAC, S_POWERFUL_HIND_LEGS, S_COILED_TAIL, S_JET_BOOSTER, S_RHINO_INERTIA, S_SMALL_BEAK, S_LIVE_WIRE, S_COOLING_VENT, S_COOLING_FISSURE, S_MEDBOT_VENT, S_MEDBOT_FISSURE, S_DUST_VENT, S_DUST_FISSURE, S_FUEL_VENT, S_FUEL_FISSURE, S_ROBOT_BRAIN, S_HULL_PLATING, S_MOUTH_VENT, S_CRUDE_WELD, BACKSTORY_FLUFF_UNKNOWN, BACKSTORY_GOOD_UNKNOWN, BACKSTORY_BAD_UNKNOWN, BACKSTORY_MIXED_UNKNOWN, BACKSTORY_NEUTRAL_UNKNOWN]

pattern S_FIST, S_FOOT, S_HOOKED_CLAW, S_SMALL_CLAW, S_SNOUT, S_SMALL_JAW, S_JAW, S_LARGE_JAW, S_ANTLER, S_HORN, S_RHINO_HORN, S_TENTACLE, S_TIP, S_LIP, S_THORN, S_BOILING_FISSURE, S_BEE_STING, S_STING, S_VENOM_TOOTH, S_VENOM_FANG, S_SCREECHING_BEAK, S_LARGE_TAIL, S_HUGE_TAIL, S_ARMORED_SKIN, S_BARK, S_NOSTRIL, S_RATLLE, S_INSECT_MORTALITY, S_SAPIENT_BRAIN, S_ANIMAL_BRAIN, S_SCENT_GLAND, S_BOILING_VENT, S_EYE_3, S_EYE_6, S_EYE_8, S_VISION_6, S_VISION_12, S_VISION_16, S_EAR_3, S_EAR_6, S_EAR_8, S_SPEED_GLAND_5, S_SPEED_GLAND_10 :: GroupName ItemKind

pattern S_ANIMAL_STOMACH, S_HUNGRY, S_RAZOR, S_FLOTATION_BAG, S_INK_SAC, S_POWERFUL_HIND_LEGS, S_COILED_TAIL, S_JET_BOOSTER, S_RHINO_INERTIA, S_SMALL_BEAK, S_LIVE_WIRE, S_COOLING_VENT, S_COOLING_FISSURE, S_MEDBOT_VENT, S_MEDBOT_FISSURE, S_DUST_VENT, S_DUST_FISSURE, S_FUEL_VENT, S_FUEL_FISSURE, S_ROBOT_BRAIN, S_HULL_PLATING, S_MOUTH_VENT, S_CRUDE_WELD, BACKSTORY_FLUFF_UNKNOWN, BACKSTORY_GOOD_UNKNOWN, BACKSTORY_BAD_UNKNOWN, BACKSTORY_MIXED_UNKNOWN, BACKSTORY_NEUTRAL_UNKNOWN :: GroupName ItemKind

organsGN :: [GroupName ItemKind]
organsGN =
     SCAVENGER : ALCOHOL
     : [ELECTRIC_AMBIENCE, GENETIC_FLAW_3, GENETIC_FLAW_10, GENETIC_FLAW, BACKSTORY, BACKSTORY_FLUFF, BACKSTORY_GOOD, BACKSTORY_BAD, BACKSTORY_MIXED, BACKSTORY_NEUTRAL]

pattern SCAVENGER :: GroupName ItemKind
pattern ALCOHOL :: GroupName ItemKind

pattern ELECTRIC_AMBIENCE, GENETIC_FLAW_3, GENETIC_FLAW_10, GENETIC_FLAW, BACKSTORY, BACKSTORY_FLUFF, BACKSTORY_GOOD, BACKSTORY_BAD, BACKSTORY_MIXED, BACKSTORY_NEUTRAL :: GroupName ItemKind

pattern S_FIST = GroupName "fist"
pattern S_FOOT = GroupName "foot"
pattern S_HOOKED_CLAW = GroupName "hooked claw"
pattern S_SMALL_CLAW = GroupName "small claw"
pattern S_SNOUT = GroupName "snout"
pattern S_SMALL_JAW = GroupName "small jaw"
pattern S_JAW = GroupName "jaw"
pattern S_LARGE_JAW = GroupName "large jaw"
pattern S_ANTLER = GroupName "antler"
pattern S_HORN = GroupName "horn"
pattern S_RHINO_HORN = GroupName "rhino horn"
pattern S_TENTACLE = GroupName "tentacle"
pattern S_TIP = GroupName "tip"
pattern S_LIP = GroupName "lip"
pattern S_THORN = GroupName "thorn"
pattern S_BOILING_FISSURE = GroupName "boiling fissure"
pattern S_BEE_STING = GroupName "bee sting"
pattern S_STING = GroupName "sting"
pattern S_VENOM_TOOTH = GroupName "venom tooth"
pattern S_VENOM_FANG = GroupName "venom fang"
pattern S_SCREECHING_BEAK = GroupName "screeching beak"
pattern S_LARGE_TAIL = GroupName "large tail"
pattern S_HUGE_TAIL = GroupName "huge tail"
pattern S_ARMORED_SKIN = GroupName "armored skin"
pattern S_BARK = GroupName "bark"
pattern S_NOSTRIL = GroupName "nostril"
pattern S_RATLLE = GroupName "rattle"
pattern S_INSECT_MORTALITY = GroupName "insect mortality"
pattern S_SAPIENT_BRAIN = GroupName "sapient brain"
pattern S_ANIMAL_BRAIN = GroupName "animal brain"
pattern S_SCENT_GLAND = GroupName "scent gland"
pattern S_BOILING_VENT = GroupName "boiling vent"
pattern S_EYE_3 = GroupName "eye 3"
pattern S_EYE_6 = GroupName "eye 6"
pattern S_EYE_8 = GroupName "eye 8"
pattern S_VISION_6 = GroupName "vision 6"
pattern S_VISION_12 = GroupName "vision 12"
pattern S_VISION_16 = GroupName "vision 16"
pattern S_EAR_3 = GroupName "ear 3"
pattern S_EAR_6 = GroupName "ear 6"
pattern S_EAR_8 = GroupName "ear 8"
pattern S_SPEED_GLAND_5 = GroupName "speed gland 5"
pattern S_SPEED_GLAND_10 = GroupName "speed gland 10"

pattern SCAVENGER = GroupName "scavenger"
pattern ALCOHOL = GroupName "alcohol"

-- ** Allure-specific
pattern S_ANIMAL_STOMACH = GroupName "animal stomach"
pattern S_HUNGRY = GroupName "hungry"
pattern S_RAZOR = GroupName "razor"
pattern S_FLOTATION_BAG = GroupName "flotation bag"
pattern S_INK_SAC = GroupName "inc sac"
pattern S_POWERFUL_HIND_LEGS = GroupName "powerful hind legs"
pattern S_COILED_TAIL = GroupName "coiled tail"
pattern S_JET_BOOSTER = GroupName "jet booster"
pattern S_RHINO_INERTIA = GroupName "rhino inertia"
pattern S_SMALL_BEAK = GroupName "small beak"
pattern S_LIVE_WIRE = GroupName "live wire"
pattern S_COOLING_VENT = GroupName "cooling vent"
pattern S_COOLING_FISSURE = GroupName "cooling fissure"
pattern S_MEDBOT_VENT = GroupName "medbot vent"
pattern S_MEDBOT_FISSURE = GroupName "medbot fissure"
pattern S_DUST_VENT = GroupName "dust vent"
pattern S_DUST_FISSURE = GroupName "dust fissure"
pattern S_FUEL_VENT = GroupName "fuel vent"
pattern S_FUEL_FISSURE = GroupName "fuel fissure"
pattern S_ROBOT_BRAIN = GroupName "robot brain"
pattern S_HULL_PLATING = GroupName "hull plating"
pattern S_MOUTH_VENT = GroupName "mouth vent"
pattern S_CRUDE_WELD = GroupName "crude weld"

pattern ELECTRIC_AMBIENCE = GroupName "electric ambience"
pattern GENETIC_FLAW_3 = GroupName "genetic flaw 3"
pattern GENETIC_FLAW_10 = GroupName "genetic flaw 10"
pattern GENETIC_FLAW = GroupName "genetic flaw"
pattern BACKSTORY = GroupName "backstory"
pattern BACKSTORY_FLUFF_UNKNOWN = GroupName "fluff backstory unknown"
pattern BACKSTORY_FLUFF = GroupName "fluff backstory"
pattern BACKSTORY_GOOD_UNKNOWN = GroupName "good backstory unknown"
pattern BACKSTORY_GOOD = GroupName "good backstory"
pattern BACKSTORY_BAD_UNKNOWN = GroupName "bad backstory unknown"
pattern BACKSTORY_BAD = GroupName "bad backstory"
pattern BACKSTORY_MIXED_UNKNOWN = GroupName "mixed backstory unknown"
pattern BACKSTORY_MIXED = GroupName "mixed backstory"
pattern BACKSTORY_NEUTRAL_UNKNOWN = GroupName "neutral backstory unk."
pattern BACKSTORY_NEUTRAL = GroupName "neutral backstory"

-- * Content

organs :: [ItemKind]
organs =
  [fist, foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, tip, lip, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, bark, eye3, eye6, eye8, vision6, vision12, vision16, nostril, ear3, ear6, ear8, rattleOrgan, insectMortality, sapientBrain, animalBrain, speedGland5, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed]
  -- Allure-specific
  ++ [animalStomach, hungry, smallBeak, razor, liveWire, flotationBag, inkSac, powerfulHindLegs, coiledTail, jetBooster, rhinoInertia, electricAmbience, electricAmbienceRecharge, robotBrain, hullPlating, mouthVent, dustVent, dustFissure, fuelVent, fuelFissure, geneticFlaw3BadArmorMelee, geneticFlaw3BadArmorRanged, geneticFlaw10BadArmorMelee, geneticFlaw10BadArmorRanged, backstoryFluffTemplate, backstoryFluff1, backstoryGoodTemplate, backstoryGood1, backstoryGood2, backstoryGood3, backstoryBadTemplate, backstoryBad1, backstoryBad2, backstoryBad3, backstoryBad4, backstoryMixedTemplate, backstoryMixed1, backstoryMixed2, backstoryNeutralTemplate, backstoryNeutral1, backstoryNeutral2, backstoryNeutral3]

fist,    foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, tip, lip, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, bark, eye3, eye6, eye8, vision6, vision12, vision16, nostril, ear3, ear6, ear8, rattleOrgan, insectMortality, sapientBrain, animalBrain, speedGland5, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed :: ItemKind
-- Allure-specific
animalStomach,       hungry, smallBeak, razor, liveWire, flotationBag, inkSac, powerfulHindLegs, coiledTail, jetBooster, rhinoInertia, electricAmbience, electricAmbienceRecharge, robotBrain, hullPlating, mouthVent, dustVent, dustFissure, fuelVent, fuelFissure, geneticFlaw3BadArmorMelee, geneticFlaw3BadArmorRanged, geneticFlaw10BadArmorMelee, geneticFlaw10BadArmorRanged, backstoryFluffTemplate, backstoryFluff1, backstoryGoodTemplate, backstoryGood1, backstoryGood2, backstoryGood3, backstoryBadTemplate, backstoryBad1, backstoryBad2, backstoryBad3, backstoryBad4, backstoryMixedTemplate, backstoryMixed1, backstoryMixed2, backstoryNeutralTemplate, backstoryNeutral1, backstoryNeutral2, backstoryNeutral3 :: ItemKind

symbolNecklace, symbolWand :: ContentSymbol ItemKind
symbolNecklace = rsymbolNecklace $ ritemSymbols standardRules
symbolWand = rsymbolWand $ ritemSymbols standardRules

-- * No-cooldown melee damage organs without effects

thorn = fist
  { isymbol  = symbolWand
  , iname    = "thorn"
  , ifreq    = [(S_THORN, 1)]
  , icount   = 2 + 1 `d` 2  -- unrealistic, but not boring
  , iverbHit = "puncture"
  , idamage  = 1 `d` 1
  , iaspects = [SetFlag Meleeable]  -- not Durable
  , ieffects = [VerbNoLonger "be not so thorny any more" "."]
  , idesc    = "Sharp yet brittle."
  }
tip = fist
  { iname    = "tip"
  , ifreq    = [(S_TIP, 1)]
  , icount   = 1
  , iverbHit = "poke"
  , idamage  = 1 `d` 1
  , idesc    = ""
  }
fist = ItemKind
  { isymbol  = toContentSymbol ','
  , iname    = "fist"
  , ifreq    = [(S_FIST, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 2
  , irarity  = [(1, 1)]
  , iverbHit = "punch"
  , iweight  = 2000
  , idamage  = 2 `d` 1
  , iaspects = [SetFlag Durable, SetFlag Meleeable]
  , ieffects = []
  , idesc    = "Simple but effective."
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [(S_FOOT, 1)]
  , iverbHit = "kick"
  , idamage  = 2 `d` 1
  , idesc    = "A weapon you can still use if disarmed."
                 -- great example of tutorial hints inside a flavourful text
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [(S_SMALL_CLAW, 1)]
  , iverbHit = "slash"
  , idamage  = 2 `d` 1
  , idesc    = "A pearly spike."
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [(S_SNOUT, 1)]
  , icount   = 1
  , iverbHit = "nudge"
  , idamage  = 2 `d` 1
  , idesc    = "Sensitive and wide-nostrilled."
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [(S_SMALL_JAW, 1)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 3 `d` 1
  , idesc    = "Filled with small, even teeth."
  }

-- * Cooldown melee damage organs without effects

tentacle = fist  -- two copies only
  { iname    = "tentacle"
  , ifreq    = [(S_TENTACLE, 1)]
  , iverbHit = "slap"
  , idamage  = 4 `d` 1
  , iaspects = Timeout 3  -- minimal timeout that lets other organs show
               : iaspects fist
  , idesc    = "Damp and dextrous."
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [(S_JAW, 1)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 5 `d` 1
  , iaspects = Timeout (2 + 1 `d` 2)  -- no effect, but limit raw damage
               : iaspects fist
  , idesc    = "Delivers a powerful bite."
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [(S_HORN, 1)]
  , iverbHit = "impale"
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 7  -- no effect, but limit raw damage; two copies
               , AddSkill SkArmorMelee 10 ]  -- bonus doubled
               ++ iaspects fist
  , idesc    = "Sharp and long, for defence or attack."
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [(S_LARGE_TAIL, 1)]
  , icount   = 1
  , iverbHit = "knock"
  , idamage  = 6 `d` 1  -- not sharp
  , iaspects = Timeout (4 + 1 `d` 2)  -- one copy, so can be low
               : iaspects fist
  , idesc    = "Almost as long as the trunk."
  }
largeJaw = fist  -- organs can't be too weak, because some non-humans also use
                 -- human weapons and then the sudden contrast would be cruel
  { iname    = "large jaw"
  , ifreq    = [(S_LARGE_JAW, 1)]
  , icount   = 1
  , iverbHit = "crush"
  , idamage  = 10 `d` 1  -- tops the best non-crafted, non-unique human weapons
  , iaspects = Timeout (8 + 1 `d` 2)  -- no effect, but limit raw damage
               : iaspects fist
  , idesc    = "Enough to swallow anything in a single gulp."
  }

-- * Direct damage organs with effects

beeSting = fist
  { isymbol  = symbolWand
  , iname    = "bee sting"
  , ifreq    = [(S_BEE_STING, 1)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 200, AddSkill SkArmorRanged 45
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [Paralyze 10, RefillHP 3]  -- low gain; no tragedy if dies early
                 -- no special message when runs out, because it's 1 copy
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { isymbol  = symbolWand
  , iname    = "sting"
  , ifreq    = [(S_STING, 1)]
  , icount   = 1
  , iverbHit = "inject"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (10 - 1 `dL` 4)
               : iaspects fist
  , ieffects = [toOrganBad S_RETAINING (3 + 1 `d` 3)]
  , idesc    = "Painful, debilitating and harmful."
  }
lip = fist
  { iname    = "lip"
  , ifreq    = [(S_LIP, 1)]
  , icount   = 1
  , iverbHit = "lap"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (3 + 1 `d` 2)
               : iaspects fist
  , ieffects = [toOrganBad S_WEAKENED (1 + 1 `dL` 3)]
  , idesc    = ""
  }
venomTooth = fist
  { isymbol  = symbolWand
  , iname    = "venom tooth"
  , ifreq    = [(S_VENOM_TOOTH, 1)]
  , iverbHit = "bite"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (10 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [toOrganBad S_SLOWED (2 + 1 `dL` 3)]
  , idesc    = "A chilling numbness spreads from its bite."
  }
hookedClaw = fist
  { isymbol  = symbolWand
  , iname    = "hooked claw"
  , ifreq    = [(S_HOOKED_CLAW, 1)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , idamage  = 2 `d` 1
  , iaspects = Timeout (12 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [toOrganBad S_SLOWED 2]
  , idesc    = "A curved talon."
  }
screechingBeak = fist
  { isymbol  = symbolWand
  , iname    = "screeching beak"
  , ifreq    = [(S_SCREECHING_BEAK, 1)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = 3 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [Summon SCAVENGER $ 1 `dL` 3]
  , idesc    = "Both a weapon and a beacon, calling more scavengers to the meal."
  }
antler = fist
  { isymbol  = symbolWand
  , iname    = "antler"
  , ifreq    = [(S_ANTLER, 1)]
  , iverbHit = "ram"
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout $ 3 + (1 `d` 3) * 3
               , AddSkill SkArmorMelee 10 ]  -- bonus doubled
               ++ iaspects fist
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
  , idesc    = ""
  }
rhinoHorn = fist
  { isymbol  = symbolWand
  , iname    = "ugly horn"  -- made of keratin, unlike real horns
  , ifreq    = [(S_RHINO_HORN, 1)]
  , icount   = 1  -- single, unlike real horns
  , iverbHit = "gore"
  , idamage  = 4 `d` 1
  , iaspects = Timeout 5
               : iaspects fist
  , ieffects = [Impress, Yell]  -- the owner is a mid-boss, after all
  , idesc    = "Very solid, considering it has the same composition as fingernails."
  }
hugeTail = fist
  { isymbol  = symbolWand
  , iname    = "huge tail"
  , ifreq    = [(S_HUGE_TAIL, 1)]
  , icount   = 1
  , iverbHit = "upend"
  , idamage  = 7 `d` 1
  , iaspects = Timeout (7 + 1 `d` 2)
               : iaspects fist
                 -- timeout higher, lest they regain push before closing again
  , ieffects = [PushActor (ThrowMod 200 50 1)]  -- 1 step, fast
      -- 2 steps would be too hard to counteract via second line of heroes
  , idesc    = "This immense and muscular tail transfers a huge momentum in a matter of seconds."
  }

-- * Melee weapons without direct damage

venomFang = fist
  { isymbol  = symbolWand
  , iname    = "venom fang"
  , ifreq    = [(S_VENOM_FANG, 1)]
  , iverbHit = "bite"
  , idamage  = 0
  , iaspects = Timeout (10 - 1 `dL` 5)
               : iaspects fist
  , ieffects = [toOrganNoTimer S_POISONED]
  , idesc    = "Dripping with deadly venom."
  }

-- * Special melee weapons

sulfurFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(S_MEDBOT_FISSURE, 1)]
  , icount   = 2 + 1 `d` 2
  , idamage  = 0  -- heal not via (negative) idamage, for armour would block it
  , iaspects = SetFlag Benign : iaspects boilingFissure
  , ieffects = [ RefillHP 5
               , toOrganNoTimer S_HUNGRY  -- the metabolic price to pay
               , VerbNoLonger "run out of nano medbot liquid" "." ]
  , idesc    = ""
  }
boilingFissure = fist
  { isymbol  = symbolWand
  , iname    = "fissure"
  , ifreq    = [(S_BOILING_FISSURE, 1)]
  , icount   = 2 + 1 `d` 2
  , iverbHit = "hiss at"
  , idamage  = 2 `d` 1  -- can remove hunger, so high price
  , iaspects = [ AddSkill SkHurtMelee 20  -- decreasing as count decreases
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [ DropItem 1 1 COrgan CONDITION  -- useful; limited; HP price
               , VerbNoLonger "widen the crack, releasing pressure" "." ]
  , idesc    = ""
  }
arsenicFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(S_COOLING_FISSURE, 1)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 2 `d` 1
  , ieffects = [ toOrganBad S_PARSIMONIOUS (5 + 1 `d` 3)
                   -- weaken/freeze, impacting intellectual abilities first
               , VerbNoLonger "clog with ice" "." ]
  , idesc    = ""
  }

-- * Armor organs

armoredSkin = ItemKind
  { isymbol  = toContentSymbol ','
  , iname    = "armored skin"
  , ifreq    = [(S_ARMORED_SKIN, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 30, AddSkill SkArmorRanged 15
               , SetFlag Durable ]
  , ieffects = []
  , idesc    = "Homemade armour is just as good."  -- hmm, it may get confused with leather armor jackets, etc.
  , ikit     = []
  }
bark = armoredSkin
  { iname    = "bark"
  , ifreq    = [(S_BARK, 1)]
  , idesc    = ""
  }

-- * Sense organs

eye :: Int -> GroupName ItemKind -> ItemKind
eye n grp = armoredSkin
  { iname    = "eye"
  , ifreq    = [(grp, 1)]
  , icount   = 2
  , iverbHit = "glare at"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = "A piercing stare."
  }
eye3 = eye 3 S_EYE_3
eye6 = eye 6 S_EYE_6
eye8 = eye 8 S_EYE_8
vision :: Int -> GroupName ItemKind -> ItemKind
vision n grp = armoredSkin
  { iname    = "vision"
  , ifreq    = [(grp, 1)]
  , iverbHit = "visualize"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
vision6 = vision 6 S_VISION_6
vision12 = vision 12 S_VISION_12
vision16 = vision 16 S_VISION_16
nostril = armoredSkin
  { iname    = "nostril"
  , ifreq    = [(S_NOSTRIL, 1)]
  , icount   = 2
  , iverbHit = "snuff"
  , iaspects = [ AddSkill SkSmell 1  -- times 2, from icount
               , SetFlag Durable ]
  , idesc    = ""
  }
ear :: Int -> GroupName ItemKind -> ItemKind
ear n grp = armoredSkin
  { iname    = "ear"
  , ifreq    = [(grp, 1)]
  , icount   = 2
  , iverbHit = "overhear"
  , iaspects = [ AddSkill SkHearing (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
ear3 = ear 3 S_EAR_3
ear6 = ear 6 S_EAR_6
ear8 = ear 8 S_EAR_8

-- * Assorted

rattleOrgan = armoredSkin
  { iname    = "rattle"
  , ifreq    = [(S_RATLLE, 1)]
  , iverbHit = "announce"
  , iaspects = [ Timeout $ 10 + (1 `d` 3) * 10  -- long, to limit spam
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [Yell, RefillCalm 5]
  , idesc    = ""
  }
insectMortality = armoredSkin
  { iname    = "insect mortality"
  , ifreq    = [(S_INSECT_MORTALITY, 1)]
  , iverbHit = "age"
  , iaspects = [ AddSkill SkAggression 2  -- try to attack before you die
               , Timeout $ 30 + (1 `d` 3) * 10  -- die very slowly
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP (-1), Yell]
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [(S_SAPIENT_BRAIN, 1)]
  , iverbHit = "outbrain"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]  -- can move at once when waking up
               ++ [AddSkill SkAlter 4]  -- can use all stairs; dig rubble, ice
               ++ [AddSkill SkWait 2]  -- can brace and sleep
               ++ [AddSkill SkApply 1]  -- can use most items, not just foods
               ++ [SetFlag Durable]
  , idesc    = ""
  }
animalBrain = armoredSkin
  { iname    = "animal brain"
  , ifreq    = [(S_ANIMAL_BRAIN, 1)]
  , iverbHit = "blank"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]  -- can move at once when waking up
               ++ [AddSkill SkAlter 2]  -- can use normal stairs; can't dig
               ++ [AddSkill SkWait 2]  -- can brace and sleep
               -- No @SkApply@ bonus, so can only apply foods. Note, however,
               -- that AI doesn't risk applying unIded items, so in early
               -- game animals won't eat anything.
               ++ [AddSkill SkDisplace (-1)]  -- no melee tactics
               ++ [AddSkill SkMoveItem (-1)]  -- no item gathering
               ++ [AddSkill SkProject (-1)]  -- nor item flinging
               ++ [SetFlag Durable]
  , idesc    = ""
  }
speedGland :: Int -> GroupName ItemKind -> ItemKind
speedGland n grp = armoredSkin
  { isymbol  = symbolWand
  , iname    = "speed gland"
  , ifreq    = [(grp, 1)]
  , iverbHit = "spit at"
  , iaspects = [ Timeout $ intToDice (100 `div` n)
               , AddSkill SkSpeed $ intToDice n
               , AddSkill SkArmorMelee $ intToDice $ - n * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 1]
  , idesc    = ""
  }
speedGland5 = speedGland 5 S_SPEED_GLAND_5
speedGland10 = speedGland 10 S_SPEED_GLAND_10
scentGland = armoredSkin
  { isymbol  = symbolWand
  , iname    = "scent gland"
  , ifreq    = [(S_SCENT_GLAND, 1)]
  , icount   = 10 + 1 `d` 3  -- runs out
  , iverbHit = "spray at"
  , iaspects = [ Timeout $ (1 `d` 3) * 10
               , SetFlag Periodic, SetFlag Fragile ]  -- not Durable
  , ieffects = [ VerbNoLonger "look spent" "."
               , ApplyPerfume
               , Explode S_DISTRESSING_ODOR ]
                   -- keep explosion at the end to avoid the ambiguity of
                   -- "of ([foo explosion] of [bar])"
  , idesc    = ""
  }
sulfurVent = armoredSkin
  { isymbol  = toContentSymbol 'v'
  , iname    = "vent"
  , ifreq    = [(S_MEDBOT_VENT, 1)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (3 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_MELEE_PROTECTIVE_BALM]
  , idesc    = ""
  }
boilingVent = armoredSkin
  { isymbol  = toContentSymbol 'v'
  , iname    = "vent"
  , ifreq    = [(S_BOILING_VENT, 1)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (4 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_BOILING_WATER]
  , idesc    = ""
  }
arsenicVent = armoredSkin
  { isymbol  = toContentSymbol 'v'
  , iname    = "vent"
  , ifreq    = [(S_COOLING_VENT, 1)]
  , iflavour = zipPlain [White]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_VIOLENT_SLOWNESS_MIST]
  , idesc    = ""
  }

-- * Special

bonusHP = armoredSkin
  { isymbol  = toContentSymbol 'H'  -- '+' reserved for conditions
  , iname    = "bonus HP"
  , ifreq    = [(S_BONUS_HP, 1)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddSkill SkMaxHP 1]
  , idesc    = "Special training and connections in the right places give this adventurer reinforced musculature and augmented internal organs, much more resilient to damage."
  }
braced = armoredSkin
  { isymbol  = toContentSymbol 'B'
  , iname    = "braced"
  , ifreq    = [(S_BRACED, 1)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "brace"
  , iweight  = 0
  , iaspects = [ AddSkill SkArmorMelee 50, AddSkill SkArmorRanged 25
               , AddSkill SkHearing 10
               , SetFlag Condition ] -- hack: display as condition
  , idesc    = "Apart of increased resilience to attacks, being braced protects from displacement by foes and other forms of forced translocation, e.g., pushing or pulling."
  }
asleep = armoredSkin
  { isymbol  = toContentSymbol 'S'
  , iname    = "asleep"
  , ifreq    = [(S_ASLEEP, 1)]
  , iflavour = zipPlain [BrGreen]  -- regenerates HP (very slowly)
  , icount   = 5
  , iverbHit = "slay"
  , iweight  = 0
  , iaspects = [AddSkill sk (-1) | sk <- [SkMove .. SkApply]]
               ++ [ AddSkill SkMelee 1, AddSkill SkAlter 1, AddSkill SkWait 1
                  , AddSkill SkSight (-3), AddSkill SkArmorMelee (-10)
                  , SetFlag Condition ]  -- hack: display as condition
  , idesc    = "Sleep helps to regain health, albeit extremely slowly. Being asleep makes you vulnerable, with gradually diminishing effects as the slumber wears off over several turns. Any non-idle action, not only combat but even yawning or stretching removes a sizable portion of the sleepiness."
  }
impressed = armoredSkin
  { isymbol  = toContentSymbol 'I'
  , iname    = "impressed"  -- keep the same as in @ifreq@, to simplify code
  , ifreq    = [(S_IMPRESSED, 1), (CONDITION, 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxCalm (-1)  -- to help player notice on HUD
                                          -- and to count as bad condition
               , AddSkill SkArmorMelee (-3)
               , SetFlag Fragile  -- to announce "no longer" only when
                                  -- all copies gone
               , SetFlag Condition ]  -- this is really a condition,
                                      -- just not a timed condition
  , ieffects = [ OnSmash $ verbMsgLess "impressed"
               , OnSmash $ verbMsgNoLonger "impressed" ]
                   -- not periodic, so no wear each turn, so only @OnSmash@
  , idesc    = "Being impressed by one's adversary sounds like fun, but on battlefield it equals treason. Almost. Throw in depleted battle calm and it leads to mindless desertion outright."
  }

-- * Allure-specific melee weapons

smallBeak = fist
  { iname    = "small beak"
  , ifreq    = [(S_SMALL_BEAK, 1)]
  , icount   = 1
  , iverbHit = "nom"
  , idamage  = 2 `d` 1
  , idesc    = "Cute, but painful."
  }
liveWire = fist
  { isymbol  = symbolWand
  , iname    = "live wire"
  , ifreq    = [(S_LIVE_WIRE, 1)]
  , icount   = 1
  , iverbHit = "shock"
  , idamage  = 0
  , iaspects = Timeout (2 + 1 `d` 2)
               : iaspects fist
  , ieffects = [ Discharge 1 $ 80 - 1 `d` 40
               , RefillHP (-1) ]
  , idesc    = ""
  }
razor = fist
  { isymbol  = symbolWand
  , iname    = "razor edge"
  , ifreq    = [(S_RAZOR, 1)]
  , icount   = 1 + 1 `d` 2
  , iverbHit = "slice"
  , idamage  = 2 `d` 1
  , iaspects = [ SetFlag Meleeable  -- not Durable
               , Timeout 4 ]  -- but cooldown to use other weapons
  , ieffects = [ toOrganBad S_WEAKENED (2 + 1 `dL` 3)
               , VerbNoLonger "lose all sharpness" "." ]
                 -- we interpret charges as sharpness of the actor or his razor'
                 -- no pronoun in the message to avoid "you lose its sharpness"
  , idesc    = ""
  }
dustFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(S_DUST_FISSURE, 1)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 1 `d` 1
  , ieffects = [ toOrganBad S_WEAKENED 20
               , VerbNoLonger "cough one last time" "." ]
  , idesc    = ""
  }
fuelFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(S_FUEL_FISSURE, 1)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 0
  , ieffects = [ Burn 1
               , VerbNoLonger "have its fissures mended by emergency auto-sealants" "." ]
  , idesc    = ""
  }

-- * Allure-specific other

animalStomach = armoredSkin
  { isymbol  = toContentSymbol 'u'
  , iname    = "animal stomach"
  , ifreq    = [(S_ANIMAL_STOMACH, 1)]
  , iverbHit = "burp"
  , iaspects = [ Timeout $ 500 + (1 `d` 3) * 50  -- hunger very slowly
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [toOrganNoTimer S_HUNGRY]
  , idesc    = ""
  }
hungry = armoredSkin
  { isymbol  = toContentSymbol 'U'
  , iname    = "hungry"  -- keep the same as in @ifreq@, to simplify code
  , ifreq    = [(S_HUNGRY, 1), (CONDITION, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , iverbHit = "pang"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxHP (-1)
               , SetFlag Fragile  -- to announce "no longer" only when
                                  -- all copies gone
               , SetFlag Condition ]  -- this is really a condition,
                                      -- just not a timed condition
  , ieffects = [ OnSmash $ verbMsgLess "hungry"
               , OnSmash $ verbMsgNoLonger "hungry" ]
                   -- not periodic, so no wear each turn, so only @OnSmash@
  , idesc    = "Hunger limits physical fitness. In extreme cases, when compounded, it causes such fragility that the slightest stress becomes lethal."
  }
flotationBag = armoredSkin
  { isymbol  = toContentSymbol 'O'
  , iname    = "flotation bag"
  , ifreq    = [(S_FLOTATION_BAG, 1)]
  , iverbHit = "uplift"
  , iaspects = [AddSkill SkArmorRanged (-15), SetFlag Durable]
  , ieffects = [OnSmash $ Explode S_FOCUSED_CONCUSSION]
                  -- if too weak, use S_FOCUSED_FRAGMENTATION that is
                  -- less thematic than CONCUSSION, but more likely to cause
                  -- the chain reaction among peers that we are after
  , idesc    = "A large organ that enables effortless flight. It is essentially a hydrogen container with easily regulated internal pressure. It evolved a protection against blunt trauma, but not against puncture."
  }
inkSac = armoredSkin  -- neither melee nor aspects nor periodic, so to be
  { isymbol  = symbolNecklace    -- triggered, needs a special symbol
  , iname    = "ink sac"
  , ifreq    = [(S_INK_SAC, 1)]
  , iverbHit = "squirt"
  , iaspects = [Timeout $ 3 + 1 `d` 3, SetFlag Durable]
  , ieffects = [ Explode S_SMOKE  -- weak, but lingers long
               , PullActor (ThrowMod 200 50 1)  -- 1 step, fast
               , RefillCalm 100 ]  -- balance @Explode@ so that AI uses it
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Octopus#Ink_sac
  }
powerfulHindLegs = armoredSkin  -- neither melee nor periodic so to trigger
  { isymbol  = symbolNecklace              -- needs a special symbol
  , iname    = "pair"
  , ifreq    = [(S_POWERFUL_HIND_LEGS, 1)]
  , iverbHit = "jump"
  , iaspects = [ ELabel "of powerful hind legs"
               , Timeout $ 3 + 1 `d` 3, SetFlag Durable ]
  , ieffects = [toOrganGood S_HASTED 1]  -- see comments about jumping pole
  , idesc    = "With legs like that, just a moment of preparation is enough to make a rapid succession of huge leaps."  -- no damage, but feet or claws are separate organs and they can be weapons
  }
coiledTail = powerfulHindLegs
  { iname    = "coiled tail"
  , ifreq    = [(S_COILED_TAIL, 1)]
  , iverbHit = "spring"
  , iaspects = [Timeout $ 3 + 1 `d` 3, SetFlag Durable]
  , idesc    = "When the coiled tail springs, expect a lurch that leaves you no time to react."
  }
jetBooster = armoredSkin  -- neither melee nor periodic so to be triggered,
  { isymbol  = symbolNecklace        -- needs a special symbol
  , iname    = "jet booster"
  , ifreq    = [(S_JET_BOOSTER, 1)]
  , iverbHit = "dart"
  , iaspects = [Timeout $ 4 + 1 `d` 2, SetFlag Durable]
  , ieffects = [ PushActor (ThrowMod 800 100 1)  -- 8 steps, 2 turns
               , RefillCalm 100 ]  -- give AI an incentive to use it
  , idesc    = "The throttles are quite erratic with age, but the punch is none the less."
  }
rhinoInertia = jetBooster
  { iname    = "rhino inertia"
  , ifreq    = [(S_RHINO_INERTIA, 1)]
  , iverbHit = "thunder"
  , idesc    = "It's a struggle to move the mass and it's a reinforced concrete wall that stops it."
  }
electricAmbience = armoredSkin
  { isymbol  = toContentSymbol 'v'
  , iname    = "static current ambience"
  , ifreq    = [(ELECTRIC_AMBIENCE, 1)]
  , iverbHit = "shortcut"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_CURRENT_DISCHARGE]
  , idesc    = ""
  }
electricAmbienceRecharge = electricAmbience
  { iname    = "static current ambience"
  , ieffects = [RefillHP 1, Explode S_CURRENT_RECHARGE]
  }
robotBrain = armoredSkin
  { iname    = "robot brain"
  , ifreq    = [(S_ROBOT_BRAIN, 1)]
  , iverbHit = "outcompute"
  , iaspects = [AddSkill sk 1 | sk <- [SkMove .. SkApply]]
               ++ [AddSkill SkMove 4]  -- can move at once when waking up
               ++ [AddSkill SkAlter 1]  -- can open doors; only easiest stairs
               ++ [AddSkill SkWait 2]  -- can brace and sleep
               -- No @SkAlter@ bonus, so can only use the easiest stairs.
               ++ [AddSkill SkApply (-1)]  -- can't even eat food, but can fling
               ++ [SetFlag Durable]
  , idesc    = ""
  }
hullPlating = armoredSkin
  { iname    = "hull plating"
  , ifreq    = [(S_HULL_PLATING, 1)]
  , idesc    = ""
  }
mouthVent = armoredSkin
  { isymbol  = toContentSymbol 'v'
  , iname    = "mouth vent"
  , ifreq    = [(S_MOUTH_VENT, 1)]
  , iflavour = zipPlain [BrMagenta]
  , iverbHit = "surprise"
  , iaspects = [ Timeout 7
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [OneOf $
      map (AndEffect (Explode S_SMOKE) . uncurry VerbMsg)
          [ ("say: Sir, your luggage has already been collected", ".")
          , ("ask: Would you kindly help me?", "")
          , ("complain: I can't reach you with this tool", "." ) ]
      ++ map Explode
             [ S_PHEROMONE, S_RHINO_HOLOGRAM, S_CURRENT_DISCHARGE
             , blastNoStatOf S_IMMOBILE, S_SPARK ]]
  , idesc    = ""
  }
dustVent = armoredSkin
  { isymbol  = toContentSymbol 'v'
  , iname    = "vent"
  , ifreq    = [(S_DUST_VENT, 1)]
  , iflavour = zipPlain [BrCyan]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (5 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_VIOLENT_FLASH]
  , idesc    = ""
  }
fuelVent = armoredSkin
  { isymbol  = toContentSymbol 'v'
  , iname    = "vent"
  , ifreq    = [(S_FUEL_VENT, 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (3 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode S_VIOLENT_BURNING_OIL_4]
  , idesc    = ""
  }
-- HP change varies due to body size.
--
-- This is destroyed on drop due to being an organ and so it runs
-- the @OnSmash@ effects. Due to being a condition, it's not activated
-- at actor death, avoiding a lot of spam and bringing him back to life.
geneticFlaw :: Int -> Bool -> Int -> GroupName ItemKind -> ItemKind
geneticFlaw fr badArmorMelee n grp = armoredSkin
  { isymbol  = toContentSymbol 'F'
  , iname    = "genetic flaw"  -- keep the same as in @ifreq@, to simplify code
  , ifreq    = [(GENETIC_FLAW, fr), (grp, 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "flaw"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxHP (intToDice $ - n)
               , SetFlag MetaGame, SetFlag Condition ]
                   -- avoid being blamed in combat messages for bad defence,
                   -- but no CONDITION group, not to be healed too easily
               ++ [AddSkill SkWait (-1) | n >= 10]
               ++ [AddSkill SkApply (-1) | n >= 10]
               ++ if badArmorMelee
                  then [AddSkill SkArmorMelee (-20)]
                  else [AddSkill SkArmorRanged (-10)]
  , ieffects = [ OnSmash $ DropItem maxBound maxBound COrgan CONDITION
                   -- key for AI is it eliminates all impression conditions
               , OnSmash $ RefillHP n
               , OnSmash $ VerbNoLonger "undergo instant infracellular decontamination" "." ]  -- unlike the civilian version, this one is instant and the attunement is automatic and relatively quick (the usual double cooldown when equipping items again)
  , idesc    = "Nobody is perfect. At least without infracellular engineering, which is heavily regulated, insanely expensive and automatically reverted without refund before critical medical interventions. One more reason to be a good citizen, work hard and not die often. But where is the fun in that?"
  }
geneticFlaw3BadArmorMelee = geneticFlaw 1 True 3 GENETIC_FLAW_3
geneticFlaw3BadArmorRanged = geneticFlaw 3 False 3 GENETIC_FLAW_3
geneticFlaw10BadArmorMelee = geneticFlaw 1 True 10 GENETIC_FLAW_10
geneticFlaw10BadArmorRanged = geneticFlaw 3 False 10 GENETIC_FLAW_10

-- * Allure-specific backstory items

-- The name and description of each backstory item should add something
-- to the biography or character of the hero or both. However, it should
-- not constrain it so much that it conflicts with any other items.
-- E.g., 'life spent in military' is not acceptable,
-- but a spell in military is fine, just as is life spent doing
-- dangerous tasks, serious missions or giving orders.
-- The idea, in addition to avoiding inconsistency, is to let the player
-- fill in the gaps and, in effect, invent the backstories.
-- OTOH, the player needs something to work with, so a Wikipedia
-- definition of a mild vice or a veiled allusion to in-game mechanics
-- of a virtue are not enough. It's fine if some backstory
-- items are more constraining that others. Variety is good.
--
-- Make sure the effects here that not always fire don't fail with UseId
-- and so reveal the backstory item too early and without the fun.
-- The exception is fluff items, where the effects are marginal by definition.
backstoryFluffTemplate = ItemKind
  { isymbol  = toContentSymbol '?'
  , iname    = "unrevealed rumination"
  , ifreq    = [(BACKSTORY_FLUFF_UNKNOWN, 1)]
  , iflavour = zipStory [BrBlue, Blue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "surprise"
  , iweight  = 0
  , idamage  = 0
  , iaspects = [ PresentAs BACKSTORY_FLUFF_UNKNOWN, SetFlag MetaGame
               , SetFlag Durable
               , SetFlag MinorAspects ]
                   -- avoid question marks by weapons in HUD, because,
                   -- unlike necklaces, these won't have stat boosts
  , ieffects = []
  , idesc    = "Not all crucial facts about a team member are remembered and revealed as soon as would be most beneficial for the team."
  , ikit     = []
  }
backstoryFluff1 = backstoryFluffTemplate
  { iname    = "\"Naughty Kid\""
  , ifreq    = [(BACKSTORY_FLUFF, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 500, SetFlag UnderMelee]
               ++ iaspects backstoryFluffTemplate
  , ieffects = [When (CalmLeq 30) $ OneOf $ map (uncurry VerbMsg)
      [ ("rage, froth-mouthed", ".")
      , ("say: Déjà vu?", "")
      , ("yell: I missed that so", "!")
      , ("hesitate for the briefest moment", ".")
      , ("inquire: in the face? in the face?", "")
      , ("lose it", ".")
      ]]
  , idesc    = "Rumination: Bad temper, anguish and uncontrollable fury blot out all other childhood memories."
  }
backstoryGoodTemplate = backstoryFluffTemplate
  { iname    = "unrevealed virtue"
  , ifreq    = [(BACKSTORY_GOOD_UNKNOWN, 1)]
  , iflavour = zipStory [cGoodEvent, cVeryGoodEvent]
  , iaspects = [ PresentAs BACKSTORY_GOOD_UNKNOWN, SetFlag MetaGame
               , SetFlag Durable, SetFlag MinorAspects ]
  }
backstoryGood1 = backstoryGoodTemplate
  { iname    = "\"Zero g Champ\""
  , ifreq    = [(BACKSTORY_GOOD, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 1000, SetFlag UnderRanged, SetFlag Unique]
               ++ iaspects backstoryGoodTemplate
                 -- unique, so that team not overpowered
  , ieffects = [toOrganGood S_RANGED_DEFLECTING 10]
                  -- rare, but long lasting
  , idesc    = "Virtue: Years of training for null gravity squash tournaments unexpectedly pay off. At this very distance and speed, trajectory angles are obvious and lounging and feinting comes naturally."
  }
backstoryGood2 = backstoryGoodTemplate
  { iname    = "\"Loyalty ritual\""
  , ifreq    = [(BACKSTORY_GOOD, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 200, SetFlag Periodic]
               ++ iaspects backstoryGoodTemplate
  , ieffects = [Impress `AndEffect` VerbMsg "mumble repeatedly" "."]
  , idesc    = "Virtue: Reciting, every once in a while, the names of all the people your life directly depends on is a proven loyalty-affirming custom. It also lets you pinpoint the moment you start hating a teammate."  -- 'ritual' is exotic enough to add to biography
  }
backstoryGood3 = backstoryGoodTemplate
  { iname    = "\"Bravery\""
  , ifreq    = [(BACKSTORY_GOOD, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 1, SetFlag UnderMelee]
               ++ iaspects backstoryGoodTemplate
  , ieffects = [When (CalmLeq 2) $ When (TriggeredBy ActivationMeleeable)
                $ SeqEffect
                    [ Recharge 4 20
                    , RefillCalm 2
                    ]]
                 -- @TriggeredBy@ condition prevents micro-management
  , idesc    = "Virtue: Several years of life threatening events leave either a trauma or a lesson. The lesson is, when fear is the strongest, turn away from yourself and fiercely focus on your mission."
  }
backstoryBadTemplate = backstoryFluffTemplate
  { iname    = "unrevealed vice"
  , ifreq    = [(BACKSTORY_BAD_UNKNOWN, 1)]
  , iflavour = zipStory [cBadEvent, cVeryBadEvent]
  , iaspects = [ PresentAs BACKSTORY_BAD_UNKNOWN, SetFlag MetaGame
               , SetFlag Durable, SetFlag MinorAspects ]
  }
backstoryBad1 = backstoryBadTemplate
  { iname    = "\"Drug addiction\""
  , ifreq    = [(BACKSTORY_BAD, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 200, SetFlag Periodic, SetFlag Unique]
               ++ iaspects backstoryBadTemplate
  , ieffects = [When (CalmLeq 40) $ When (CalmGeq 20) $ SeqEffect
      [ toOrganBad S_SLOWED (3 + 1 `d` 3)
      , toOrganBad S_FRENZIED (40 + 1 `d` 10)
      , toOrganGood S_STRENGTHENED (5 + 1 `d` 5) ]]  -- to short to compensate
  , idesc    = "Vice: The small pill that distracts from mild anxiety also distracts from survival and responsibility towards teammates."
  }
backstoryBad2 = backstoryBadTemplate
  { iname    = "\"Overconfidence\""
  , ifreq    = [(BACKSTORY_BAD, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 200, SetFlag UnderMelee]
               ++ iaspects backstoryBadTemplate
  , ieffects = [When (CalmGeq 80) $ SeqEffect
      [ toOrganBad S_WEAKENED (20 + 1 `d` 5)
      , toOrganBad S_DEAF (20 + 1 `d` 5) ]]
  , idesc    = "Vice: Never underestimate a maddened, towering abomination, when you only have a stick to fend it off with. Bonus advice: the fact you excelled in school athletics doesn't mean every power in the universe is going to read you body language and cower."
  }
backstoryBad3 = backstoryBadTemplate
  { iname    = "\"Arrogance\""
  , ifreq    = [(BACKSTORY_BAD, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 200, SetFlag UnderMelee]
               ++ iaspects backstoryBadTemplate
  , ieffects = [When (CalmGeq 80) $ SeqEffect
      [ Discharge 3 40
      , Yell
      , OneOf $ map (uncurry VerbMsg)
          [ ("yell: How dare you touch me?", "")
          , ("yell: Do you know who I am?", "")
          , ("yell: Calling my lawyer", "!")
          ] ]]
  , idesc    = "Vice: Privilege can be a boon, but if taken for granted, it can be a doom."
  }
backstoryBad4 = backstoryBadTemplate
  { iname    = "\"Alcoholism\""
  , ifreq    = [(BACKSTORY_BAD, 100), (BACKSTORY, 1)]
  , iaspects = [ Timeout 200, SetFlag Periodic
               , SetFlag Unique ]  -- a tragic backstory and so unique
               ++ iaspects backstoryBadTemplate
  , ieffects = [AtMostOneOf  -- simple but pessimistic probability; not accurate
      [ DestroyItem 1 1 CStash ALCOHOL
          -- this is not drinking, but smashing; say, inner fight, won;
          -- @ConsumeItems [(1, ALCOHOL)] []@ would drink, but not from stash
          -- and only for alcohols that are durable, which is unlikely
        `OrEffect`  -- only create if none available
        SeqEffect [ CreateItem (Just 1) CStash ALCOHOL timerNone
                  , VerbMsg "explain: Some moonshine from gathered scraps, just in case" "." ]
      , NopEffect  -- always fails, no identification
      ]]
  , idesc    = "Vice: Adventurers that are abstaining alcoholics are double heroes, even if they too may trip sometimes. That doesn't make alcoholism a virtue, though."
  }
backstoryMixedTemplate = backstoryFluffTemplate
  { iname    = "unrevealed twist"
  , ifreq    = [(BACKSTORY_MIXED_UNKNOWN, 1)]
  , iflavour = zipStory [cRisk, cGraveRisk]
  , iaspects = [ PresentAs BACKSTORY_MIXED_UNKNOWN, SetFlag MetaGame
               , SetFlag Durable, SetFlag MinorAspects ]
  }
backstoryMixed1 = backstoryMixedTemplate
  { iname    = "\"Heavy eyes\""
  , ifreq    = [(BACKSTORY_MIXED, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 100, SetFlag Periodic]
               ++ iaspects backstoryMixedTemplate
  , ieffects = [AtMostOneOf [ PutToSleep
                            , NopEffect
                            , NopEffect
                            ]]
                  -- mixed, sleep refills Calm fully, but hobbles the actor
  , idesc    = "Twist: Can sleep anywhere, any time. The catch: sleeps anywhere, any time."
  }
backstoryMixed2 = backstoryMixedTemplate
  { iname    = "\"Too Young to Die\""
  , ifreq    = [(BACKSTORY_MIXED, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 1, SetFlag UnderRanged, SetFlag UnderMelee]
               ++ iaspects backstoryMixedTemplate
  , ieffects = [When (HpLeq 10) $ Unless (TriggeredBy ActivationTrigger)
                $ SeqEffect
      [RefillCalm (-10), toOrganGood S_HASTED 1, Recharge 1 20]]
        -- mixed: Calm lost but fury helps survive; may fire once per turn
        -- @TriggeredBy@ condition prevents micro-management
  , idesc    = "Twist: Despair, fury, denial. Beastly reactions to approaching death. This is unforgivable in space, especially when getting acquainted with dying is so accessible, for a generation already, either via hibernation and nanobot revival or mental exercise culminating in writing a will to be notarized Earth-side."  -- hint that the nanobot revival heroes use all the time may have stemmed from space travel hibernation research; if so, the culture followed, but some youngsters are lazy wimps
  }
backstoryNeutralTemplate = backstoryFluffTemplate
  { iname    = "unrevealed quirk"
  , ifreq    = [(BACKSTORY_NEUTRAL_UNKNOWN, 1)]
  , iflavour = zipStory [cNeutralEvent, cRareNeutralEvent]
  , iaspects = [ PresentAs BACKSTORY_NEUTRAL_UNKNOWN, SetFlag MetaGame
               , SetFlag Durable, SetFlag MinorAspects ]
  }
backstoryNeutral1 = backstoryNeutralTemplate
  { iname    = "\"Letting Go\""
  , ifreq    = [(BACKSTORY_NEUTRAL, 100), (BACKSTORY, 1)]
  , ieffects = [ OnSmash (Explode S_YOUTH_SPRINKLE)  -- may hit foes as well
               , Unless (TriggeredBy ActivationTrigger)
                 $ VerbMsg "laugh warmly" "." ]
                   -- never activated, but prevents premature identification
  , idesc    = "Quirk: Dying beautifully is an art that takes a lifetime to master and leaves spectators peaceful and uplifted. That's true even for clinical death, potentially reversible with nano medbot treatment back in town."
  }
-- Both can be moved to Mixed if less numerous, even though the effects mild.
backstoryNeutral2 = backstoryNeutralTemplate
  { iname    = "\"Mood swings\""
  , ifreq    = [(BACKSTORY_NEUTRAL, 100), (BACKSTORY, 1)]
  , iaspects = [Timeout 100, SetFlag Periodic]
               ++ iaspects backstoryMixedTemplate
  , ieffects = [When (HpLeq 50) $ OneOf [RefillCalm 20, RefillCalm (-20)]]
  , idesc    = "Quirk: Hormonal imbalances make it hard to compensate for natural neural system unsteadiness."
  }
backstoryNeutral3 = backstoryNeutralTemplate
  { iname    = "\"Cracking under stress\""
  , ifreq    = [(BACKSTORY_NEUTRAL, 100), (BACKSTORY, 1)]
  , iaspects = [SetFlag UnderRanged, SetFlag UnderMelee]
               ++ iaspects backstoryMixedTemplate
  , ieffects = [When (HpLeq 50) $ Unless (TriggeredBy ActivationTrigger)
                $ OneOf [RefillCalm 10, RefillCalm (-10)]]
                  -- @TriggeredBy@ condition prevents micro-management
  , idesc    = "Quirk: Not everyone needs to be impassive. A brush with death may result in panic today, but in cheering up and rallying the whole team tomorrow. However, if the cost of bad outcomes is prohibitive, exposed posts and front line assignments are better avoided."  -- 'the whole team' is an exaggeration, but it's in-character here
  }
