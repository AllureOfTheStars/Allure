-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor organ definitions.
module Content.ItemKindOrgan
  ( -- * Group name patterns
    pattern FIST, pattern FOOT, pattern HOOKED_CLAW, pattern SMALL_CLAW, pattern SNOUT, pattern SMALL_JAW, pattern JAW, pattern LARGE_JAW, pattern ANTLER, pattern HORN, pattern RHINO_HORN, pattern TENTACLE, pattern THORN, pattern BOILING_FISSURE, pattern ARSENIC_FISSURE, pattern SULFUR_FISSURE, pattern BEE_STING, pattern STING, pattern VENOM_TOOTH, pattern VENOM_FANG, pattern SCREECHING_BEAK, pattern LARGE_TAIL, pattern HUGE_TAIL, pattern ARMORED_SKIN, pattern BARK, pattern NOSTRIL, pattern RATLLE, pattern INSECT_MORTALITY, pattern SAPIENT_BRAIN, pattern ANIMAL_BRAIN, pattern SCENT_GLAND, pattern BOILING_VENT, pattern ARSENIC_VENT, pattern SULFUR_VENT, pattern SCAVENGER, pattern EYE_3, pattern EYE_6, pattern EYE_8, pattern VISION_6, pattern VISION_12, pattern VISION_16, pattern EAR_3, pattern EAR_6, pattern EAR_8, pattern SPEED_GLAND_5, pattern SPEED_GLAND_10
  , pattern ANIMAL_STOMACH, pattern HUNGRY, pattern GENETIC_FLAW, pattern GENETIC_FLAW_3, pattern GENETIC_FLAW_10, pattern RAZOR, pattern SMALL_BEAK, pattern LIVE_WIRE, pattern BIOGAS_VENT, pattern BIOGAS_FISSURE, pattern MEDBOT_VENT, pattern MEDBOT_FISSUE, pattern ROBOT_BRAIN, pattern HULL_PLATING, pattern MOUTH_VENT, pattern CRUDE_WELD
  , -- * Content
    organs
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindBlast
import Content.ItemKindTemporary
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

pattern FIST, FOOT, HOOKED_CLAW, SMALL_CLAW, SNOUT, SMALL_JAW, JAW, LARGE_JAW, ANTLER, HORN, RHINO_HORN, TENTACLE, THORN, BOILING_FISSURE, ARSENIC_FISSURE, SULFUR_FISSURE, BEE_STING, STING, VENOM_TOOTH, VENOM_FANG, SCREECHING_BEAK, LARGE_TAIL, HUGE_TAIL, ARMORED_SKIN, BARK, NOSTRIL, RATLLE, INSECT_MORTALITY, SAPIENT_BRAIN, ANIMAL_BRAIN, SCENT_GLAND, BOILING_VENT, ARSENIC_VENT, SULFUR_VENT, SCAVENGER, EYE_3, EYE_6, EYE_8, VISION_6, VISION_12, VISION_16, EAR_3, EAR_6, EAR_8, SPEED_GLAND_5, SPEED_GLAND_10 :: GroupName ItemKind

pattern ANIMAL_STOMACH, HUNGRY, GENETIC_FLAW, GENETIC_FLAW_3, GENETIC_FLAW_10, RAZOR, SMALL_BEAK, LIVE_WIRE, BIOGAS_VENT, BIOGAS_FISSURE, MEDBOT_VENT, MEDBOT_FISSUE, ROBOT_BRAIN, HULL_PLATING, MOUTH_VENT, CRUDE_WELD :: GroupName ItemKind

pattern FIST = GroupName "fist"
pattern FOOT = GroupName "foot"
pattern HOOKED_CLAW = GroupName "hooked claw"
pattern SMALL_CLAW = GroupName "small claw"
pattern SNOUT = GroupName "snout"
pattern SMALL_JAW = GroupName "small jaw"
pattern JAW = GroupName "jaw"
pattern LARGE_JAW = GroupName "large jaw"
pattern ANTLER = GroupName "antler"
pattern HORN = GroupName "horn"
pattern RHINO_HORN = GroupName "rhino horn"
pattern TENTACLE = GroupName "tentacle"
pattern THORN = GroupName "thorn"
pattern BOILING_FISSURE = GroupName "boiling fissure"
pattern ARSENIC_FISSURE = GroupName "arsenic fissure"
pattern SULFUR_FISSURE = GroupName "sulfur fissure"
pattern BEE_STING = GroupName "bee sting"
pattern STING = GroupName "sting"
pattern VENOM_TOOTH = GroupName "venom tooth"
pattern VENOM_FANG = GroupName "venom fang"
pattern SCREECHING_BEAK = GroupName "screeching beak"
pattern LARGE_TAIL = GroupName "large tail"
pattern HUGE_TAIL = GroupName "huge tail"
pattern ARMORED_SKIN = GroupName "armored skin"
pattern BARK = GroupName "bark"
pattern NOSTRIL = GroupName "nostril"
pattern RATLLE = GroupName "rattle"
pattern INSECT_MORTALITY = GroupName "insect mortality"
pattern SAPIENT_BRAIN = GroupName "sapient brain"
pattern ANIMAL_BRAIN = GroupName "animal brain"
pattern SCENT_GLAND = GroupName "scent gland"
pattern BOILING_VENT = GroupName "boiling vent"
pattern ARSENIC_VENT = GroupName "arsenic vent"
pattern SULFUR_VENT = GroupName "sulfur vent"
pattern SCAVENGER = GroupName "scavenger"
pattern EYE_3 = GroupName "eye 3"
pattern EYE_6 = GroupName "eye 6"
pattern EYE_8 = GroupName "eye 8"
pattern VISION_6 = GroupName "vision 6"
pattern VISION_12 = GroupName "vision 12"
pattern VISION_16 = GroupName "vision 16"
pattern EAR_3 = GroupName "ear 3"
pattern EAR_6 = GroupName "ear 6"
pattern EAR_8 = GroupName "ear 8"
pattern SPEED_GLAND_5 = GroupName "speed gland 5"
pattern SPEED_GLAND_10 = GroupName "speed gland 10"

-- ** Allure-specific
pattern ANIMAL_STOMACH = GroupName "animal stomach"
pattern HUNGRY = GroupName "hungry"
pattern GENETIC_FLAW = GroupName "genetic flaw"
pattern GENETIC_FLAW_3 = GroupName "genetic flaw 3"
pattern GENETIC_FLAW_10 = GroupName "genetic flaw 10"
pattern RAZOR = GroupName "razor"
pattern SMALL_BEAK = GroupName "small beak"
pattern LIVE_WIRE = GroupName "live wire"
pattern BIOGAS_VENT = GroupName "biogas vent"
pattern BIOGAS_FISSURE = GroupName "biogas fissure"
pattern MEDBOT_VENT = GroupName "medbot vent"
pattern MEDBOT_FISSUE = GroupName "medbot fissure"
pattern ROBOT_BRAIN = GroupName "robot brain"
pattern HULL_PLATING = GroupName "hull plating"
pattern MOUTH_VENT = GroupName "mouth vent"
pattern CRUDE_WELD = GroupName "crude weld"

-- * Content

organs :: [ItemKind]
organs =
  [fist, foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, bark, eye3, eye6, eye8, vision6, vision12, vision16, nostril, ear3, ear6, ear8, rattleOrgan, animalStomach, insectMortality, sapientBrain, animalBrain, speedGland5, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed]
  -- Allure-specific
  ++ [hungry, smallBeak, razor, liveWire, robotBrain, hullPlating, mouthVent, geneticFlaw3, geneticFlaw10]

fist,    foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, antler, horn, rhinoHorn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, hugeTail, armoredSkin, bark, eye3, eye6, eye8, vision6, vision12, vision16, nostril, ear3, ear6, ear8, rattleOrgan, animalStomach, insectMortality, sapientBrain, animalBrain, speedGland5, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, braced, asleep, impressed :: ItemKind
-- Allure-specific
hungry, smallBeak, razor, liveWire, robotBrain, hullPlating, mouthVent, geneticFlaw3, geneticFlaw10 :: ItemKind

-- Weapons

-- * Human weapon organs

fist = ItemKind
  { isymbol  = ','
  , iname    = "fist"
  , ifreq    = [(FIST, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 2
  , irarity  = [(1, 1)]
  , iverbHit = "punch"
  , iweight  = 2000
  , idamage  = 4 `d` 1
  , iaspects = [SetFlag Durable, SetFlag Meleeable]
  , ieffects = []
  , idesc    = "Simple but effective."
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [(FOOT, 1)]
  , iverbHit = "kick"
  , idamage  = 4 `d` 1
  , idesc    = "A weapon you can still use if disarmed."
                 -- great example of tutorial hints inside a flavourful text
  }

-- * Other weapon organs

hookedClaw = fist
  { iname    = "hooked claw"
  , ifreq    = [(HOOKED_CLAW, 1)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , idamage  = 2 `d` 1
  , iaspects = Timeout (12 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [toOrganBad SLOWED 2]
  , idesc    = "A curved talon."
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [(SMALL_CLAW, 1)]
  , iverbHit = "slash"
  , idamage  = 2 `d` 1
  , idesc    = "A pearly spike."
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [(SNOUT, 1)]
  , icount   = 1
  , iverbHit = "bite"
  , idamage  = 2 `d` 1
  , idesc    = "Sensitive and wide-nostrilled."
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [(SMALL_JAW, 1)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 3 `d` 1
  , idesc    = "Filled with small, even teeth."
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [(JAW, 1)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 5 `d` 1
  , idesc    = "Delivers a powerful bite."
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [(LARGE_JAW, 1)]
  , icount   = 1
  , iverbHit = "crush"
  , idamage  = 10 `d` 1
  , iaspects = [Timeout $ 2 + 1 `d` 2]  -- no effect, but limit raw damage
               ++ iaspects fist
  , idesc    = "Enough to swallow anything in a single gulp."
  }
antler = fist
  { iname    = "antler"
  , ifreq    = [(ANTLER, 1)]
  , icount   = 2
  , iverbHit = "ram"
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout $ 3 + (1 `d` 3) * 3
               , AddSkill SkArmorMelee 10 ]  -- bonus doubled
               ++ iaspects fist
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
  , idesc    = ""
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [(HORN, 1)]
  , icount   = 2
  , iverbHit = "impale"
  , idamage  = 5 `d` 1
  , iaspects = [ AddSkill SkHurtMelee 10
               , AddSkill SkArmorMelee 10 ]  -- bonus doubled
               ++ iaspects fist
  , idesc    = "Sharp and long, for defence or attack."
  }
rhinoHorn = fist
  { iname    = "ugly horn"  -- made of keratin, unlike real horns
  , ifreq    = [(RHINO_HORN, 1)]
  , icount   = 1  -- single, unlike real horns
  , iverbHit = "gore"
  , idamage  = 5 `d` 1
  , iaspects = [Timeout 5, AddSkill SkHurtMelee 20]
               ++ iaspects fist
  , ieffects = [Impress, Yell]  -- the owner is a mid-boss, after all
  , idesc    = "Very solid, considering it has the same composition as fingernails."
  }
tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [(TENTACLE, 1)]
  , icount   = 4
  , iverbHit = "slap"
  , idamage  = 4 `d` 1
  , idesc    = "Damp and dextrous."
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [(THORN, 1)]
  , icount   = 2 + 1 `d` 3
  , iverbHit = "puncture"
  , idamage  = 2 `d` 1
  , iaspects = [SetFlag Meleeable]  -- not Durable
  , ieffects = [VerbNoLonger "be not so thorny any more"]
  , idesc    = "Sharp yet brittle."
  }
boilingFissure = fist
  { iname    = "fissure"
  , ifreq    = [(BOILING_FISSURE, 1)]
  , icount   = 5 + 1 `d` 5
  , iverbHit = "hiss at"
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee 20  -- decreasing as count decreases
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [ DropItem 1 1 COrgan CONDITION  -- useful; limited
               , VerbNoLonger "widen the crack, releasing pressure" ]
  , idesc    = ""
  }
arsenicFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(BIOGAS_FISSURE, 1)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 2 `d` 1
  , ieffects = [ toOrganBad PARSIMONIOUS (5 + 1 `d` 3)
               -- weaken/poison, impacting intellectual abilities first
               , VerbNoLonger "become clogged with organic residue" ]
  , idesc    = ""
  }
sulfurFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [(MEDBOT_FISSUE, 1)]
  , icount   = 2 + 1 `d` 2
  , idamage  = 0  -- heal not via (negative) idamage, for armour would block it
  , iaspects = SetFlag Benign : iaspects boilingFissure
  , ieffects = [ RefillHP 5
               , toOrganNoTimer HUNGRY  -- the metabolic price to pay
               , VerbNoLonger "run out of nano medbot liquid" ]
  , idesc    = ""
  }
beeSting = fist
  { iname    = "bee sting"
  , ifreq    = [(BEE_STING, 1)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee 200, AddSkill SkArmorRanged 45
               , SetFlag Meleeable ]  -- not Durable
  , ieffects = [Paralyze 6, RefillHP 4]
                 -- no special message when runs out, because it's 1 copy
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [(STING, 1)]
  , icount   = 1
  , iverbHit = "inject"
  , idamage  = 1 `d` 1
  , iaspects = [Timeout $ 10 - 1 `dL` 4, AddSkill SkHurtMelee 40]
               ++ iaspects fist
  , ieffects = [toOrganBad RETAINING (3 + 1 `d` 3)]
  , idesc    = "Painful, debilitating and harmful."
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [(VENOM_TOOTH, 1)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 1 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [toOrganBad SLOWED (3 + 1 `d` 3)]
  , idesc    = "A chilling numbness spreads from its bite."
  }
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [(VENOM_FANG, 1)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 0
  , iaspects = Timeout (10 - 1 `dL` 5)
               : iaspects fist
  , ieffects = [toOrganNoTimer POISONED]
  , idesc    = "Dripping with deadly venom."
  }
screechingBeak = fist
  { iname    = "screeching beak"
  , ifreq    = [(SCREECHING_BEAK, 1)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = 3 `d` 1
  , iaspects = Timeout (7 - 1 `dL` 3)
               : iaspects fist
  , ieffects = [Summon SCAVENGER $ 1 `dL` 3]
  , idesc    = "Both a weapon and a beacon, calling more scavengers to the meal."
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [(LARGE_TAIL, 1)]
  , icount   = 1
  , iverbHit = "knock"
  , idamage  = 7 `d` 1
  , iaspects = [Timeout $ 2 + 1 `d` 2, AddSkill SkHurtMelee 20]
               ++ iaspects fist
                 -- timeout higher, lest they regain push before closing again
  , ieffects = [PushActor (ThrowMod 200 50 1)]  -- 1 step, fast
  , idesc    = "Almost as long as the trunk."
  }
hugeTail = largeTail
  { iname    = "huge tail"
  , ifreq    = [(HUGE_TAIL, 1)]
  , iverbHit = "upend"
  , iaspects = [Timeout $ 3 + 1 `d` 2, AddSkill SkHurtMelee 20]
               ++ iaspects fist
                 -- timeout higher, lest they regain push before closing again
  , ieffects = [PushActor (ThrowMod 400 50 1)]  -- 2 steps, fast
  , idesc    = "Slow but immensely heavy."
  }

-- Non-weapons

-- * Armor organs

armoredSkin = ItemKind
  { isymbol  = ','
  , iname    = "armored skin"
  , ifreq    = [(ARMORED_SKIN, 1)]
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
  , ifreq    = [(BARK, 1)]
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
eye3 = eye 3 EYE_3
eye6 = eye 6 EYE_6
eye8 = eye 8 EYE_8
vision :: Int -> GroupName ItemKind -> ItemKind
vision n grp = armoredSkin
  { iname    = "vision"
  , ifreq    = [(grp, 1)]
  , iverbHit = "visualize"
  , iaspects = [ AddSkill SkSight (intToDice n)
               , SetFlag Durable ]
  , idesc    = ""
  }
vision6 = vision 6 VISION_6
vision12 = vision 12 VISION_12
vision16 = vision 16 VISION_16
nostril = armoredSkin
  { iname    = "nostril"
  , ifreq    = [(NOSTRIL, 1)]
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
ear3 = ear 3 EAR_3
ear6 = ear 6 EAR_6
ear8 = ear 8 EAR_8

-- * Assorted

rattleOrgan = armoredSkin
  { iname    = "rattle"
  , ifreq    = [(RATLLE, 1)]
  , iverbHit = "announce"
  , iaspects = [ Timeout $ 10 + (1 `d` 3) * 10  -- long, to limit spam
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [Yell, RefillCalm 5]
  , idesc    = ""
  }
animalStomach = armoredSkin
  { iname    = "animal stomach"
  , ifreq    = [(ANIMAL_STOMACH, 1)]
  , iverbHit = "burp"
  , iaspects = [ Timeout $ 500 + (1 `d` 3) * 50  -- hunger very slowly
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [toOrganNoTimer HUNGRY]
  , idesc    = ""
  }
insectMortality = armoredSkin
  { iname    = "insect mortality"
  , ifreq    = [(INSECT_MORTALITY, 1)]
  , iverbHit = "age"
  , iaspects = [ AddSkill SkAggression 2  -- try to attack before you die
               , Timeout $ 30 + (1 `d` 3) * 10  -- die very slowly
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP (-1), Yell]
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [(SAPIENT_BRAIN, 1)]
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
  , ifreq    = [(ANIMAL_BRAIN, 1)]
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
  { iname    = "speed gland"
  , ifreq    = [(grp, 1)]
  , iverbHit = "spit at"
  , iaspects = [ AddSkill SkSpeed $ intToDice n
               , Timeout $ intToDice (100 `div` n)
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 1]
  , idesc    = ""
  }
speedGland5 = speedGland 5 SPEED_GLAND_5
speedGland10 = speedGland 10 SPEED_GLAND_10
scentGland = armoredSkin
  { iname    = "scent gland"
  , ifreq    = [(SCENT_GLAND, 1)]
  , icount   = 10 + 1 `d` 3  -- runs out
  , iverbHit = "spray at"
  , iaspects = [ Timeout $ (1 `d` 3) * 10
               , SetFlag Periodic, SetFlag Fragile ]  -- not Durable
  , ieffects = [ VerbNoLonger "look spent"
               , ApplyPerfume
               , Explode DISTRESSING_ODOR ]
                   -- keep explosion at the end to avoid the ambiguity of
                   -- "of ([foo explosion] of [bar])"
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [(BOILING_VENT, 1)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode BOILING_WATER]
  , idesc    = ""
  }
arsenicVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [(BIOGAS_VENT, 1)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode SPARSE_SHOWER]
  , idesc    = ""
  }
sulfurVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [(MEDBOT_VENT, 1)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [ Timeout $ (2 + 1 `d` 3) * 5
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [RefillHP 2, Explode DENSE_SHOWER]
  , idesc    = ""
  }

-- * Special

bonusHP = armoredSkin
  { isymbol  = 'H'  -- '+' reserved for conditions
  , iname    = "bonus HP"
  , ifreq    = [(S_BONUS_HP, 1)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddSkill SkMaxHP 1]
  , idesc    = "Special training and connections in the right places give this adventurer reinforced musculature and augmented internal organs, much more resilient to damage."
  }
braced = armoredSkin
  { isymbol  = 'B'
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
  { isymbol  = 'S'
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
  { isymbol  = 'I'
  , iname    = "impressed"
  , ifreq    = [(S_IMPRESSED, 1), (CONDITION, 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxCalm (-1)  -- to help player notice on HUD
                                          -- and to count as bad condition
               , SetFlag Fragile  -- to announce "no longer" only when
                                  -- all copies gone
               , SetFlag Condition ]  -- this is really a condition,
                                      -- just not a timed condition
  , ieffects = [ OnSmash $ verbMsgLess "impressed"
               , OnSmash $ verbMsgNoLonger "impressed" ]
                   -- not periodic, so no wear each turn, so only @OnSmash@
  , idesc    = "Being impressed by one's adversary sounds like fun, but on battlefield it equals treason. Almost. Throw in depleted battle calm and it leads to mindless desertion outright."
  }

-- * Allure-specific

hungry = armoredSkin
  { isymbol  = 'U'
  , iname    = "hungry"
  , ifreq    = [(HUNGRY, 1)]
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
smallBeak = fist
  { iname    = "small beak"
  , ifreq    = [(SMALL_BEAK, 1)]
  , icount   = 1
  , iverbHit = "nom"
  , idamage  = 2 `d` 1
  , idesc    = "Cute, but painful."
  }
razor = fist
  { iname    = "razor edge"
  , ifreq    = [(RAZOR, 1)]
  , icount   = 1 + 1 `d` 2
  , iverbHit = "slice"
  , idamage  = 2 `d` 1
  , iaspects = [SetFlag Meleeable]  -- not Durable
  , ieffects = [ toOrganBad WEAKENED (2 + 1 `dL` 3)
               , VerbNoLonger "lose all sharpness" ]
                 -- we interpret charges as sharpness of the actor or his razor'
                 -- no pronoun in the message to avoid "you lose its sharpness"
  , idesc    = ""
  }
liveWire = fist
  { iname    = "live wire"
  , ifreq    = [(LIVE_WIRE, 1)]
  , icount   = 1
  , iverbHit = "shock"
  , idamage  = 0
  , iaspects = [ Timeout $ 3 + 1 `d` 2
               , AddSkill SkHurtMelee 20 ]
               ++ iaspects fist
  , ieffects = [ toOrganBad IMMOBILE (3 + 1 `d` 3)
               , RefillHP (-1) ]
  , idesc    = ""
  }
robotBrain = armoredSkin
  { iname    = "robot brain"
  , ifreq    = [(ROBOT_BRAIN, 1)]
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
  , ifreq    = [(HULL_PLATING, 1)]
  , idesc    = ""
  }
mouthVent = armoredSkin
  { iname    = "mouth vent"
  , ifreq    = [(MOUTH_VENT, 1)]
  , iflavour = zipPlain [BrMagenta]
  , iverbHit = "surprise"
  , iaspects = [ Timeout 7
               , SetFlag Periodic, SetFlag Durable ]
  , ieffects = [OneOf $ map Explode
      [PHEROMONE, RHINO_HOLOGRAM, blastNoStatOf IMMOBILE, SMOKE, SPARK]]
  , idesc    = ""
  }
-- HP change varies due to body size
geneticFlaw :: Int -> GroupName ItemKind -> ItemKind
geneticFlaw n grp = armoredSkin
  { isymbol  = 'F'
  , iname    = "genetic flaw"
  , ifreq    = [(GENETIC_FLAW, 1), (grp, 1)]
  , iflavour = zipPlain [BrRed]
  , iverbHit = "flaw"
  , iweight  = 0
  , iaspects = [ AddSkill SkMaxHP (intToDice $ - n)
               , Odds (1 `d` 400) [AddSkill SkArmorMelee (-20)]
                                  [AddSkill SkArmorRanged (-10)]
               , SetFlag Condition ]
                   -- destroy on drop to run the @OnSmash@ effects
               ++ [AddSkill SkWait (-1) | n >= 10]
               ++ [AddSkill SkApply (-1) | n >= 10]
  , ieffects = [ OnSmash $ DropItem maxBound maxBound COrgan CONDITION
                   -- key for AI is it eliminates all impression conditions
               , OnSmash $ DropItem maxBound maxBound COrgan HUNGRY
               , OnSmash $ RefillHP n
               , OnSmash $ VerbNoLonger "undergo instant infracellular decontamination" ]  -- unlike the civilian version, this one is instant and the attunement is automatic and relatively quick (the usual double cooldown when equipping items again)
  , idesc    = "Nobody is perfect. At least without infracellular engineering, which is heavily regulated, insanely expensive and automatically reverted without refund before critical medical interventions. One more reason to be a good citizen, work hard and not die often. But where is the fun in that?"
  }
geneticFlaw3 = geneticFlaw 3 GENETIC_FLAW_3
geneticFlaw10 = geneticFlaw 10 GENETIC_FLAW_10
