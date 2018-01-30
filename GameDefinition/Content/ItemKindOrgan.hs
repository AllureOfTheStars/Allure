-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Actor organ definitions.
module Content.ItemKindOrgan
  ( organs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.ItemAspect (Aspect (..))
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

organs :: [ItemKind]
organs =
  [fist, foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision5, vision6, vision7, vision8, vision10, vision12, vision14, vision16, nostril, insectMortality, sapientBrain, animalBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, impressed]
  -- Allure-specific
  ++ [razor, liveWire, robotBrain, wasteContainer, spotlight]

fist,    foot, hookedClaw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, thorn, boilingFissure, arsenicFissure, sulfurFissure, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision5, vision6, vision7, vision8, vision10, vision12, vision14, vision16, nostril, insectMortality, sapientBrain, animalBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, arsenicVent, sulfurVent, bonusHP, impressed :: ItemKind
-- Allure-specific
razor, liveWire, robotBrain, wasteContainer, spotlight :: ItemKind

-- Weapons

-- * Human weapon organs

fist = ItemKind
  { isymbol  = ','
  , iname    = "fist"
  , ifreq    = [("fist", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 2
  , irarity  = [(1, 1)]
  , iverbHit = "punch"
  , iweight  = 2000
  , idamage  = 4 `d` 1
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Meleeable]
  , idesc    = "Simple but effective."
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [("foot", 50)]
  , iverbHit = "kick"
  , idamage  = 4 `d` 1
  , idesc    = "A weapon you can still use if disarmed."
  }

-- * Universal weapon organs

hookedClaw = fist
  { iname    = "hooked claw"
  , ifreq    = [("hooked claw", 50)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , idamage  = 2 `d` 1
  , iaspects = [Timeout $ 10 - 1 `dL` 5]
  , ieffects = [Recharging (toOrganGameTurn "slowed" 2)]
  , idesc    = "A curved talon."
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [("small claw", 50)]
  , iverbHit = "slash"
  , idamage  = 2 `d` 1
  , idesc    = "A pearly spike."
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [("snout", 10)]
  , icount   = 1
  , iverbHit = "bite"
  , idamage  = 2 `d` 1
  , idesc    = "Sensitive and wide-nostrilled."
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [("small jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 3 `d` 1
  , idesc    = "Filled with small, even teeth."
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = 5 `d` 1
  , idesc    = "Delivers a powerful bite."
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [("large jaw", 100)]
  , icount   = 1
  , iverbHit = "crush"
  , idamage  = 10 `d` 1
  , idesc    = "Enough to swallow anything in a single gulp."
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [("horn", 20)]
  , icount   = 2
  , iverbHit = "impale"
  , idamage  = 6 `d` 1
  , iaspects = [AddHurtMelee 20]
  , idesc    = "Sharp and solid, for defence or attack."
  }

-- * Special weapon organs

tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [("tentacle", 50)]
  , icount   = 4
  , iverbHit = "slap"
  , idamage  = 4 `d` 1
  , idesc    = "Damp and dextrous."
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [("thorn", 100)]
  , icount   = 2 + 1 `d` 3
  , iverbHit = "impale"
  , idamage  = 1 `d` 3
  , ifeature = [Meleeable]  -- not Durable
  , idesc    = "Sharp yet brittle."
  }
boilingFissure = fist
  { iname    = "fissure"
  , ifreq    = [("boiling fissure", 100)]
  , icount   = 5 + 1 `d` 5
  , iverbHit = "hiss at"
  , idamage  = 1 `d` 1
  , iaspects = [AddHurtMelee 20]  -- decreasing as count decreases
  , ieffects = [InsertMove $ 1 `d` 3]
  , ifeature = [Meleeable]  -- not Durable
  , idesc    = ""
  }
arsenicFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("biogas fissure", 100)]
  , icount   = 3 + 1 `d` 3
  , idamage  = 2 `d` 1
  , ieffects = [toOrganGameTurn "weakened" (2 + 1 `dL` 3)]
  , idesc    = ""
  }
sulfurFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("medbot fissure", 100)]
  , icount   = 2 + 1 `d` 2
  , idamage  = 0
  , ieffects = [RefillHP 5]
  , idesc    = ""
  }
beeSting = fist
  { iname    = "bee sting"
  , ifreq    = [("bee sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = 0
  , iaspects = [AddArmorMelee 90, AddArmorRanged 45]
  , ieffects = [Paralyze 6, RefillHP 4]
  , ifeature = [Meleeable]  -- not Durable
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [("sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = 1 `d` 1
  , iaspects = [Timeout $ 10 - 1 `dL` 5, AddHurtMelee 40]
  , ieffects = [Recharging (Paralyze 4)]
  , idesc    = "Painful, debilitating and harmful."
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 2 `d` 1
  , iaspects = [Timeout $ 7 - 1 `dL` 3]
  , ieffects = [Recharging (toOrganGameTurn "slowed" (3 + 1 `d` 3))]
  , idesc    = "A chilling numbness spreads from its bite."
  }
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = 2 `d` 1
  , iaspects = [Timeout $ 10 - 1 `dL` 5]
  , ieffects = [Recharging (toOrganNone "poisoned")]
  , idesc    = "Dripping with deadly venom."
  }
screechingBeak = fist
  { iname    = "screeching beak"
  , ifreq    = [("screeching beak", 100)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = 2 `d` 1
  , iaspects = [Timeout $ 7 - 1 `dL` 3]
  , ieffects = [Recharging $ Summon "scavenger" $ 1 `dL` 3]
  , idesc    = "Both a weapon and a beacon, calling more scavengers to the meal."
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [("large tail", 50)]
  , icount   = 1
  , iverbHit = "knock"
  , idamage  = 6 `d` 1
  , iaspects = [Timeout $ 1 + 1 `d` 3, AddHurtMelee 20]
  , ieffects = [Recharging (PushActor (ThrowMod 400 50))]  -- 2 steps
  , idesc    = "Slow but heavy."
  }

-- Non-weapons

-- * Armor organs

armoredSkin = ItemKind
  { isymbol  = ','
  , iname    = "armored skin"
  , ifreq    = [("armored skin", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [AddArmorMelee 30, AddArmorRanged 15]
  , ieffects = []
  , ifeature = [Durable]
  , idesc    = "Homemade armour is just as good."
  , ikit     = []
  }

-- * Sense organs

eye :: Int -> ItemKind
eye n = armoredSkin
  { iname    = "eye"
  , ifreq    = [(toGroupName $ "eye" <+> tshow n, 100)]
  , icount   = 2
  , iverbHit = "glare at"
  , iaspects = [AddSight (intToDice n)]
  , idesc    = "A piercing stare."
  }
eye2 = eye 2
eye3 = eye 3
eye4 = eye 4
eye5 = eye 5
eye6 = eye 6
eye7 = eye 7
eye8 = eye 8
vision :: Int -> ItemKind
vision n = armoredSkin
  { iname    = "vision"
  , ifreq    = [(toGroupName $ "vision" <+> tshow n, 100)]
  , iverbHit = "visualize"
  , iaspects = [AddSight (intToDice n)]
  , idesc    = ""
  }
vision4 = vision 4
vision5 = vision 5
vision6 = vision 6
vision7 = vision 7
vision8 = vision 8
vision10 = vision 10
vision12 = vision 12
vision14 = vision 14
vision16 = vision 16
nostril = armoredSkin
  { iname    = "nostril"
  , ifreq    = [("nostril", 100)]
  , icount   = 2
  , iverbHit = "snuff"
  , iaspects = [AddSmell 1]  -- times 2, from icount
  , idesc    = ""
  }

-- * Assorted

insectMortality = armoredSkin
  { iname    = "insect mortality"
  , ifreq    = [("insect mortality", 100)]
  , iverbHit = "age"
  , iaspects = [Timeout $ 30 + (1 `d` 2) * 10]
  , ieffects = [Recharging (RefillHP (-1))]
  , ifeature = [Periodic] ++ ifeature armoredSkin
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [("sapient brain", 100)]
  , iverbHit = "outbrain"
  , iaspects = [AddAbility ab 1 | ab <- [minBound..maxBound]]
               ++ [AddAbility AbAlter 2]  -- can use stairs
  , idesc    = ""
  }
animalBrain = armoredSkin
  { iname    = "animal brain"
  , ifreq    = [("animal brain", 100)]
  , iverbHit = "blank"
  , iaspects = [AddAbility ab 1 | ab <- [minBound..maxBound]]
               ++ [AddAbility AbAlter 2]  -- can use stairs
               ++ [ AddAbility ab (-1)
                  | ab <- [AbDisplace, AbMoveItem, AbProject, AbApply] ]
  , idesc    = ""
  }
speedGland :: Int -> ItemKind
speedGland n = armoredSkin
  { iname    = "speed gland"
  , ifreq    = [(toGroupName $ "speed gland" <+> tshow n, 100)]
  , iverbHit = "spit at"
  , iaspects = [ AddSpeed $ intToDice n
               , Timeout $ intToDice $ 100 `div` n ]
  , ieffects = [Recharging (RefillHP 1)]
  , ifeature = [Periodic] ++ ifeature armoredSkin
  , idesc    = ""
  }
speedGland2 = speedGland 2
speedGland4 = speedGland 4
speedGland6 = speedGland 6
speedGland8 = speedGland 8
speedGland10 = speedGland 10
scentGland = armoredSkin
  { iname    = "scent gland"
  , ifreq    = [("scent gland", 100)]
  , icount   = 2 + 1 `d` 3  -- runs out
  , iverbHit = "spray at"
  , iaspects = [Timeout $ (1 `d` 3) * 10]
  , ieffects = [ Recharging (Temporary "look spent")
               , Recharging (Explode "distressing odor")
               , Recharging ApplyPerfume ]
  , ifeature = [Periodic]  -- not Durable
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("boiling vent", 100)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "menace"
  , iaspects = [Timeout $ (2 + 1 `d` 2) * 5]
  , ieffects = [ Recharging (Explode "boiling water")
               , Recharging (RefillHP 2) ]
  , ifeature = [Periodic] ++ ifeature armoredSkin
  , idesc    = ""
  }
arsenicVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("biogas vent", 100)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "menace"
  , iaspects = [Timeout $ (2 + 1 `d` 2) * 5]
  , ieffects = [ Recharging (Explode "sparse shower")
               , Recharging (RefillHP 2) ]
  , ifeature = [Periodic] ++ ifeature armoredSkin
  , idesc    = ""
  }
sulfurVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("medbot vent", 100)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [Timeout $ (2 + 1 `d` 2) * 5]
  , ieffects = [ Recharging (Explode "dense shower")
               , Recharging (RefillHP 2) ]
  , ifeature = [Periodic] ++ ifeature armoredSkin
  , idesc    = ""
  }

-- * Special

bonusHP = armoredSkin
  { isymbol  = 'H'  -- '+' reserved for temporary conditions
  , iname    = "bonus HP"
  , iflavour = zipPlain [BrBlue]
  , ifreq    = [("bonus HP", 1)]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddMaxHP 1]
  , idesc    = ""
  }
impressed = armoredSkin
  { isymbol  = '!'
  , iname    = "impressed"
  , iflavour = zipPlain [BrRed]
  , ifreq    = [("impressed", 1), ("temporary condition", 1)]
  , iverbHit = "confuse"
  , iweight  = 0
  , iaspects = [AddMaxCalm (-1)]  -- to help player notice on main screen
  , ieffects = [OnSmash $ tmpNoLonger "impressed"]  -- not @Periodic@
  , ifeature = [Fragile, Durable]  -- hack: destroy on drop
  , idesc    = ""
  }

-- * Allure-specific

razor = fist
  { iname    = "razor"
  , ifreq    = [("razor", 100)]
  , icount   = 2 + 1 `d` 5
  , iverbHit = "slice"
  , idamage  = 2 `d` 1
  , idesc    = ""
  }
liveWire = fist
  { iname    = "live wire"
  , ifreq    = [("live wire", 100)]
  , icount   = 1
  , iverbHit = "shock"
  , idamage  = 1 `d` 1
  , iaspects = [Timeout $ 3 + 1 `d` 2, AddHurtMelee 20]
  , ieffects = [ Recharging (DropItem 1 maxBound COrgan "temporary condition")
               , Recharging $ RefillHP (-2)
               ]
  , idesc    = ""
  }
robotBrain = armoredSkin
  { iname    = "robot brain"
  , ifreq    = [("robot brain", 100)]
  , iverbHit = "outcompute"
  , iaspects = [AddAbility ab 1 | ab <- [minBound..maxBound]]
               ++ [AddAbility AbApply (-1)]
  , idesc    = ""
  }
wasteContainer = armoredSkin
  { iname    = "waste container"
  , ifreq    = [("waste container", 100)]
  , iverbHit = "spill over"
  , iaspects = [Timeout $ (1 + 1 `d` 2) * 30]
  , ieffects = [ Recharging (Summon "mobile animal" $ 1 `dL` 2)
               , Recharging (RefillHP 1)
               , Recharging (Explode "waste") ]
  , ifeature = [Periodic] ++ ifeature armoredSkin
  , idesc    = ""
  }
spotlight = armoredSkin
  { iname    = "spotlight"
  , ifreq    = [("spotlight", 100)]
  , iverbHit = "illuminate"
  , iaspects = [AddShine 3]
  , idesc    = ""
  }
