-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Organ definitions.
module Content.ItemKindOrgan
  ( organs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

organs :: [ItemKind]
organs =
  [fist, foot, claw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, razor, thorn, boilingFissure, biogasFissure, medbotFissure, insectMortality, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, liveWire, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision6, vision8, vision10, vision12, vision14, vision16, nostril, sapientBrain, animalBrain, robotBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, biogasVent, medbotVent, wasteContainer, spotlight, bonusHP]

fist,    foot, claw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, razor, thorn, boilingFissure, biogasFissure, medbotFissure, insectMortality, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, liveWire, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision6, vision8, vision10, vision12, vision14, vision16, nostril, sapientBrain, animalBrain, robotBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, biogasVent, medbotVent, wasteContainer, spotlight, bonusHP :: ItemKind

-- Weapons

-- * Human weapon organs

fist = ItemKind
  { isymbol  = '%'
  , iname    = "fist"
  , ifreq    = [("fist", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 2
  , irarity  = [(1, 1)]
  , iverbHit = "punch"
  , iweight  = 2000
  , idamage  = toDmg $ 4 * d 1
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Identified, Meleeable]
  , idesc    = ""
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [("foot", 50)]
  , iverbHit = "kick"
  , idamage  = toDmg $ 4 * d 1
  , idesc    = ""
  }

-- * Universal weapon organs

claw = fist
  { iname    = "claw"
  , ifreq    = [("claw", 50)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , idamage  = toDmg $ 2 * d 1
  , iaspects = [Timeout $ 4 + d 4]
  , ieffects = [Recharging (toOrganGameTurn "slow 10" 2)]
  , idesc    = ""
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [("small claw", 50)]
  , iverbHit = "slash"
  , idamage  = toDmg $ 2 * d 1
  , idesc    = ""
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [("snout", 10)]
  , icount   = 1
  , iverbHit = "bite"
  , idamage  = toDmg $ 2 * d 1
  , idesc    = ""
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [("small jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = toDmg $ 3 * d 1
  , idesc    = ""
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , idamage  = toDmg $ 5 * d 1
  , idesc    = ""
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [("large jaw", 100)]
  , icount   = 1
  , iverbHit = "crush"
  , idamage  = toDmg $ 12 * d 1
  , idesc    = ""
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [("horn", 20)]
  , icount   = 2
  , iverbHit = "impale"
  , idamage  = toDmg $ 8 * d 1
  , idesc    = ""
  }

-- * Monster weapon organs

tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [("tentacle", 50)]
  , icount   = 4
  , iverbHit = "slap"
  , idamage  = toDmg $ 4 * d 1
  , idesc    = ""
  }

-- * Special weapon organs

razor = fist
  { iname    = "razor"
  , ifreq    = [("razor", 100)]
  , icount   = 2 + d 5
  , iverbHit = "slice"
  , idamage  = toDmg $ 2 * d 1
  , idesc    = ""
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [("thorn", 100)]
  , icount   = 2 + d 3
  , iverbHit = "impale"
  , idamage  = toDmg $ 2 * d 1
  , ifeature = [Identified, Meleeable]  -- not Durable
  , idesc    = ""
  }
boilingFissure = fist
  { iname    = "fissure"
  , ifreq    = [("boiling fissure", 100)]
  , icount   = 5 + d 5
  , iverbHit = "hiss at"
  , idamage  = toDmg 0
  , ieffects = [Burn $ 1 * d 1]
  , ifeature = [Identified, Meleeable]  -- not Durable
  , idesc    = ""
  }
biogasFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("biogas fissure", 100)]
  , icount   = 2 + d 2
  , ieffects = [Burn $ 1 * d 1, toOrganGameTurn "weakened" (2 + d 2)]
  }
medbotFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("medbot fissure", 100)]
  , icount   = 2 + d 2
  , ieffects = [Burn $ 1 * d 1, RefillHP 6]
  }
beeSting = fist
  { iname    = "bee sting"
  , ifreq    = [("bee sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = toDmg 0
  , iaspects = [AddArmorMelee 90, AddArmorRanged 90]
  , ieffects = [Burn $ 2 * d 1, Paralyze 6, RefillHP 5]
  , ifeature = [Identified, Meleeable]  -- not Durable
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [("sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , idamage  = toDmg 0
  , iaspects = [Timeout $ 1 + d 5]
  , ieffects = [Burn $ 2 * d 1, Recharging (Paralyze 4)]
  , idesc    = "Painful, debilitating and harmful."
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = toDmg $ 2 * d 1
  , iaspects = [Timeout $ 5 + d 3]
  , ieffects = [Recharging (toOrganGameTurn "slow 10" (3 + d 3))]
  , idesc    = ""
  }
-- TODO: should also confer poison resistance, but current implementation
-- is too costly (poison removal each turn)
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , idamage  = toDmg $ 2 * d 1
  , iaspects = [Timeout $ 7 + d 5]
  , ieffects = [Recharging (toOrganNone "poisoned")]
  , idesc    = ""
  }
screechingBeak = fist
  { iname    = "screeching beak"
  , ifreq    = [("screeching beak", 100)]
  , icount   = 1
  , iverbHit = "peck"
  , idamage  = toDmg $ 2 * d 1
  , iaspects = [Timeout $ 5 + d 5]
  , ieffects = [Recharging (Summon [("scavenger", 1)] $ 1 + dl 2)]
  , idesc    = ""
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [("large tail", 50)]
  , icount   = 1
  , iverbHit = "knock"
  , idamage  = toDmg $ 8 * d 1
  , iaspects = [Timeout $ 1 + d 3]
  , ieffects = [Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = ""
  }
liveWire = fist
  { iname    = "live wire"
  , ifreq    = [("live wire", 100)]
  , icount   = 1
  , iverbHit = "shock"
  , idamage  = toDmg $ 1 * d 1
  , iaspects = [Timeout $ 3 + d 3]
  , ieffects = [ Recharging (DropItem COrgan "temporary conditions")
               , Recharging $ RefillHP (-2)
               ]
  , idesc    = ""
  }

-- Non-weapons

-- * Armor organs

armoredSkin = ItemKind
  { isymbol  = '%'
  , iname    = "armored skin"
  , ifreq    = [("armored skin", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = toDmg 0
  , iaspects = [AddArmorMelee 30, AddArmorRanged 30]
  , ieffects = []
  , ifeature = [Durable, Identified]
  , idesc    = ""
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
  , idesc    = ""
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
vision6 = vision 6
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
  , iaspects = [Timeout $ 40 + d 10]
  , ieffects = [Periodic, Recharging (RefillHP (-1))]
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
robotBrain = armoredSkin
  { iname    = "robot brain"
  , ifreq    = [("robot brain", 100)]
  , iverbHit = "outcompute"
  , iaspects = [AddAbility ab 1 | ab <- [minBound..maxBound]]
               ++ [AddAbility AbApply (-1)]
  , idesc    = ""
  }
speedGland :: Int -> ItemKind
speedGland n = armoredSkin
  { iname    = "speed gland"
  , ifreq    = [(toGroupName $ "speed gland" <+> tshow n, 100)]
  , iverbHit = "spit at"
  , iaspects = [ AddSpeed $ intToDice n
               , Timeout $ intToDice $ 100 `div` n ]
  , ieffects = [Periodic, Recharging (RefillHP 1)]
  , idesc    = ""
  }
speedGland2 = speedGland 2
speedGland4 = speedGland 4
speedGland6 = speedGland 6
speedGland8 = speedGland 8
speedGland10 = speedGland 10
scentGland = armoredSkin  -- TODO: cone attack, 3m away, project? apply?
  { iname    = "scent gland"
  , ifreq    = [("scent gland", 100)]
  , iverbHit = "spray at"
  , iaspects = [Timeout $ 10 + d 2 |*| 5 ]
  , ieffects = [ Periodic, Recharging (Explode "distressing odor")
               , Recharging ApplyPerfume ]
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("boiling vent", 100)]
  , iflavour = zipPlain [BrBlue]
  , iverbHit = "menace"
  , iaspects = [Timeout $ 2 + d 2 |*| 5]
  , ieffects = [Periodic, Recharging (Explode "boiling water")]
  , idesc    = ""
  }
biogasVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("biogas vent", 100)]
  , iflavour = zipPlain [BrGreen]
  , iverbHit = "menace"
  , iaspects = [Timeout $ 2 + d 2 |*| 5]
  , ieffects = [Periodic, Recharging (Explode "weakness mist")]
  , idesc    = ""
  }
medbotVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("medbot vent", 100)]
  , iflavour = zipPlain [BrYellow]
  , iverbHit = "menace"
  , iaspects = [Timeout $ 2 + d 2 |*| 5]
  , ieffects = [Periodic, Recharging (Explode "protecting balm")]
  , idesc    = ""
  }
wasteContainer = armoredSkin
  { iname    = "waste container"
  , ifreq    = [("waste container", 100)]
  , iverbHit = "spill over"
  , iaspects = [Timeout $ 5 + d 5 |*| 10]
  , ieffects = [ Periodic
               , Recharging (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , Recharging (RefillHP 1)
               , Recharging (Explode "waste") ]
  , idesc    = ""
  }
spotlight = armoredSkin
  { iname    = "spotlight"
  , ifreq    = [("spotlight", 100)]
  , iverbHit = "blind"
  , iaspects = [AddShine 3]
  , idesc    = ""
  }
bonusHP = armoredSkin
  { isymbol  = '+'
  , iname    = "bonus HP"
  , ifreq    = [("bonus HP", 100)]
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddMaxHP 1]
  , idesc    = ""
  }
