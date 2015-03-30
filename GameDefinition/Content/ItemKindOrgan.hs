-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Organ definitions.
module Content.ItemKindOrgan ( organs ) where

import qualified Data.EnumMap.Strict as EM

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

organs :: [ItemKind]
organs =
  [fist, foot, claw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, razor, thorn, boilingFissure, biogasFissure, medbotFissure, insectMortality, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, liveWire, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision6, vision8, vision10, vision12, vision14, vision16, nostril, sapientBrain, animalBrain, robotBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, explosionVent, medbotVent, wasteContainer, spotlight, bonusHP]

fist,    foot, claw, smallClaw, snout, smallJaw, jaw, largeJaw, horn, tentacle, razor, thorn, boilingFissure, biogasFissure, medbotFissure, insectMortality, beeSting, sting, venomTooth, venomFang, screechingBeak, largeTail, liveWire, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision6, vision8, vision10, vision12, vision14, vision16, nostril, sapientBrain, animalBrain, robotBrain, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, scentGland, boilingVent, explosionVent, medbotVent, wasteContainer, spotlight, bonusHP :: ItemKind

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
  , iaspects = []
  , ieffects = [Hurt (4 * d 1)]
  , ifeature = [Durable, Identified]
  , idesc    = ""
  , ikit     = []
  }
foot = fist
  { iname    = "foot"
  , ifreq    = [("foot", 50)]
  , icount   = 2
  , iverbHit = "kick"
  , ieffects = [Hurt (4 * d 1)]
  , idesc    = ""
  }

-- * Universal weapon organs

claw = fist
  { iname    = "claw"
  , ifreq    = [("claw", 50)]
  , icount   = 2  -- even if more, only the fore claws used for fighting
  , iverbHit = "hook"
  , iaspects = [Timeout $ 4 + d 4]
  , ieffects = [Hurt (4 * d 1), Recharging (toOrganGameTurn "slow 10" 2)]
  , idesc    = ""
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [("small claw", 50)]
  , icount   = 2
  , iverbHit = "hook"
  , iaspects = [Timeout $ 4 + d 4]
  , ieffects = [Hurt (2 * d 1), Recharging (toOrganGameTurn "slow 10" 1)]
  , idesc    = ""
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [("snout", 10)]
  , iverbHit = "bite"
  , ieffects = [Hurt (2 * d 1)]
  , idesc    = ""
  }
smallJaw = fist
  { iname    = "small jaw"
  , ifreq    = [("small jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , ieffects = [Hurt (3 * d 1)]
  , idesc    = ""
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , ieffects = [Hurt (5 * d 1)]
  , idesc    = ""
  }
largeJaw = fist
  { iname    = "large jaw"
  , ifreq    = [("large jaw", 100)]
  , icount   = 1
  , iverbHit = "crush"
  , ieffects = [Hurt (12 * d 1)]
  , idesc    = ""
  }
horn = fist
  { iname    = "horn"
  , ifreq    = [("horn", 20)]
  , icount   = 2
  , iverbHit = "impale"
  , ieffects = [Hurt (8 * d 1)]
  , idesc    = ""
  }

-- * Monster weapon organs

tentacle = fist
  { iname    = "tentacle"
  , ifreq    = [("tentacle", 50)]
  , icount   = 4
  , iverbHit = "slap"
  , ieffects = [Hurt (4 * d 1)]
  , idesc    = ""
  }

-- * Special weapon organs

razor = fist
  { iname    = "razor"
  , ifreq    = [("razor", 100)]
  , icount   = 7
  , iverbHit = "slice"
  , ieffects = [Hurt (2 * d 1)]
  , idesc    = ""
  }
thorn = fist
  { iname    = "thorn"
  , ifreq    = [("thorn", 100)]
  , icount   = 2 + d 3
  , iverbHit = "impale"
  , ieffects = [Hurt (2 * d 1)]
  , ifeature = [Identified]  -- not Durable
  , idesc    = ""
  }
boilingFissure = fist
  { iname    = "fissure"
  , ifreq    = [("boiling fissure", 100)]
  , icount   = 5 + d 5
  , iverbHit = "hiss at"
  , ieffects = [Burn $ 1 * d 1]
  , ifeature = [Identified]  -- not Durable
  , idesc    = ""
  }
biogasFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("biogas fissure", 100)]
  , icount   = 2 + d 2
  , ieffects = [Hurt (1 * d 1), toOrganGameTurn "weakened" (2 + d 2)]
  }
medbotFissure = boilingFissure
  { iname    = "fissure"
  , ifreq    = [("medbot fissure", 100)]
  , icount   = 2 + d 2
  , ieffects = [Hurt (1 * d 1), RefillHP 6]
  }
beeSting = fist
  { iname    = "bee sting"
  , ifreq    = [("bee sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , iaspects = [AddArmorMelee 90, AddArmorRanged 90]
  , ieffects = [Burn $ 2 * d 1, Paralyze 10, RefillHP 5]
  , ifeature = [Identified]  -- not Durable
  , idesc    = "Painful, but beneficial."
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [("sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , iaspects = [Timeout $ 1 + d 5]
  , ieffects = [Burn $ 1 * d 1, Recharging (Paralyze 3)]
  , idesc    = "Painful, debilitating and harmful."
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , iaspects = [Timeout $ 5 + d 3]
  , ieffects = [ Hurt (2 * d 1)
               , Recharging (toOrganGameTurn "slow 10" (3 + d 3)) ]
  , idesc    = ""
  }
-- TODO: should also confer poison resistance, but current implementation
-- is too costly (poison removal each turn)
venomFang = fist
  { iname    = "venom fang"
  , ifreq    = [("venom fang", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , iaspects = [Timeout $ 7 + d 5]
  , ieffects = [ Hurt (2 * d 1)
               , Recharging (toOrganNone "poisoned") ]
  , idesc    = ""
  }
screechingBeak = armoredSkin
  { iname    = "screeching beak"
  , ifreq    = [("screeching beak", 100)]
  , icount   = 1
  , iverbHit = "peck"
  , iaspects = [Timeout $ 5 + d 5]
  , ieffects = [ Recharging (Summon [("scavenger", 1)] $ 1 + dl 2)
               , Hurt (2 * d 1)
               ]
  , idesc    = ""
  }
largeTail = fist
  { iname    = "large tail"
  , ifreq    = [("large tail", 50)]
  , icount   = 1
  , iverbHit = "knock"
  , iaspects = [Timeout $ 1 + d 3]
  , ieffects = [Hurt (8 * d 1), Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = ""
  }
liveWire = fist
  { iname    = "live wire"
  , ifreq    = [("live wire", 100)]
  , icount   = 2
  , iverbHit = "shock"
  , iaspects = [Timeout $ 5 + d 5]
  , ieffects = [ Hurt (1 * d 1)
               , Recharging (DropItem COrgan "temporary conditions" True)
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
  , icount   = 1
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
  , iaspects = [AddSmell 2]
  , idesc    = ""
  }

-- * Assorted

insectMortality = fist
  { iname    = "insect mortality"
  , ifreq    = [("insect mortality", 100)]
  , icount   = 1
  , iverbHit = "age"
  , iaspects = [Periodic, Timeout $ 40 + d 10]
  , ieffects = [Recharging (RefillHP (-1))]
  , idesc    = ""
  }
sapientBrain = armoredSkin
  { iname    = "sapient brain"
  , ifreq    = [("sapient brain", 100)]
  , icount   = 1
  , iverbHit = "outbrain"
  , iaspects = [AddSkills unitSkills]
  , idesc    = ""
  }
animalBrain = armoredSkin
  { iname    = "animal brain"
  , ifreq    = [("animal brain", 100)]
  , icount   = 1
  , iverbHit = "blank"
  , iaspects = [let absNo = [AbDisplace, AbMoveItem, AbProject, AbApply]
                    sk = EM.fromList $ zip absNo [-1, -1..]
                in AddSkills $ addSkills unitSkills sk]
  , idesc    = ""
  }
robotBrain = armoredSkin
  { iname    = "robot brain"
  , ifreq    = [("robot brain", 100)]
  , icount   = 1
  , iverbHit = "outcompute"
  , iaspects = [let absNo = [AbApply]
                    sk = EM.fromList $ zip absNo [-1, -1..]
                in AddSkills $ addSkills unitSkills sk]
  , idesc    = ""
  }
speedGland :: Int -> ItemKind
speedGland n = armoredSkin
  { iname    = "speed gland"
  , ifreq    = [(toGroupName $ "speed gland" <+> tshow n, 100)]
  , icount   = 1
  , iverbHit = "spit at"
  , iaspects = [ AddSpeed $ intToDice n
               , Periodic
               , Timeout $ intToDice $ 100 `div` n ]
  , ieffects = [Recharging (RefillHP 1)]
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
  , icount   = 2
  , iverbHit = "spray at"
  , iaspects = [Periodic, Timeout $ 10 + d 2 |*| 5 ]
  , ieffects = [ Recharging (Explode "distressing odor")
               , Recharging ApplyPerfume ]
  , idesc    = ""
  }
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("boiling vent", 100)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , iverbHit = "menace"
  , iaspects = [Periodic, Timeout $ 2 + d 2 |*| 5]
  , ieffects = [Recharging (Explode "boiling water")]
  , idesc    = ""
  }
explosionVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("explosion vent", 100), ("biogas vent", 100)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , iverbHit = "menace"
  , iaspects = [Periodic, Timeout $ 2 + d 2 |*| 5]
  , ieffects = [Recharging (Explode "blast 20")]
  , idesc    = ""
  }
medbotVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("medbot vent", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , iverbHit = "menace"
  , iaspects = [Periodic, Timeout $ 2 + d 2 |*| 5]
  , ieffects = [Recharging (Explode "protecting balm")]
  , idesc    = ""
  }
wasteContainer = armoredSkin
  { iname    = "waste container"
  , ifreq    = [("waste container", 100)]
  , icount   = 1
  , iverbHit = "spill over"
  , iaspects = [Periodic, Timeout $ 5 + d 5 |*| 10]
  , ieffects = [ Recharging (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , Recharging (RefillHP 1)
               , Recharging (Explode "waste") ]
  , idesc    = ""
  }
spotlight = armoredSkin
  { iname    = "spotlight"
  , ifreq    = [("spotlight", 100)]
  , icount   = 1
  , iverbHit = "blind"
  , iaspects = [AddLight 3]
  , idesc    = ""
  }
bonusHP = armoredSkin
  { iname    = "bonus HP"
  , ifreq    = [("bonus HP", 100)]
  , icount   = 1
  , iverbHit = "intimidate"
  , iweight  = 0
  , iaspects = [AddMaxHP 1]
  , idesc    = ""
  }
