-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Organ definitions.
module Content.ItemKindOrgan ( organs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

organs :: [ItemKind]
organs =
  [fist, foot, claw, smallClaw, snout, jaw, largeJaw, horn, tentacle, thorn, razor, fissure, sting, venomTooth, venomFang, largeTail, liveWire, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision6, vision8, vision10, vision12, vision14, vision16, nostril, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, boilingVent, explosionVent, wasteContainer, spotlight, bonusHP]

fist,    foot, claw, smallClaw, snout, jaw, largeJaw, horn, tentacle, thorn, razor, fissure, sting, venomTooth, venomFang, largeTail, liveWire, armoredSkin, eye2, eye3, eye4, eye5, eye6, eye7, eye8, vision4, vision6, vision8, vision10, vision12, vision14, vision16, nostril, speedGland2, speedGland4, speedGland6, speedGland8, speedGland10, boilingVent, explosionVent, wasteContainer, spotlight, bonusHP :: ItemKind

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
  , iverbHit = "slash"
  , ieffects = [Hurt (6 * d 1)]
  , idesc    = ""
  }
smallClaw = fist
  { iname    = "small claw"
  , ifreq    = [("small claw", 50)]
  , icount   = 2
  , iverbHit = "slash"
  , ieffects = [Hurt (3 * d 1)]
  , idesc    = ""
  }
snout = fist
  { iname    = "snout"
  , ifreq    = [("snout", 10)]
  , iverbHit = "bite"
  , ieffects = [Hurt (2 * d 1)]
  , idesc    = ""
  }
jaw = fist
  { iname    = "jaw"
  , ifreq    = [("jaw", 20)]
  , icount   = 1
  , iverbHit = "rip"
  , ieffects = [Hurt (4 * d 1)]
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

thorn = fist
  { iname    = "thorn"
  , ifreq    = [("thorn", 100)]
  , icount   = 7
  , iverbHit = "impale"
  , ieffects = [Hurt (1 * d 1)]
  , ifeature = [Identified]  -- not Durable
  , idesc    = ""
  }
razor = fist
  { iname    = "razor"
  , ifreq    = [("razor", 100)]
  , icount   = 7
  , iverbHit = "slice"
  , ieffects = [Hurt (2 * d 1)]
  , idesc    = ""
  }
fissure = fist
  { iname    = "fissure"
  , ifreq    = [("fissure", 100)]
  , icount   = 2
  , iverbHit = "hiss at"
  , ieffects = [Burn 1]
  , idesc    = ""
  }
sting = fist
  { iname    = "sting"
  , ifreq    = [("sting", 100)]
  , icount   = 1
  , iverbHit = "sting"
  , iaspects = [Timeout $ 1 + d 5]
  , ieffects = [Burn 1, Recharging (Paralyze 3)]
  , idesc    = ""
  }
venomTooth = fist
  { iname    = "venom tooth"
  , ifreq    = [("venom tooth", 100)]
  , icount   = 2
  , iverbHit = "bite"
  , iaspects = [Timeout $ 5 + d 3]
  , ieffects = [ Hurt (3 * d 1)
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
  , ieffects = [ Hurt (3 * d 1)
               , Recharging (toOrganNone "poisoned") ]
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
  , icount   = 4
  , iverbHit = "shock"
  , iaspects = [Timeout $ 2 + d 2]
  , ieffects = [Hurt (1 * d 1), Recharging (DropItem COrgan "temporary conditions" True)]
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
  , iaspects = [AddSmell 1]
  , idesc    = ""
  }

-- * Assorted

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
boilingVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("boiling vent", 100)]
  , icount   = 1
  , iverbHit = "menace"
  , iaspects = [Periodic, Timeout $ 3 + d 4 |*| 5]
  , ieffects = [Recharging (Explode "boiling water")]
  , idesc    = ""
  }
explosionVent = armoredSkin
  { iname    = "vent"
  , ifreq    = [("explosion vent", 100)]
  , icount   = 1
  , iverbHit = "menace"
  , iaspects = [Periodic, Timeout $ 2 + d 4 |*| 5]
  , ieffects = [Recharging (Explode "blast 20")]
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
