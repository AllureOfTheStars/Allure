-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Weapon and treasure definitions.
module Content.ItemKind ( cdefs ) where

import Data.List

import Content.ItemKindActor
import Content.ItemKindOrgan
import Content.ItemKindShrapnel
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc (CStore (..))
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = validateItemKind
  , content = items ++ organs ++ shrapnels ++ actors
  }

items :: [ItemKind]
items =
  [canOfGlue, crankSpotlight, buckler, dart, dart200, gem1, gem2, gem3, gloveFencing, gloveGauntlet, gloveJousting, currency, gorget, harpoon, jumpingPole, contactLens, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, net, oilLamp, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, shield, dagger, hammer, sword, halberd, wand1, wand2, candle, armorLeather, armorMail, honingSteel]

canOfGlue,    crankSpotlight, buckler, dart, dart200, gem1, gem2, gem3, gloveFencing, gloveGauntlet, gloveJousting, currency, gorget, harpoon, jumpingPole, contactLens, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, net, oilLamp, potion1, potion2, potion3, potion4, potion5, potion6, potion7, ring1, potion8, potion9, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, shield, dagger, hammer, sword, halberd, wand1, wand2, candle, armorLeather, armorMail, honingSteel :: ItemKind

gem, necklace, potion, ring, scroll, wand :: ItemKind  -- generic templates

{- Item group symbols (from Angband, only as an informal convention for now):

! potion, flask, concoction, bottle, jar, vial, canister
? scroll, book, note, tablet, remote
, food
- magical wand, magical rod, transmitter, pistol, rifle
_ magical staff, scanner
= ring
" necklace
$ currency, gem
~ light, tool
/ polearm
| edged weapon
\ hafted weapon
} launcher
{ projectile
( clothes
[ torso armour
] misc. armour
) shield

-}

-- * Thrown weapons

dart = ItemKind
  { isymbol  = '{'
  , iname    = "steak knife"
  , ifreq    = [("useful", 100), ("any arrow", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 3 * d 3
  , irarity  = [(1, 20)]
  , iverbHit = "prick"
  , iweight  = 100
  , iaspects = [AddHurtRanged ((d 6 + dl 6) * 10)]
  , ieffects = [Hurt (3 * d 1)]
  , ifeature = [toVelocity 75]  -- no fins no special balance
  , idesc    = "Not particularly well balanced, but with a laser-sharpened titanium tip and blade."
  , ikit     = []
  }
dart200 = ItemKind
  { isymbol  = '{'
  , iname    = "billiard ball"
  , ifreq    = [("useful", 100), ("any arrow", 50)]  -- TODO: until arrows added
  , iflavour = zipPlain [BrWhite]
  , icount   = 3 * d 3
  , irarity  = [(4, 20)]
  , iverbHit = "prick"
  , iweight  = 300
  , iaspects = [AddHurtRanged ((d 6 + dl 6) * 10)]
  , ieffects = [Hurt (2 * d 1)]
  , ifeature = [toVelocity 150]
  , idesc    = "Ideal shape, size and weight for throwing."
  , ikit     = []
  }

-- * Exotic thrown weapons

canOfGlue = ItemKind
  { isymbol  = '{'
  , iname    = "can of glue"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Magenta]
  , icount   = dl 4
  , irarity  = [(5, 5), (10, 20)]
  , iverbHit = "glue"
  , iweight  = 1500
  , iaspects = []
  , ieffects = [Paralyze (5 + d 10)]
  , ifeature = [toVelocity 50]  -- unwieldy
  , idesc    = "A can of liquid, fast-setting, construction glue. Take the lid off before throwing."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = '{'
  , iname    = "harpoon"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = dl 5
  , irarity  = [(5, 3), (10, 5)]
  , iverbHit = "hook"
  , iweight  = 4000
  , iaspects = [AddHurtRanged ((d 2 + 2 * dl 5) * 10)]
  , ieffects = [Hurt (4 * d 1), PullActor (ThrowMod 200 50)]
  , ifeature = []
  , idesc    = "A display piece harking back to the Earth's oceanic tourism hayday. The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
net = ItemKind
  { isymbol  = '{'
  , iname    = "net"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = dl 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , iaspects = []
  , ieffects = [ Paralyze (5 + d 5)
               , DropBestWeapon, DropEqp ')' False ]
  , ifeature = []
  , idesc    = "A large synthetic fibre net with weights affixed along the edges. Entangles weapon and shields alike."
  , ikit     = []
  }

-- * Lights

candle = ItemKind
  { isymbol  = '~'
  , iname    = "candle"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 10)]
  , iverbHit = "scorch"
  , iweight  = 500
  , iaspects = [ AddLight 3
               , AddSight (-2) ]
  , ieffects = [Burn 3]
  , ifeature = [ toVelocity 50  -- easy to break when throwing
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "A smoking, thick candle with an unsteady fire."
  , ikit     = []
  }
oilLamp = ItemKind
  { isymbol  = '~'
  , iname    = "oil lamp"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 4), (10, 4)]
  , iverbHit = "burn"
  , iweight  = 1000
  , iaspects = [AddLight 3, AddSight (-1)]
  , ieffects = [Burn 3, Paralyze 3, OnSmash (Explode "burning oil 3")]
  , ifeature = [ toVelocity 70  -- hard not to spill the oil while throwing
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "A sizable glass lamp filled with plant oil feeding a wick."
  , ikit     = []
  }
crankSpotlight = ItemKind
  { isymbol  = '~'
  , iname    = "crank spotlight"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(10, 2)]
  , iverbHit = "snag"
  , iweight  = 2400
  , iaspects = [AddLight 4, AddArmorRanged $ - d 3]  -- noise and busy hands
  , ieffects = []
  , ifeature = [ EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "Powerful, wide-beam spotlight, powered by a hand-crank. Requires noisy two-handed recharging every few minutes."
  , ikit     = []
  }

-- * Treasure

gem = ItemKind
  { isymbol  = '*'
  , iname    = "gem"
  , ifreq    = [("treasure", 100)]  -- x3, but rare on shallow levels
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , iaspects = [AddLight 1, AddSpeed (-1)]  -- reflects strongly, distracts
  , ieffects = []
  , ifeature = [ Durable  -- prevent destruction by evil monsters
               , Precious ]
  , idesc    = "Precious, though useless. Worth around 100 gold grains."
  , ikit     = []
  }
gem1 = gem
  { irarity  = [(2, 0), (10, 10)]
  }
gem2 = gem
  { irarity  = [(5, 0), (10, 10)]
  }
gem3 = gem
  { irarity  = [(8, 0), (10, 10)]
  }
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold grain"
  , ifreq    = [("treasure", 100), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + d 20 + dl 20
  , irarity  = [(1, 0), (5, 20), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 1
  , iaspects = []
  , ieffects = []
  , ifeature = [Durable, Identified, Precious]
  , idesc    = "Reliably valuable in every civilized place."
  , ikit     = []
  }

-- * Periodic jewelry

gorget = ItemKind
  { isymbol  = '"'
  , iname    = "gorget"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy [BrCyan]
  , irarity  = [(4, 1), (10, 2)]
  , icount   = 1
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [Periodic $ d 4 + dl 4, AddArmorMelee 1, AddArmorRanged 1]
  , ieffects = [RefillCalm 1]
  , ifeature = [ Precious, EqpSlot EqpSlotPeriodic "", Identified
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  , ikit     = []
  }
necklace = ItemKind
  { isymbol  = '"'
  , iname    = "necklace"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , irarity  = [(4, 2), (10, 5)]
  , icount   = 1
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = []
  , ieffects = []
  , ifeature = [ Precious, EqpSlot EqpSlotPeriodic ""
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Tingling, rattling chain of flat encrusted links. Eccentric millionaires are known to hide their highly personalized body augmentation packs in such large jewelry pieces."
  , ikit     = []
  }
necklace1 = necklace
  { iaspects = [Periodic $ d 2 + dl 2]
  , ieffects = [RefillHP 1]
  }
necklace2 = necklace
  { irarity  = [(2, 0), (10, 1)]
  , iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Summon $ 1 + dl 2, Explode "waste"]
  }
necklace3 = necklace
  { iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Paralyze $ 5 + d 5 + dl 5, RefillCalm 50]
  }
necklace4 = necklace
  { iaspects = [Periodic $ 2 * d 10 + dl 10]
  , ieffects = [Teleport $ 2 + d 3]
  }
necklace5 = necklace
  { iaspects = [Periodic $ d 4 + dl 2]
  , ieffects = [Teleport $ 10 + d 10]
  }
necklace6 = necklace
  { iaspects = [Periodic $ 2 * d 5 + dl 5]
  , ieffects = [PushActor (ThrowMod 100 50)]
  }
necklace7 = necklace
  { irarity  = [(4, 0), (10, 2)]
  , iaspects = [Periodic $ 2 * d 5 + dl 15]
  , ieffects = [InsertMove 1, RefillHP (-1)]
  , ifeature = ifeature necklace ++ [Durable]
                 -- evil players would throw before death, to destroy
      -- TODO: teach AI to wear only for fight; prevent players from meleeing
      -- allies with that (Durable)
  }

-- * Non-periodic jewelry

contactLens = ItemKind
  { isymbol  = '='
  , iname    = "contact lens"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(6, 0), (10, 1)]
  , iverbHit = "rap"
  , iweight  = 50
  , iaspects = [AddSight $ dl 3]
  , ieffects = []
  , ifeature = [Precious, Identified, Durable, EqpSlot EqpSlotAddSight ""]
  , idesc    = "Advanced design. Never needs to be taken off."
  , ikit     = []
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(6, 2), (10, 5)]
  , iverbHit = "knock"
  , iweight  = 15
  , iaspects = []
  , ieffects = []
  , ifeature = [Precious, Identified]
  , idesc    = "A sturdy ring with a softly shining eye. If it contains a body booster unit, beware of the side-effects."
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(2, 0), (10, 2)]
  , iaspects = [AddSpeed 1, AddMaxHP $ dl 3 - 5 - d 3]
  , ifeature = ifeature ring ++ [Durable, EqpSlot EqpSlotAddSpeed ""]
  }
ring2 = ring
  { iaspects = [AddMaxHP $ 3 + dl 5, AddMaxCalm $ dl 6 - 15 - d 6]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddMaxHP ""]
  }
ring3 = ring
  { iaspects = [AddMaxCalm $ 10 + dl 10]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddMaxCalm ""]
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring4 = ring  -- TODO: move to level-ups and to timed effects
  { irarity  = [(3, 8), (10, 12)]
  , iaspects = [AddHurtMelee $ d 5 + dl 9, AddMaxHP $ dl 3 - 4 - d 2]
  , ifeature = ifeature ring ++ [Durable, EqpSlot EqpSlotAddHurtMelee ""]
  }
ring5 = ring  -- by the time it's found, probably no space in eqp
  { irarity  = [(5, 0), (10, 1)]
  , iaspects = [AddLight $ d 2]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddLight ""]
  }

-- * Exploding consumables, often intended to be thrown

potion = ItemKind
  { isymbol  = '!'
  , iname    = "vial"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy brightCol
  , icount   = 1
  , irarity  = [(1, 10), (10, 8)]
  , iverbHit = "splash"
  , iweight  = 200
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 50  -- oily, bad grip
               , Applicable, Fragile ]
  , idesc    = "A flask of bubbly, slightly oily liquid of a suspect color."
  , ikit     = []
  }
potion1 = potion
  { ieffects = [ NoEffect "rose water", Impress
               , OnSmash (ApplyPerfume), OnSmash (Explode "fragrance") ]
  }
potion2 = potion
  { irarity  = [(10, 2)]
  , ieffects = [ NoEffect "musky concoction", DropBestWeapon
               , OnSmash (Explode "pheromone")]
  }
potion3 = potion
  { ieffects = [RefillHP 5, OnSmash (Explode "healing mist")]
  }
potion4 = potion  -- TODO: a bit boring
  { irarity  = [(1, 5)]
  , ieffects = [RefillHP (-5), OnSmash (Explode "wounding mist")]
  }
potion5 = potion
  { ieffects = [ Explode "explosion blast 10"
               , PushActor (ThrowMod 200 75)
               , OnSmash (Explode "explosion blast 10") ]
  }
potion6 = potion
  { irarity  = [(10, 2)]
  , ieffects = [ NoEffect "distortion"
               , OnSmash (Explode "distortion")]
  }
potion7 = potion
  { ieffects = [ NoEffect "bait cocktail"
               , OnSmash (Summon $ 1 + dl 2), OnSmash (Explode "waste") ]
  }
potion8 = potion
  { ieffects = [ OneOf [Impress, DropBestWeapon, RefillHP 5, Burn 3]
               , OnSmash (OneOf [ Explode "healing mist"
                                , Explode "wounding mist"
                                , Explode "fragrance"
                                , Explode "explosion blast 10" ]) ]
  }
potion9 = potion
  { irarity  = [(1, 4), (10, 6)]
  , ieffects = [ OneOf [ Dominate, DropBestWeapon, RefillHP 15, Burn 9
                       , InsertMove 2]
               , OnSmash (OneOf [ Explode "healing mist"  -- TODO: make stronger
                                , Explode "pheromone"
                                , Explode "distortion"
                                , Explode "explosion blast 20" ]) ]
  }

-- * Non-exploding consumables, not specifically designed for throwing

scroll = ItemKind
  { isymbol  = '?'
  , iname    = "tablet"
  , ifreq    = [("useful", 100), ("any scroll", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1
  , irarity  = [(1, 10), (10, 7)]
  , iverbHit = "thump"
  , iweight  = 700
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 25  -- bad grip
               , Applicable ]
  , idesc    = "A standard issue spaceship crew tablet displaying a fixed infographic and a big button. Some of these still contain a one-time password authoriziing a particular spaceship's infrastructure transition. How would the infrastructure respond after so many years is unknown."
  , ikit     = []
  }
scroll1 = scroll
  { irarity  = [(10, 2)]
  , ieffects = [CallFriend 1]
  }
scroll2 = scroll
  { irarity  = [(1, 5), (10, 3)]
  , ieffects = [NoEffect "of fireworks", Explode "firecracker 7"]
  }
scroll3 = scroll
  { irarity  = [(1, 4), (10, 2)]
  , ieffects = [Ascend (-1)]
  }
scroll4 = scroll
  { ieffects = [ OneOf [ Teleport $ 2 + d 5, RefillCalm 10, RefillCalm (-10)
                       , InsertMove 4, Paralyze 10, Identify CGround ] ]
  }
scroll5 = scroll
  { irarity  = [(1, 4), (10, 6)]
  , ieffects = [ OneOf [ CallFriend 1, Summon $ d 2, Ascend (-1), Ascend 1
                       , RefillCalm 30, RefillCalm (-30), CreateItem $ d 2 ]
                       , PolyItem CGround ]
               -- TODO: ask player: Escape 1
  }
scroll6 = scroll
  { ieffects = [Teleport $ 2 + d 5]
  }
scroll7 = scroll
  { irarity  = [(10, 2)]
  , ieffects = [InsertMove $ d 2 + dl 2]
  }
scroll8 = scroll
  { irarity  = [(3, 6), (10, 3)]
  , ieffects = [Identify CGround]  -- TODO: ask player: AskPlayer cstore eff?
  }
scroll9 = scroll
  { irarity  = [(3, 3), (10, 9)]
  , ieffects = [PolyItem CGround]
  }

-- * Armor

armorLeather = ItemKind
  { isymbol  = '['
  , iname    = "spacesuit breastplate"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(4, 9)]
  , iverbHit = "thud"
  , iweight  = 7000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (1 + dl 3) * 5
               , AddArmorRanged $ (1 + dl 3) * 5 ]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "A hard-shell torso segment cut from a disposed off spacesuit."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "bulletproof vest"
  , iflavour = zipPlain [Cyan]
  , irarity  = [(7, 9)]
  , iweight  = 12000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (2 + dl 3) * 5
               , AddArmorRanged $ (2 + dl 3) * 5 ]
  , idesc    = "A civilian bulletproof vest. Discourages foes from attacking your torso, making it harder for them to land a blow."
  }
gloveFencing = ItemKind
  { isymbol  = ']'
  , iname    = "construction glove"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(4, 6), (10, 12)]
  , iverbHit = "flap"
  , iweight  = 100
  , iaspects = [ AddHurtMelee $ 2 * (d 2 + 2 * dl 5)
               , AddArmorRanged $ d 2 + dl 2 ]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- flaps and flutters
               , Durable, EqpSlot EqpSlotAddArmorRanged "", Identified ]
  , idesc    = "A flexible construction glove from rough leather ensuring a good grip. Also, quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "spacesuit glove"
  , irarity  = [(6, 12)]
  , iflavour = zipPlain [BrCyan]
  , iweight  = 300
  , iaspects = [ AddArmorMelee $ 2 * (d 2 + dl 2)
               , AddArmorRanged $ 2 * (d 2 + dl 2) ]
  , idesc    = "A piece of a hull maintenance spacesuit, padded and reinforced with carbon fibre."
  }
gloveJousting = gloveFencing
  { iname    = "welding handgear"
  , irarity  = [(6, 6)]
  , iflavour = zipFancy [BrRed]
  , iweight  = 500
  , iaspects = [ AddHurtMelee $ - 10 - d 5 + dl 5
               , AddArmorMelee $ 2 * (d 2 + dl 3)
               , AddArmorRanged $ 2 * (d 2 + dl 3) ]
  , idesc    = "Rigid, bulky handgear embedding a welding equipment, complete with an affixed small shield and a darkened visor. Awe-inspiring."
  }
-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
buckler = ItemKind
  { isymbol  = ')'
  , iname    = "buckler"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 7)]
  , iverbHit = "bash"
  , iweight  = 2000
  , iaspects = [AddArmorMelee 40, AddHurtMelee (-30)]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Heavy and unwieldy arm protection made from an outer airlock panel. Absorbs a precentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(7, 7)]
  , iflavour = zipPlain [Green]
  , iweight  = 3000
  , iaspects = [AddArmorMelee 80, AddHurtMelee (-70)]
  , ifeature = [ toVelocity 20  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Large and unwieldy rectangle made of anti-meteorite ceramic sheet. Absorbs a precentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = '|'
  , iname    = "kitchen knife"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 20), (10, 4)]
  , iverbHit = "stab"
  , iweight  = 1000
  , iaspects = [AddHurtMelee $ 2 * (d 2 + 2 * dl 5), AddArmorMelee $ d 4 + dl 4]
  , ieffects = [Hurt (4 * d 1)]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A heavy professional kitchen blade. Will do fine cutting any kind of meat and also thrusting and parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
hammer = ItemKind
  { isymbol  = '\\'
  , iname    = "demolition hammer"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(4, 12), (10, 2)]
  , iverbHit = "club"
  , iweight  = 1500
  , iaspects = [AddHurtMelee $ d 2 + 2 * dl 5]
  , ieffects = [Hurt (6 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the sharp tip costs
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A hammer on a long handle used for construction work. It may not cause grave wounds, but neither does it ricochet or glance off armor. Great sidearm for opportinistic blows against armored foes."
  , ikit     = []
  }
sword = ItemKind
  { isymbol  = '/'
  , iname    = "sharpened pipe"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(3, 1), (6, 20), (10, 10)]
  , iverbHit = "slash"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (9 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the tip costs speed
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A makeshift weapon of simple design, but great potential. Hard to master, though."
  , ikit     = []
  }
halberd = ItemKind
  { isymbol  = '/'
  , iname    = "halberd"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(7, 1), (10, 10)]
  , iverbHit = "impale"
  , iweight  = 3000
  , iaspects = [AddArmorMelee $ 2 * (d 4 + dl 4)]
  , ieffects = [Hurt (12 * d 1)]
  , ifeature = [ toVelocity 20  -- not balanced
               , Durable, EqpSlot EqpSlotWeapon "", Identified ]
  , idesc    = "A perfect replica made for a reenactor troupe, missing only some sharpening. Versatile, with great reach and leverage. Foes are held at a distance."
  , ikit     = []
  }

-- * Wands

wand = ItemKind
  { isymbol  = '-'
  , iname    = "injector"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , irarity  = []  -- TODO: add charges, etc.
  , iverbHit = "club"
  , iweight  = 300
  , iaspects = [AddLight 1, AddSpeed (-1)]  -- pulsing with power, distracts
  , ieffects = []
  , ifeature = [ toVelocity 125  -- sufficiently advanced tech
               , Applicable, Durable ]
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."
  , ikit     = []
  }
wand1 = wand
  { ieffects = []  -- TODO: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '{'
  }
wand2 = wand
  { ieffects = []
  }

-- * Assorted tools

jumpingPole = ItemKind
  { isymbol  = '~'
  , iname    = "jumping pole"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
  , iverbHit = "prod"
  , iweight  = 10000
  , iaspects = []
  , ieffects = [InsertMove 2]  -- TODO: implement with timed speed instead
                               -- and then make Durable, freq 2, and just trade
                               -- taken turn now for a free turn later
  , ifeature = [Applicable, Identified]
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }

honingSteel = ItemKind
  { isymbol  = '~'
  , iname    = "honing steel"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(5, 5)]
  , iverbHit = "prod"
  , iweight  = 400
  , iaspects = [AddHurtMelee $ d 2 + 2 * dl 5]
  , ieffects = []
  , ifeature = [EqpSlot EqpSlotAddHurtMelee "", Identified]
  , idesc    = "Originally used for realigning the bent or buckled edges of kitchen knives in the local bars. Now it saves lives by letting you fix your weapons between or even during fights, without the need to set up camp, fish out tools and assemble a proper sharpening workshop."
  , ikit     = []
  }
