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
  [bolas, brassLantern, buckler, dart, dart200, gem1, gem2, gem3, gloveFencing, gloveGauntlet, gloveJousting, currency, gorget, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, net, oilLamp, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, ring1, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, shield, dagger, hammer, sword, halberd, wand1, wand2, woodenTorch, armorLeather, armorMail]

bolas,    brassLantern, buckler, dart, dart200, gem1, gem2, gem3, gloveFencing, gloveGauntlet, gloveJousting, currency, gorget, harpoon, jumpingPole, monocle, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, net, oilLamp, potion1, potion2, potion3, potion4, potion5, potion6, potion7, ring1, potion8, potion9, ring2, ring3, ring4, ring5, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, shield, dagger, hammer, sword, halberd, wand1, wand2, woodenTorch, armorLeather, armorMail :: ItemKind

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
  , iname    = "kitchen knife"
  , ifreq    = [("useful", 100), ("any arrow", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 3 * d 3
  , irarity  = [(1, 20)]
  , iverbHit = "prick"
  , iweight  = 200
  , iaspects = [AddHurtRanged ((d 6 + dl 6) * 10)]
  , ieffects = [Hurt (3 * d 1)]
  , ifeature = [toVelocity 75]  -- too flexible and the handle causes air drag
  , idesc    = "Little, but sharp and sturdy."
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
  , ifeature = [toVelocity 150]  -- ideal shape
  , idesc    = "Finely balanced for throws of great speed."
  , ikit     = []
  }

-- * Exotic thrown weapons

bolas = ItemKind
  { isymbol  = '{'
  , iname    = "bolas set"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = dl 4
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "entangle"
  , iweight  = 500
  , iaspects = []
  , ieffects = [Hurt (2 * d 1), Paralyze (5 + d 5), ActivateInv '!']
  , ifeature = []
  , idesc    = "Wood balls tied with hemp rope for tripping, entangling and bringing down crashing."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = '{'
  , iname    = "harpoon"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = dl 5
  , irarity  = [(5, 5), (10, 20)]
  , iverbHit = "hook"
  , iweight  = 4000
  , iaspects = [AddHurtRanged ((d 2 + 2 * dl 5) * 10)]
  , ieffects = [Hurt (4 * d 1), PullActor (ThrowMod 200 50)]
  , ifeature = []
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
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
               , DropBestWeapon, DropEqp ']' False ]
  , ifeature = []
  , idesc    = "A wide net with weights along the edges. Entangles weapon and armor alike."  -- TODO: shield instead of armor if a separate symbol for shields
  , ikit     = []
  }

-- * Lights

woodenTorch = ItemKind
  { isymbol  = '~'
  , iname    = "wooden torch"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 10)]
  , iverbHit = "scorch"
  , iweight  = 1200
  , iaspects = [ AddLight 3
               , AddSight (-2) ]  -- not only flashes, but also sparks
  , ieffects = [Burn 3]
  , ifeature = [EqpSlot EqpSlotAddLight "", Identified]
  , idesc    = "A smoking, heavy wooden torch, burning in an unsteady fire."
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
  , idesc    = "A clay lamp filled with plant oil feeding a tiny wick."
  , ikit     = []
  }
brassLantern = ItemKind
  { isymbol  = '~'
  , iname    = "brass lantern"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "burn"
  , iweight  = 2400
  , iaspects = [AddLight 4, AddSight (-1)]
  , ieffects = [Burn 4, Paralyze 4, OnSmash (Explode "burning oil 4")]
  , ifeature = [ toVelocity 70  -- hard to throw so that it opens and burns
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "Very bright and very heavy brass lantern."
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
  , idesc    = "Precious, though useless. Worth around 100 gold."
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
  , idesc    = "Highly ornamental, cold, large, steel mediallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
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
  , idesc    = "Tingling, rattling chain of flat encrusted links."
  , ikit     = []
  }
necklace1 = necklace
  { iaspects = [Periodic $ d 2 + dl 2]
  , ieffects = [RefillHP 1]
  , idesc    = "A cord of dried herbs and healing berries."
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

monocle = ItemKind
  { isymbol  = '='
  , iname    = "monocle"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(6, 0), (10, 1)]
  , iverbHit = "rap"
  , iweight  = 50
  , iaspects = [AddSight $ dl 3]
  , ieffects = []
  , ifeature = [Precious, Identified, Durable, EqpSlot EqpSlotAddSight ""]
  , idesc    = "Let's you better focus your weaker eye."
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
  , idesc    = "A sturdy ring with a strangely shining eye."
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
  , idesc    = "A haphazardly scribbled piece of parchment. May contain directions or a secret call sign."
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
  , iname    = "leather armor"
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
  , idesc    = "A stiff jacket formed from leather boiled in bee wax. Smells much better than the rest of your garment."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "mail armor"
  , iflavour = zipPlain [Cyan]
  , irarity  = [(7, 9)]
  , iweight  = 12000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (2 + dl 3) * 5
               , AddArmorRanged $ (2 + dl 3) * 5 ]
  , idesc    = "A long shirt woven from iron rings. Discourages foes from attacking your torso, making it harder for them to land a blow."
  }
gloveFencing = ItemKind
  { isymbol  = ']'
  , iname    = "leather gauntlet"
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
  , idesc    = "A fencing glove from rough leather ensuring a good grip. Also, quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "steel gauntlet"
  , irarity  = [(6, 12)]
  , iflavour = zipPlain [BrCyan]
  , iweight  = 300
  , iaspects = [ AddArmorMelee $ 2 * (d 2 + dl 2)
               , AddArmorRanged $ 2 * (d 2 + dl 2) ]
  , idesc    = "Long leather gauntlet covered in overlapping steel plates."
  }
gloveJousting = gloveFencing
  { iname    = "jousting gauntlet"
  , irarity  = [(6, 6)]
  , iflavour = zipFancy [BrRed]
  , iweight  = 500
  , iaspects = [ AddHurtMelee $ - 10 - d 5 + dl 5
               , AddArmorMelee $ 2 * (d 2 + dl 3)
               , AddArmorRanged $ 2 * (d 2 + dl 3) ]
  , idesc    = "Rigid, steel jousting handgear. If only you had a lance. And a horse."
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
  , idesc    = "Heavy and unwieldy. Absorbs a precentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
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
  , idesc    = "Large and unwieldy. Absorbs a precentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = '|'
  , iname    = "dagger"
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
  , idesc    = "A short dagger for thrusting and parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
hammer = ItemKind
  { isymbol  = '\\'
  , iname    = "war hammer"
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
  , idesc    = "It may not cause grave wounds, but neither does it glance off nor ricochet. Great sidearm for opportinistic blows against armored foes."
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
  , idesc    = "Hard to master, but chops limbs when used effectively."
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
  , idesc    = "Versatile, with great reach and leverage. Foes are held at a distance."
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
  , ifeature = [ toVelocity 125  -- magic
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
