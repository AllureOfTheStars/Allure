-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Item definitions.
module Content.ItemKind
  ( content, items, otherItemContent
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindEmbed
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

content :: [ItemKind]
content = items ++ otherItemContent

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
  [sandstoneRock, dart, spike, slingStone, slingBullet, paralizingProj, harpoon, net, light1, light2, light3, blanket, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, imageItensifier, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, gem5, currency]
  -- Allure-specific
  ++ [needle, constructionHooter, scroll14]

sandstoneRock,    dart, spike, slingStone, slingBullet, paralizingProj, harpoon, net, light1, light2, light3, blanket, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, imageItensifier, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, gem5, currency :: ItemKind
-- Allure-specific
needle, constructionHooter, scroll14 :: ItemKind

necklace, ring, potion, flask, scroll, wand, gem :: ItemKind  -- generic templates

-- Keep the dice rolls and sides in aspects small so that not too many
-- distinct items are generated (for display in item lore and for narrative
-- impact ("oh, I found the more powerful of the two variants of the item!",
-- instead of "hmm, I found one of the countless variants, a decent one").
-- In particular, for unique items, unless they inherit aspects from
-- a standard item, permit only a couple possible variants.
-- This is especially important if an item kind has mulitple random aspects.
-- Instead multiply dice results, e.g., (1 `d` 3) * 5 instead of 1 `d` 15.

-- * Item group symbols, from Angband and variants

symbolProjectile, _symbolLauncher, symbolLight, symbolTool, symbolSpecial, symbolGold, symbolNecklace, symbolRing, symbolPotion, symbolFlask, symbolScroll, symbolTorsoArmor, symbolMiscArmor, _symbolClothes, symbolShield, symbolPolearm, symbolEdged, symbolHafted, symbolWand, _symbolStaff, symbolFood :: Char

symbolProjectile = '{'
_symbolLauncher  = '}'
symbolLight      = '('
symbolTool       = ')'
symbolSpecial    = '*'  -- don't overuse, because it clashes with projectiles
symbolGold       = '$'  -- also gems
symbolNecklace   = '"'
symbolRing       = '='
symbolPotion     = '!'  -- concoction, bottle, jar, vial, canister
symbolFlask      = '!'
symbolScroll     = '?'  -- book, note, tablet, remote, chip, card
symbolTorsoArmor = '['
symbolMiscArmor  = '['
_symbolClothes   = '['
symbolShield     = ']'
symbolPolearm    = '/'
symbolEdged      = '|'
symbolHafted     = '\\'
symbolWand       = '-'  -- magical rod, transmitter, pistol, rifle
_symbolStaff     = '_'  -- scanner
symbolFood       = ','  -- also body part; distinct from floor: not middle dot

-- * Thrown weapons

sandstoneRock = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "ceramic foam splinter"
  , ifreq    = [("sandstone rock", 1), ("weak arrow", 10)]
  , iflavour = zipPlain [Green]
  , icount   = 1 `d` 2
  , irarity  = [(1, 50), (10, 1)]
  , iverbHit = "hit"
  , iweight  = 300
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ -16 * 5]
  , ieffects = []
  , ifeature = [toVelocity 70, Fragile, Identified]  -- not dense, irregular
  , idesc    = "A light, irregular lump of ceramic foam used in construction."
  , ikit     = []
  }
dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "billiard ball"
  , ifreq    = [("useful", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 4 `d` 3
  , irarity  = [(1, 20), (10, 10)]
  , iverbHit = "strike"
  , iweight  = 170
  , idamage  = [(98, 1 `d` 1), (2, 2 `d` 1)]
  , iaspects = [AddHurtMelee $ (-14 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- only leather-piercing
  , ieffects = []
  , ifeature = [Identified]
  , idesc    = "Ideal shape, size and weight for throwing."
  , ikit     = []
  }
spike = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "steak knife"
  , ifreq    = [("useful", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [Cyan]
  , icount   = 4 `d` 3
  , irarity  = [(1, 10), (10, 15)]
  , iverbHit = "nick"
  , iweight  = 100
  , idamage  = [(98, 2 `d` 1), (2, 4 `d` 1)]
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- heavy vs armor
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
  , ifeature = [toVelocity 70, Identified]  -- hitting with tip costs speed
  , idesc    = "Not particularly well balanced, but with a laser-sharpened titanium tip and blade."
  , ikit     = []
  }
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "steel hex nut"
  , ifreq    = [("useful", 5), ("any arrow", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 3 `d` 3
  , irarity  = [(1, 1), (10, 20)]
  , iverbHit = "hit"
  , iweight  = 200
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 4) * 5]
                 -- heavy vs armor
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
  , ifeature = [toVelocity 150, Identified]
  , idesc    = "A large hexagonal fastening nut, securely lodging in the pouch of a makeshift string and cloth sling due to its angular shape."
  , ikit     = []
  }
slingBullet = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bearing ball"
  , ifreq    = [("useful", 5), ("any arrow", 100)]
  , iflavour = zipPlain [White]
  , icount   = 6 `d` 3
  , irarity  = [(1, 1), (10, 15)]
  , iverbHit = "hit"
  , iweight  = 28
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ (-17 + 1 `d` 2 + 1 `dL` 4) * 5]
                 -- not armor-piercing
  , ieffects = []
  , ifeature = [toVelocity 200, Identified]
  , idesc    = "Small but heavy bearing ball. Due to its size and shape, it securely fits in the makeshift sling's pouch and doesn't snag when released."
  , ikit     = []
  }

-- * Exotic thrown weapons

-- Identified, because shape (and name) says it all. Detailed stats id by use.
paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "can"
  , ifreq    = [("useful", 100), ("can of sticky foam", 1)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1 + 1 `dL` 3
  , irarity  = [(5, 5), (10, 20)]
  , iverbHit = "glue"
  , iweight  = 1000
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ -14 * 5]
  , ieffects = [ ELabel "of sticky foam", Paralyze (10 + 1 `d` 12)
               , OnSmash (Explode "glue") ]
  , ifeature = [toVelocity 70, Identified, Lobable, Fragile]  -- unwieldy
  , idesc    = "A can of liquid, fast-setting construction foam."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [("ship", 100), ("harpoon", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 + 1 `dL` 5
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "hook"
  , iweight  = 750
  , idamage  = [(99, 5 `d` 1), (1, 10 `d` 1)]
  , iaspects = [AddHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 4) * 5]
  , ieffects = [PullActor (ThrowMod 200 50)]
  , ifeature = [Identified]
  , idesc    = "A display piece harking back to the Earth's oceanic tourism hayday. The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1 + 1 `dL` 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [AddHurtMelee $ -14 * 5]
  , ieffects = [ toOrganGameTurn "slowed" (3 + 1 `d` 3)
               , DropItem maxBound 1 CEqp "torso armor" ]
  , ifeature = [Identified]
  , idesc    = "A large synthetic fibre net with weights affixed along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }

-- * Lights

light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "torch"
  , ifreq    = [("useful", 100), ("light source", 100), ("wooden torch", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `d` 2
  , irarity  = [(1, 15)]
  , iverbHit = "scorch"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddShine 3       -- not only flashes, but also sparks,
               , AddSight (-2) ]  -- so unused by AI due to the mixed blessing
  , ieffects = [Burn 1, EqpSlot EqpSlotLightSource]
  , ifeature = [Lobable, Identified, Equipable]  -- not Fragile; reusable flare
  , idesc    = "A torch improvised with cloth soaked in tar on a stick."
  , ikit     = []
  }
light2 = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(6, 7)]
  , iverbHit = "burn"
  , iweight  = 1500
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddShine 3, AddSight (-1)]
  , ieffects = [ Burn 1, Paralyze 6, OnSmash (Explode "burning oil 3")
               , EqpSlot EqpSlotLightSource ]
  , ifeature = [Lobable, Fragile, Identified, Equipable]
  , idesc    = "A sizable restaurant glass lamp filled with plant oil feeding a wick."
  , ikit     = []
  }
light3 = ItemKind
  { isymbol  = symbolLight
  , iname    = "crank spotlight"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "snag"
  , iweight  = 3000
  , idamage  = toDmg 0
  , iaspects = [ AddShine 4
               , AddArmorRanged $ - 1 `d` 3 ]  -- noise and distraction
  , ieffects = [EqpSlot EqpSlotLightSource]
  , ifeature = [Identified, Equipable]
  , idesc    = "Powerful, wide-beam spotlight, powered by a hand-crank. Requires noisy two-handed recharging every few minutes."
  , ikit     = []
  }
blanket = ItemKind
  { isymbol  = symbolLight
  , iname    = "mineral fibre blanket"
  , ifreq    = [("useful", 100), ("light source", 100), ("blanket", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "swoosh"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddShine (-10)  -- douses torch, lamp and lantern in one action
               , AddArmorMelee 1, AddMaxCalm 2 ]
  , ieffects = []
  , ifeature = [Lobable, Identified, Equipable]  -- not Fragile; reusable douse
  , idesc    = ""
  , ikit     = []
  }

-- * Exploding consumables, often intended to be thrown.

-- Not identified, because they are perfect for the id-by-use fun,
-- due to effects. They are fragile and upon hitting the ground explode
-- for effects roughly corresponding to their normal effects.
-- Whether to hit with them or explode them close to the tartget
-- is intended to be an interesting tactical decision.
--
-- Flasks are often not natural; maths, magic, distillery.
-- In reality, they just cover all temporary effects, which in turn matches
-- all aspects.
--
-- No flask nor temporary organ of Calm depletion, since Calm reduced often.
flask = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [("useful", 100), ("flask", 100), ("any vial", 100)]
  , iflavour = zipLiquid darkCol ++ zipPlain darkCol ++ zipFancy darkCol
               ++ zipLiquid brightCol
  , icount   = 1
  , irarity  = [(1, 7), (10, 4)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Applicable, Lobable, Fragile, toVelocity 50]  -- oily, bad grip
  , idesc    = "A flask of oily liquid of a suspect color. Something seems to be moving inside."
  , ikit     = []
  }
flask1 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of strength renewal brew"
               , toOrganActorTurn "strengthened" (20 + 1 `d` 5)
               , toOrganNone "regenerating"
               , OnSmash (Explode "dense shower") ]
  }
flask2 = flask
  { ieffects = [ ELabel "of weakness brew"
               , toOrganGameTurn "weakened" (20 + 1 `d` 5)
               , OnSmash (Explode "sparse shower") ]
  }
flask3 = flask
  { ieffects = [ ELabel "of melee protective balm"
               , toOrganActorTurn "protected from melee" (20 + 1 `d` 5)
               , OnSmash (Explode "melee protective balm") ]
  }
flask4 = flask
  { ieffects = [ ELabel "of ranged protective balm"
               , toOrganActorTurn "protected from ranged" (20 + 1 `d` 5)
               , OnSmash (Explode "ranged protective balm") ]
  }
flask5 = flask
  { ieffects = [ ELabel "of red paint"
               , toOrganGameTurn "painted red" (20 + 1 `d` 5)
               , OnSmash (Explode "red paint") ]
  }
flask6 = flask
  { irarity  = []
  }
flask7 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of haste brew"
               , toOrganActorTurn "hasted" (20 + 1 `d` 5)
               , OnSmash (Explode "blast 20")
               , OnSmash (Explode "haste spray") ]
  }
flask8 = flask
  { irarity  = [(1, 14), (10, 4)]
  , ieffects = [ ELabel "of lethargy brew"
               , toOrganGameTurn "slowed" (20 + 1 `d` 5)
               , toOrganNone "regenerating", toOrganNone "regenerating"  -- x2
               , RefillCalm 5
               , OnSmash (Explode "slowness mist") ]
  }
flask9 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of eye drops"
               , toOrganActorTurn "far-sighted" (40 + 1 `d` 10)
               , OnSmash (Explode "eye drop") ]
  }
flask10 = flask
  { irarity  = [(10, 2)]
  , ieffects = [ ELabel "of smelly concoction"
               , toOrganActorTurn "keen-smelling" (40 + 1 `d` 10)
               , DetectActor 10
               , OnSmash (Explode "smelly droplet") ]
  }
flask11 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of cat tears"
               , toOrganActorTurn "shiny-eyed" (40 + 1 `d` 10)
               , OnSmash (Explode "eye shine") ]
  }
flask12 = flask
  { irarity  = [(1, 14), (10, 10)]
  , ieffects = [ ELabel "of whiskey"
               , toOrganActorTurn "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3
               , OnSmash (Explode "whiskey spray") ]
  }
flask13 = flask
  { ieffects = [ ELabel "of bait cocktail"
               , toOrganActorTurn "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3
               , Summon "mobile animal" 1
               , OnSmash (Summon "mobile animal" 1)
               , OnSmash Impress
               , OnSmash (Explode "waste") ]
  }
-- The player has full control over throwing the flask at his party,
-- so he can milk the explosion, so it has to be much weaker, so a weak
-- healing effect is enough. OTOH, throwing a harmful flask at many enemies
-- at once is not easy to arrange, so these explostions can stay powerful.
flask14 = flask
  { irarity  = [(1, 4), (10, 14)]
  , ieffects = [ ELabel "of regeneration brew"
               , toOrganNone "regenerating", toOrganNone "regenerating"  -- x2
               , OnSmash (Explode "healing mist") ]
  }
flask15 = flask
  { ieffects = [ ELabel "of poison"
               , toOrganNone "poisoned", toOrganNone "poisoned"  -- x2
               , OnSmash (Explode "poison cloud") ]
  }
flask16 = flask
  { irarity  = [(1, 14), (10, 4)]
  , ieffects = [ ELabel "of weak poison"
               , toOrganNone "poisoned"
               , OnSmash (Explode "poison cloud") ]
  }
flask17 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of slow resistance"
               , toOrganNone "slow resistant"
               , OnSmash (Explode "blast 10")
               , OnSmash (Explode "anti-slow mist") ]
  }
flask18 = flask
  { irarity  = [(10, 4)]
  , ieffects = [ ELabel "of poison resistance"
               , toOrganNone "poison resistant"
               , OnSmash (Explode "antidote mist") ]
  }
flask19 = flask
  { ieffects = [ ELabel "of blindness"
               , toOrganGameTurn "blind" (40 + 1 `d` 10)
               , OnSmash (Explode "iron filing") ]
  }
flask20 = flask
  { ieffects = [ ELabel "of calamity"
               , toOrganNone "poisoned"
               , toOrganGameTurn "weakened" (20 + 1 `d` 5)
               , toOrganGameTurn "painted red" (20 + 1 `d` 5)
               , OnSmash (Explode "poison cloud") ]
  }

-- Potions are often natura. Various configurations of effects.
-- A different class of effects is on scrolls and/or mechanical items.
-- Some are shared.

potion = ItemKind
  { isymbol  = symbolPotion
  , iname    = "vial"
  , ifreq    = [("useful", 100), ("potion", 100), ("any vial", 100)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1
  , irarity  = [(1, 10), (10, 7)]
  , iverbHit = "splash"
  , iweight  = 200
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Applicable, Lobable, Fragile, toVelocity 50]  -- oily, bad grip
  , idesc    = "A vial of bright, frothing concoction. The best that nature has to offer."
  , ikit     = []
  }
potion1 = potion
  { ieffects = [ ELabel "of rose water", Impress, RefillCalm (-5)
               , OnSmash ApplyPerfume, OnSmash (Explode "fragrance") ]
  }
potion2 = potion
  { ifreq    = [("ship", 100)]
  , irarity  = [(6, 9), (10, 9)]
  , ieffects = [ Unique, ELabel "of Attraction", Impress, RefillCalm (-20)
               , OnSmash (Explode "pheromone") ]
  -- , idesc    = ""
  }
potion3 = potion
  { ieffects = [ RefillHP 5, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist") ]
  }
potion4 = potion
  { irarity  = [(1, 7), (10, 10)]
  , ieffects = [ RefillHP 10, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist 2") ]
  }
potion5 = potion  -- needs to be common to show at least a portion of effects
  { irarity  = [(1, 30), (10, 15)]
  , ieffects = [ OneOf [ RefillHP 10, RefillHP 5, Burn 5
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganActorTurn "strengthened" (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode "dense shower"
                                , Explode "sparse shower"
                                , Explode "melee protective balm"
                                , Explode "ranged protective balm"
                                , Explode "red paint"
                                , Explode "blast 10" ]) ]
  }
potion6 = potion  -- needs to be common to show at least a portion of effects
  { irarity  = [(1, 5), (10, 20)]
  , ieffects = [ Impress
               , OneOf [ RefillCalm (-60)
                       , RefillHP 20, RefillHP 10, Burn 10
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganActorTurn "hasted" (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "wounding mist"
                                , Explode "distressing odor"
                                , Explode "haste spray"
                                , Explode "slowness mist"
                                , Explode "fragrance"
                                , Explode "blast 20" ]) ]
  }
potion7 = potion
  { irarity  = [(1, 11), (10, 4)]
  , ieffects = [ DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "antidote mist") ]
  }
potion8 = potion
  { irarity  = [(1, 9)]
  , ieffects = [ DropItem 1 maxBound COrgan "temporary condition"
               , OnSmash (Explode "blast 10") ]
  }
potion9 = potion
  { irarity  = [(10, 9)]
  , ieffects = [ DropItem maxBound maxBound COrgan "temporary condition"
               , OnSmash (Explode "blast 20") ]
  }
potion10 = potion
  { ifreq    = [("ship", 100)]
  , irarity  = [(10, 4)]
  , ieffects = [ Unique, ELabel "of Love", RefillHP 60
               , Impress, RefillCalm (-60)
               , OnSmash (Explode "healing mist 2")
               , OnSmash (Explode "pheromone") ]
  -- , idesc    = ""
  }

-- * Non-exploding consumables, not specifically designed for throwing

scroll = ItemKind
  { isymbol  = symbolScroll
  , iname    = "chip"
  , ifreq    = [("useful", 100), ("any scroll", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1
  , irarity  = [(1, 14), (10, 11)]
  , iverbHit = "thump"
  , iweight  = 20
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 30  -- too small
               , Applicable ]
  , idesc    = "A generic, diposable chip, capable of a one-time holo-display. Some of these also contain a one-time password authorizing a particular spaceship's infrastructure transition. It is unknown how the infrastructure might respond after so many years."
  , ikit     = []
  }
scroll1 = scroll
  { ifreq    = [("ship", 100), ("any scroll", 100)]
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so available early
  , ieffects = [ Unique, ELabel "of Reckless Beacon"
               , Summon "hero" 1, Summon "mobile animal" (2 + 1 `d` 2) ]
  , idesc    = "This ihdustrial wide-spectrum alarm broadcaster, if over-amped for a single powerful blast, should be able to cut through the interference and reach any lost crew members, giving them enough positional information to locate us."
  }
scroll2 = scroll
  { irarity  = [(1, 2)]
  , ieffects = [ ELabel "of greed", DetectItem 20, Teleport 20
               , RefillCalm (-100) ]
  }
scroll3 = scroll
  { ifreq    = [("ship", 100), ("any scroll", 100)]
  , irarity  = [(1, 8), (10, 4)]
  , ieffects = [Ascend True]
  }
scroll4 = scroll  -- needs to be common to show at least a portion of effects
  { irarity  = [(1, 40), (10, 20)]
  , ieffects = [OneOf [ Teleport 5, RefillCalm 5, InsertMove 5
                      , DetectActor 20, DetectItem 20 ]]
  }
scroll5 = scroll  -- needs to be common to show at least a portion of effects
  { irarity  = [(10, 30)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend False, Ascend True
                       , Summon "hero" 1, Summon "mobile animal" $ 1 `d` 2
                       , Detect 40, RefillCalm (-100)
                       , CreateItem CGround "useful" timerNone ] ]
  }
scroll6 = scroll
  { ieffects = [Teleport 5]
  }
scroll7 = scroll
  { ieffects = [Teleport 20]
  }
scroll8 = scroll
  { irarity  = [(10, 2)]
  , ieffects = [InsertMove $ 1 + 1 `d` 2 + 1 `dL` 2]
  }
scroll9 = scroll
  { irarity  = [(1, 30)]
  , ieffects = [ ELabel "of scientific explanation"
               , Composite [Identify, RefillCalm 10] ]
      -- your most pressing existential concerns are answered scientifically,
      -- hence the calming effect
  }
scroll10 = scroll
  { irarity  = [(10, 20)]
  , ieffects = [ ELabel "of molecular reconfiguration"
               , Composite [PolyItem, Explode "firecracker 7"] ]
  }
scroll11 = scroll
  { ifreq    = [("ship", 100), ("any scroll", 100)]
  , irarity  = [(6, 9), (10, 9)]
  , ieffects = [Unique, ELabel "of Prisoner Release", Summon "hero" 1]
  , idesc    = "This lock chip opens a nearby closet containing one of our lost crew members."
  }
scroll12 = scroll
  { irarity  = [(1, 9), (10, 4)]
  , ieffects = [DetectHidden 20]
  }
scroll13 = scroll
  { ieffects = [ELabel "of acute hearing", DetectActor 20]
  }

-- * Assorted tools

jumpingPole = ItemKind
  { isymbol  = symbolTool
  , iname    = "jumping pole"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 2)]
  , iverbHit = "prod"
  , iweight  = 10000
  , idamage  = toDmg 0
  , iaspects = [Timeout $ (2 + 1 `d` 2 - 1 `dL` 2) * 10]
  , ieffects = [Recharging (toOrganActorTurn "hasted" 1)]
  , ifeature = [Durable, Applicable, Identified]
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }
sharpeningTool = ItemKind
  { isymbol  = symbolTool
  , iname    = "honing steel"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "smack"
  , iweight  = 400
  , idamage  = toDmg 0
  , iaspects = [AddHurtMelee $ (1 + 1 `dL` 5) * 5]
  , ieffects = [EqpSlot EqpSlotAddHurtMelee]
  , ifeature = [Identified, Equipable]
  , idesc    = "Originally used for realigning the bent or buckled edges of kitchen knives in the local bars. Now it saves lives by letting you fix your weapons between or even during fights, without the need to set up camp, fish out tools and assemble a proper sharpening workshop."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = symbolFood
  , iname    = "visual sensor"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "gaze at"
  , iweight  = 500
  , idamage  = toDmg 0
  , iaspects = [ AddSight 10, AddMaxCalm 30, AddShine 2
               , Timeout $ 1 + 1 `d` 2 ]
  , ieffects = [ Periodic
               , Recharging (toOrganNone "poisoned")
               , Recharging (Summon "mobile robot" 1) ]
  , ifeature = [Identified]
  , idesc    = "A functioning visual sensor torn out from some sizable robot. The circuitry seem too large for basic signal processing alone. Watch out for the sharp edges and the seeping coolant liquid."
  , ikit     = []
  }
motionScanner = ItemKind
  { isymbol  = symbolTool
  , iname    = "handhelp sonar"
  , ifreq    = [("useful", 100), ("add nocto 1", 20)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "ping"
  , iweight  = 1000
  , idamage  = toDmg 0
  , iaspects = [ AddNocto 1
               , AddArmorMelee (-10 + 1 `dL` 5)
               , AddArmorRanged (-10 + 1 `dL` 5) ]
  , ieffects = [EqpSlot EqpSlotMiscBonus]
  , ifeature = [Identified, Equipable]
  , idesc    = "Handheld underwater echolocator overdriven to scan dark corridors at the cost of emitting loud pings."
  , ikit     = []
  }

-- * Periodic jewelry

gorget = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "Old Gorget"
  , ifreq    = [("ship", 50)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(4, 3), (10, 3)]  -- weak, shallow
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = toDmg 0
  , iaspects = [ Timeout $ (1 `d` 2) * 2
               , AddArmorMelee 3
               , AddArmorRanged 2 ]
  , ieffects = [ Unique, Periodic
               , Recharging (RefillCalm 1), EqpSlot EqpSlotMiscBonus ]
  , ifeature = [Durable, Precious, Identified, Equipable]
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  , ikit     = []
  }
-- Not idenfified, because the id by use, e.g., via periodic activations. Fun.
necklace = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [("useful", 100), ("any jewelry", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(10, 2)]
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = [Periodic]
  , ifeature = [Precious, toVelocity 50, Equipable]  -- not dense enough
  , idesc    = "Tingling, rattling chain of flat encrusted links. Eccentric millionaires are known to hide their highly personalized body augmentation packs in such large jewelry pieces."
  , ikit     = []
  }
necklace1 = necklace
  { ifreq    = [("ship", 100), ("any jewelry", 100)]
  , irarity  = [(3, 0), (4, 1), (10, 2)]  -- prevents camping on lvl 3
  , iaspects = [Timeout $ (1 `d` 2) * 20]
  , ieffects = [ Unique, ELabel "of Trickle Life", EqpSlot EqpSlotMiscBonus
               , Recharging (RefillHP 1) ]
               ++ ieffects necklace
  , ifeature = Durable : ifeature necklace
  -- , idesc    = ""
  }
necklace2 = necklace
  { ifreq    = [("treasure", 100), ("any jewelry", 100)]
      -- just too nasty to call it useful
  , irarity  = [(1, 1)]
  , iaspects = [Timeout 30]
  , ieffects = [ Recharging (Summon "mobile animal" $ 1 `d` 2)
               , Recharging (Explode "waste")
               , Recharging Impress
               , Recharging (DropItem 1 maxBound COrgan "temporary condition") ]
               ++ ieffects necklace
  }
necklace3 = necklace
  { iaspects = [Timeout $ (1 `d` 2) * 20]
  , ieffects = [ ELabel "of fearful listening"
               , Recharging (DetectActor 10)
               , Recharging (RefillCalm (-20)) ]
               ++ ieffects necklace
  }
necklace4 = necklace
  { iaspects = [Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 2]
  , ieffects = [Recharging (Teleport $ 3 `d` 2)]
               ++ ieffects necklace
  }
necklace5 = necklace
  { iaspects = [Timeout $ (6 - 1 `dL` 5) * 10]
  , ieffects = [ ELabel "of escape"
               , Recharging (Teleport $ 14 + 3 `d` 3)
               , Recharging (DetectExit 20)
               , Recharging (RefillHP (-2)) ]  -- prevent micromanagement
               ++ ieffects necklace
  }
necklace6 = necklace
  { iaspects = [Timeout $ (1 + 1 `d` 3) * 2]
  , ieffects = [Recharging (PushActor (ThrowMod 100 50))]
               ++ ieffects necklace
  }
necklace7 = necklace
  { ifreq    = [("ship", 100), ("any jewelry", 100)]
  , iaspects = [AddMaxHP 15, AddArmorMelee 20, AddArmorRanged 10, Timeout 4]
  , ieffects = [ Unique, ELabel "of Overdrive", EqpSlot EqpSlotAddSpeed
               , Recharging (InsertMove $ 1 `d` 3)  -- unpredictable
               , Recharging (RefillHP (-1))
               , Recharging (RefillCalm (-1)) ]  -- fake "hears something" :)
               ++ ieffects necklace
  , ifeature = Durable : ifeature necklace
  -- , idesc    = ""
  }
necklace8 = necklace
  { iaspects = [Timeout $ (1 + 1 `d` 3) * 5]
  , ieffects = [Recharging $ Explode "spark"]
               ++ ieffects necklace
  }
necklace9 = necklace
  { iaspects = [Timeout $ (1 + 1 `d` 3) * 5]
  , ieffects = [Recharging $ Explode "fragrance"]
               ++ ieffects necklace
  }

-- * Non-periodic jewelry

imageItensifier = ItemKind
  { isymbol  = symbolRing
  , iname    = "noctovisor"
  , ifreq    = [("treasure", 100), ("add nocto 1", 80)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "rattle"
  , iweight  = 700
  , idamage  = toDmg 0
  , iaspects = [AddNocto 1, AddSight (-1), AddArmorMelee $ (1 + 1 `dL` 3) * 3]
  , ieffects = [EqpSlot EqpSlotMiscBonus]
  , ifeature = [Precious, Identified, Durable, Equipable]
  , idesc    = "Sturdy antique night vision goggles of unknown origin. Wired to run on modern micro-cells."
  , ikit     = []
  }
sightSharpening = ItemKind
  { isymbol  = symbolRing
  , iname    = "Autozoom Contact Lens"
  , ifreq    = [("treasure", 10), ("add sight", 1)]  -- not unique, so very rare
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(7, 1), (10, 5)]
  , iverbHit = "rap"
  , iweight  = 50
  , idamage  = toDmg 0
  , iaspects = [AddSight $ 1 + 1 `d` 2, AddHurtMelee $ (1 `d` 2) * 3]
  , ieffects = [EqpSlot EqpSlotAddSight]
  , ifeature = [Precious, Identified, Durable, Equipable]
  , idesc    = "Zooms on any movement, distant or close. Requires some getting used to. Never needs to be taken off."
  , ikit     = []
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and inv or even activating a wrong item by mistake.
--
-- However, rings have the explosion effect.
-- They explode on use (and throw), for the fun of hitting everything
-- around without the risk of being hit. In case of teleportation explosion
-- this can also be used to immediately teleport close friends, as opposed
-- to throwing the ring, which takes time.
--
-- Rings should have @Identified@, so that they fully identify upon picking up.
-- Effects of many of them are seen in character sheet, so it would be silly
-- not to identify them. Necklaces provide the fun of id-by-use, because they
-- have effects and when they are triggered, they id.
ring = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [("useful", 100), ("any jewelry", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "knock"
  , iweight  = 15
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Precious, Identified, Equipable]
  , idesc    = "A sturdy ring with a softly shining eye. If it contains a body booster unit, beware of the side-effects."
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(10, 2)]
  , iaspects = [AddSpeed $ 1 `d` 3, AddMaxHP (-15)]
  , ieffects = [ Explode "distortion"  -- high power
               , EqpSlot EqpSlotAddSpeed ]
  }
ring2 = ring
  { ifreq    = [("ship", 100), ("any jewelry", 100)]
  , irarity  = [(10, 2)]
  , iaspects = [AddSpeed $ (1 `d` 2) * 3, AddMaxCalm (-40), AddMaxHP (-20)]
  , ieffects = [ Unique, ELabel "of Rush"  -- no explosion, because Durable
               , EqpSlot EqpSlotAddSpeed ]
  , ifeature = Durable : ifeature ring
  -- , idesc    = ""
  }
ring3 = ring
  { irarity  = [(10, 8)]
  , iaspects = [ AddMaxHP $ 10 + (1 `dL` 5) * 2
               , AddMaxCalm $ -20 + (1 `dL` 5) * 2 ]
  , ieffects = [Explode "blast 20", EqpSlot EqpSlotAddMaxHP]
  }
ring4 = ring
  { irarity  = [(5, 1), (10, 10)]  -- needed after other rings drop Calm
  , iaspects = [AddMaxCalm $ 25 + (1 `dL` 4) * 5]
  , ieffects = [Explode "blast 20", EqpSlot EqpSlotMiscBonus]
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring5 = ring
  { irarity  = [(3, 3), (10, 3)]
  , iaspects = [ AddHurtMelee $ (2 + 1 `d` 2 + (1 `dL` 2) * 2 ) * 3
               , AddMaxHP $ (-2 - (1 `d` 2) + (1 `dL` 2) * 2) * 3 ]  -- !!!
  , ieffects = [Explode "blast 20", EqpSlot EqpSlotAddHurtMelee]
  }
ring6 = ring  -- by the time it's found, probably no space in eqp
  { irarity  = [(5, 0), (10, 2)]
  , iaspects = [AddShine $ 1 `d` 2]
  , ieffects = [ Explode "distortion"  -- high power
               , EqpSlot EqpSlotLightSource ]
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring7 = ring
  { ifreq    = [("useful", 10), ("ring of opportunity sniper", 1) ]
  , irarity  = [(10, 5)]
  , iaspects = [AddAbility AbProject 8]
  , ieffects = [ ELabel "of opportunity sniper"
               , Explode "distortion"  -- high power
               , EqpSlot EqpSlotAbProject ]
  }
ring8 = ring
  { ifreq    = [("useful", 1), ("ring of opportunity grenadier", 1) ]
  , irarity  = [(1, 1)]
  , iaspects = [AddAbility AbProject 11]
  , ieffects = [ ELabel "of opportunity grenadier"
               , Explode "distortion"  -- high power
               , EqpSlot EqpSlotAbProject ]
  }

-- * Armor

armorLeather = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "spacesuit breastplate"
  , ifreq    = [("useful", 100), ("torso armor", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 7000
  , idamage  = toDmg 0
  , iaspects = [ AddHurtMelee (-2)
               , AddArmorMelee $ (1 + 1 `dL` 4) * 5
               , AddArmorRanged $ (1 + 1 `dL` 2) * 3 ]
  , ieffects = [EqpSlot EqpSlotAddArmorMelee]
  , ifeature = [Durable, Identified, Equipable]
  , idesc    = "A hard-shell torso segment cut from a disposed off spacesuit."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "bulletproof vest"
  , ifreq    = [("useful", 100), ("torso armor", 1), ("armor ranged", 50) ]
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , idamage  = toDmg 0
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddArmorRanged $ (2 + 1 `dL` 2) * 6 ]
  , ieffects = [EqpSlot EqpSlotAddArmorRanged]
  , ifeature = [Durable, Identified, Equipable]
  , idesc    = "A civilian bulletproof vest. Discourages foes from attacking your torso, making it harder for them to land a blow."
  }
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "construction glove"
  , ifreq    = [("useful", 100), ("armor ranged", 50)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [ AddHurtMelee $ (3 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddArmorRanged $ (1 + 1 `dL` 2) * 3 ]
  , ieffects = [EqpSlot EqpSlotAddHurtMelee]
  , ifeature = [ toVelocity 50  -- flaps and flutters
               , Durable, Identified, Equipable ]
  , idesc    = "A flexible construction glove from rough leather ensuring a good grip. Also, quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "spacesuit glove"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 9), (10, 3)]
  , iweight  = 300
  , idamage  = toDmg $ 2 `d` 1
  , iaspects = [AddArmorMelee $ (2 + 1 `dL` 2) * 5]
  , ieffects = [EqpSlot EqpSlotAddArmorMelee]
  , idesc    = "A piece of a hull maintenance spacesuit, padded and reinforced with carbon fibre."
  }
gloveJousting = gloveFencing
  { iname    = "Welding Handgear"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iweight  = 1000
  , idamage  = toDmg $ 3 `d` 1
  , iaspects = [ AddHurtMelee $ (-6 + 1 `dL` 5) * 3
               , AddArmorMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 5
               , AddArmorRanged $ (1 + 1 `dL` 2) * 3 ]
                 -- very random on purpose and can even be good on occasion
  , ieffects = [Unique, EqpSlot EqpSlotAddArmorMelee]
  , idesc    = "Rigid, bulky handgear embedding a welding equipment, complete with an affixed small shield and a darkened visor. Awe-inspiring."
  }

-- * Shields

-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
-- Note that AI will pick them up but never wear and will use them at most
-- as a way to push itself (but they won't recharge, not being in eqp).
-- Being @Meleeable@ they will not be use as weapons either.
-- This is OK, using shields smartly is totally beyond AI.
buckler = ItemKind
  { isymbol  = symbolShield
  , iname    = "buckler"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 6)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = [(96, 2 `d` 1), (3, 4 `d` 1), (1, 8 `d` 1)]
  , iaspects = [ AddArmorMelee 40  -- not enough to compensate; won't be in eqp
               , AddHurtMelee (-30)  -- too harmful; won't be wielded as weapon
               , Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 2 ]
  , ieffects = [ Recharging (PushActor (ThrowMod 200 50))
               , EqpSlot EqpSlotAddArmorMelee ]
  , ifeature = [ toVelocity 50  -- unwieldy to throw
               , Durable, Identified, Meleeable ]
  , idesc    = "Heavy and unwieldy arm protection made from an outer airlock panel. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(8, 3)]
  , iflavour = zipPlain [Green]
  , iweight  = 3000
  , idamage  = [(96, 4 `d` 1), (3, 8 `d` 1), (1, 16 `d` 1)]
  , iaspects = [ AddArmorMelee 80  -- not enough to compensate; won't be in eqp
               , AddHurtMelee (-70)  -- too harmful; won't be wielded as weapon
               , Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 4 ]
  , ieffects = [ Recharging (PushActor (ThrowMod 400 50))
               , EqpSlot EqpSlotAddArmorMelee ]
  , ifeature = [ toVelocity 50  -- unwieldy to throw
               , Durable, Identified, Meleeable ]
  , idesc    = "Large and unwieldy rectangle made of anti-meteorite ceramic sheet. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = symbolEdged
  , iname    = "cleaver"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(3 * 10/12, 50), (4 * 10/12, 1)]
                 -- no weapons brought by aliens, initially, so cleaver common
  , iverbHit = "stab"
  , iweight  = 1000
  , idamage  = toDmg $ 6 `d` 1
  , iaspects = [ AddHurtMelee $ (1 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddArmorMelee $ (1 `d` 2) * 5 ]
                   -- very common, so don't make too random
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Identified, Meleeable ]
  , idesc    = "A heavy professional kitchen blade. Will do fine cutting any kind of meat and bone, as well as parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { iname    = "Double Dagger"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(1, 1), (10, 4)]
  -- Here timeout has to be small, if the player is to count on the effect
  -- occuring consistently in any longer fight. Otherwise, the effect will be
  -- absent in some important fights, leading to the feeling of bad luck,
  -- but will manifest sometimes in fights where it doesn't matter,
  -- leading to the feeling of wasted power.
  -- If the effect is very powerful and so the timeout has to be significant,
  -- let's make it really large, for the effect to occur only once in a fight:
  -- as soon as the item is equipped, or just on the first strike.
  -- Here the timeout is either very small or very large, randomly.
  -- In the latter case the weapon is best swapped for a stronger one
  -- later on in the game, but provides some variety at the start.
  , iaspects = iaspects dagger ++ [Timeout $ (1 `d` 2) * 20 - 16]
  , ieffects = ieffects dagger
               ++ [ Unique
                  , Recharging DropBestWeapon, Recharging $ RefillCalm (-3) ]
  , idesc    = "A knife with a forked blade that a focused fencer can use to catch and twist an opponent's weapon occasionally."
  }
hammer = ItemKind
  { isymbol  = symbolHafted
  , iname    = "demolition hammer"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipFancy [BrMagenta]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(3 * 10/12, 3), (5, 20), (8, 1)]
                 -- don't make it too common on lvl 3
  , iverbHit = "club"
  , iweight  = 1600
  , idamage  = [(96, 8 `d` 1), (3, 12 `d` 1), (1, 16 `d` 1)]
  , iaspects = [AddHurtMelee $ (1 `d` 2 + 1 `dL` 2) * 3]
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Identified, Meleeable ]
  , idesc    = "A hammer on a long handle used for construction work. It may not cause grave wounds, but neither does it ricochet or glance off armor. Great sidearm for opportunistic blows against armored foes."
  , ikit     = []
  }
hammerParalyze = hammer
  { iname    = "Concussion Hammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , idamage  = toDmg $ 8 `d` 1
  , iaspects = iaspects hammer ++ [Timeout 7]
  , ieffects = ieffects hammer ++ [Unique, Recharging $ Paralyze 10]
  -- , idesc    = ""
  }
hammerSpark = hammer
  { iname    = "Grand Smithhammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , idamage  = toDmg $ 12 `d` 1
  , iaspects = iaspects hammer ++ [AddShine 3, Timeout 10]
  , ieffects = ieffects hammer ++ [Unique, Recharging $ Explode "spark"]
  -- , idesc    = ""
  }
sword = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "sharpened pipe"
  , ifreq    = [("useful", 100), ("starting weapon", 10)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(4, 1), (5, 15)]
  , iverbHit = "slash"
  , iweight  = 2000
  , idamage  = toDmg $ 10 `d` 1
  , iaspects = []
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Identified, Meleeable ]
  , idesc    = "A makeshift weapon of simple design, but great potential. Hard to master, though."
  , ikit     = []
  }
swordImpress = sword
  { isymbol  = symbolEdged
  , iname    = "Master's Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , iaspects = [Timeout $ (1 `d` 2) * 40 - 30]
  , ieffects = ieffects sword ++ [Unique, Recharging Impress]
  }
swordNullify = sword
  { isymbol  = symbolEdged
  , iname    = "Gutting Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 6)]
  , iaspects = [Timeout 10]
  , ieffects = ieffects sword
               ++ [ Unique
                  , Recharging
                    $ DropItem 1 maxBound COrgan "temporary condition"
                  , Recharging $ RefillCalm (-10) ]
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "pole cleaver"
  , ifreq    = [("useful", 100), ("starting weapon", 20)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(8, 1), (9, 40)]
  , iverbHit = "impale"
  , iweight  = 3000
  , idamage  = [(96, 12 `d` 1), (3, 18 `d` 1), (1, 24 `d` 1)]
  , iaspects = [ AddHurtMelee (-20)  -- useless against armor at game start
               , AddArmorMelee $ (1 + 1 `dL` 3) * 5 ]
  , ieffects = [EqpSlot EqpSlotWeapon]
  , ifeature = [ toVelocity 20  -- not balanced
               , Durable, Identified, Meleeable ]
  , idesc    = "An improvised but deadly weapon made of a long, sharp kitchen knife glued and bound to a long pole."
  , ikit     = []
  }
halberdPushActor = halberd
  { iname    = "Swiss Halberd"
  , ifreq    = [("ship", 20)]
  , irarity  = [(8, 1), (9, 20)]
  , idamage  = toDmg $ 12 `d` 1
  , iaspects = iaspects halberd ++ [Timeout $ (1 `d` 2) * 10]
  , ieffects = ieffects halberd
               ++ [Unique, Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = "A perfect replica made for a reenactor troupe, missing only some sharpening. Versatile, with great reach and leverage. Foes are held at a distance."
  }

-- * Wands

wand = ItemKind
  { isymbol  = symbolWand
  , iname    = "injector"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , irarity  = []
  , iverbHit = "club"
  , iweight  = 300
  , idamage  = toDmg 0
  , iaspects = [AddShine 1, AddSpeed (-1)]  -- pulsing with power, distracts
  , ieffects = []
  , ifeature = [ toVelocity 125  -- sufficiently advanced tech
               , Applicable, Durable ]
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."
  , ikit     = []
  }
wand1 = wand
  { ieffects = []  -- will be: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '|' and '{'
  }
wand2 = wand
  { ieffects = []
  }

-- * Treasure

gem = ItemKind
  { isymbol  = symbolGold
  , iname    = "gem"
  , ifreq    = [("treasure", 100), ("gem", 100), ("any jewelry", 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , idamage  = toDmg 0
  , iaspects = [AddShine 1, AddSpeed (-1)]
                 -- reflects strongly, distracts; so it glows in the dark,
                 -- is visible on dark floor, but not too tempting to wear
  , ieffects = [RefillCalm (-1)]  -- minor effect to ensure no id-on-pickup
  , ifeature = [Precious]  -- no @Identified@, so kind not known
  , idesc    = "Precious, though useless. Worth around 100 gold grains."
  , ikit     = []
  }
gem1 = gem
  { irarity  = [(3, 0), (10, 24)]
  }
gem2 = gem
  { irarity  = [(5, 0), (10, 28)]
  }
gem3 = gem
  { irarity  = [(7, 0), (10, 32)]
  }
gem4 = gem
  { irarity  = [(9, 0), (10, 100)]
  }
gem5 = gem
  { isymbol  = symbolSpecial
  , iname    = "stimpack"
  , iflavour = zipPlain [BrYellow]
  , irarity  = [(1, 40), (10, 40)]
  , iaspects = []
  , ieffects = [ELabel "of youth", RefillCalm 5, RefillHP 15]
  , ifeature = [Identified, Applicable, Precious]
  , idesc    = "Calms, heals, invigorates and rejuvenates at the same time. No side-effects. As valuable as precious gems, at 100 gold grains each."
  }
currency = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold grain"
  , ifreq    = [("treasure", 100), ("currency", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dL` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 1
  , idamage  = toDmg 0
  , iaspects = []
  , ieffects = []
  , ifeature = [Identified, Precious]
  , idesc    = "Reliably valuable in every civilized place."
  , ikit     = []
  }

-- * Allure-specific

needle = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "needle"
  , ifreq    = [("needle", 1)]  -- special; TODO: fast when fired, not thrown
  , iflavour = zipPlain [BrBlue]
  , icount   = 9 `d` 3
  , irarity  = [(1, 1)]
  , iverbHit = "prick"
  , iweight  = 3
  , idamage  = toDmg $ 1 `d` 1
  , iaspects = [AddHurtMelee $ -10 * 5]
  , ieffects = []
  , ifeature = [toVelocity 70, Fragile]
  , idesc    = "A long hypodermic needle ending in a dried out micro-syringe. It's too light to throw hard, but it penetrates deeply, causing intense pain on movement."
  , ikit     = []
  }
constructionHooter = scroll
  { iname    = "construction hooter"
  , ifreq    = [("useful", 1), ("construction hooter", 1)]  -- extremely rare
  , iflavour = zipPlain [BrRed]
  , irarity  = [(1, 1)]
  , iaspects = []
  , ieffects = [Summon "construction robot" $ 1 `dL` 2]
  , ifeature = Identified : ifeature scroll
  , idesc    = "The single-use electronic overdrive hooter that construction robots use to warn about danger and call help in extreme emergency."
  , ikit     = []
  }
scroll14 = scroll
  { irarity  = [(1, 2), (10, 2)]  -- not every playthrough needs it
  , ieffects = [ Unique, ELabel "Displaying a Happy Couple"
               , toOrganActorTurn "resolute" (500 + 1 `d` 200)
                   -- a drawback (at least initially) due to @calmEnough@
               , Explode "cruise ad hologram" ]
  , idesc    = "Biodegrable self-powered mini-projector displaying a holographic ad for an inteplanetary cruise."
  }
