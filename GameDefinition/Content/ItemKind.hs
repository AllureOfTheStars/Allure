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
import Content.RuleKind
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Container
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind

content :: [ItemKind]
content = items ++ otherItemContent

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
  [sandstoneRock, dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, buckler, shield, shield2, shield3, dagger, daggerDropBestWeapon, hammer, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberdPushActor, wandTemplate, wand1, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency]
  -- Allure-specific
  ++ [needle, constructionHooter, wasteContainer, spotlight, scrollAd1, blowtorch]

sandstoneRock,    dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, buckler, shield, shield2, shield3, dagger, daggerDropBestWeapon, hammer, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberdPushActor, wandTemplate, wand1, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency :: ItemKind
-- Allure-specific
needle, constructionHooter, wasteContainer, spotlight, scrollAd1, blowtorch :: ItemKind

-- Keep the dice rolls and sides in aspects small so that not too many
-- distinct items are generated (for display in item lore and for narrative
-- impact ("oh, I found the more powerful of the two variants of the item!",
-- instead of "hmm, I found one of the countless variants, a decent one").
-- In particular, for unique items, unless they inherit aspects from
-- a standard item, permit only a couple possible variants.
-- This is especially important if an item kind has mulitple random aspects.
-- Instead multiply dice results, e.g., (1 `d` 3) * 5 instead of 1 `d` 15.
--
-- Beware of non-periodic non-weapon durable items with beneficial effects
-- and low timeout -- AI will starve applying such an item incessantly.

-- * Item group symbols, from Angband and variants

symbolProjectile, _symbolLauncher, symbolLight, symbolTool, symbolSpecial, symbolGold, symbolNecklace, symbolRing, symbolPotion, symbolFlask, symbolScroll, symbolTorsoArmor, symbolMiscArmor, _symbolClothes, symbolShield, symbolPolearm, symbolEdged, symbolHafted, symbolWand, _symbolStaff, symbolFood :: Char

symbolProjectile = rsymbolProjectile standardRules  -- '{'
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
symbolWand       = '-'  -- magical rod, transmitter, pistol, rifle, instrument
_symbolStaff     = '_'  -- scanner
symbolFood       = ','  -- also body part; distinct from floor: not middle dot

-- * Generic items, for any epoch (some may be removed for Allure)

-- ** Thrown weapons

sandstoneRock = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "ceramic foam splinter"
  , ifreq    = [("sandstone rock", 1)]
  , iflavour = zipPlain [Green]
  , icount   = 1 + 1 `d` 2  -- > 1, to let AI ignore sole pieces
  , irarity  = [(1, 50), (10, 1)]
  , iverbHit = "hit"
  , iweight  = 300
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ -16 * 5
               , SetFlag Fragile
               , toVelocity 70 ] -- not dense, irregular
  , ieffects = []
  , idesc    = "A light, irregular lump of ceramic foam used in construction."
  , ikit     = []
  }
dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "billiard ball"
  , ifreq    = [("common item", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 15), (10, 10)]
  , iverbHit = "strike"
  , iweight  = 170
  , idamage  = 1 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ (-15 + 1 `d` 2 + 1 `dL` 3) * 5]
                 -- only good against leather
  , ieffects = []
  , idesc    = "Ideal shape, size and weight for throwing."
  , ikit     = []
  }
spike = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "steak knife"
  , ifreq    = [("common item", 100), ("any arrow", 50), ("weak arrow", 50)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 10), (10, 15)]
  , iverbHit = "nick"
  , iweight  = 100
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy vs armor
               , SetFlag MinorEffects
               , toVelocity 70 ]  -- hitting with tip costs speed
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
      -- this results in a wordy item synopsis, but it's OK, the spark really
      -- is useful in some situations, not just a flavour
  , idesc    = "Not particularly well balanced, but with a laser-sharpened titanium tip and blade."
  , ikit     = []
  }
spike2 = spike
  { iname    = "skewer"
  , ifreq    = [("common item", 2), ("any arrow", 1), ("weak arrow", 1)]
  , icount   = 6 `dL` 5
  , iweight  = 150
  , idamage = 4 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
               , SetFlag MinorEffects
               , Odds (10 * 1 `dL` 10) [] [toVelocity 70] ]
                   -- at deep levels sometimes even don't limit velocity
  , idesc    = "A jagged skewer of rusty metal."
  }
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "steel hex nut"
  , ifreq    = [("common item", 5), ("any arrow", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1 + 3 `dL` 4
  , irarity  = [(1, 1), (10, 20)]
  , iverbHit = "hit"
  , iweight  = 200
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy, to bludgeon through armor
               , SetFlag MinorEffects
               , toVelocity 150 ]
  , ieffects = [ Explode "single spark"  -- when hitting enemy
               , OnSmash (Explode "single spark") ]  -- at wall hit
  , idesc    = "A large hexagonal fastening nut; due to its angular shape, securely lodging in the pouch of a makeshift string and cloth sling."
  , ikit     = []
  }
slingBullet = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bearing ball"
  , ifreq    = [("common item", 5), ("any arrow", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1 + 6 `dL` 4
  , irarity  = [(1, 1), (10, 15)]
  , iverbHit = "hit"
  , iweight  = 28
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-17 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- not too good against armor
               , ToThrow $ ThrowMod 200 100 3 ]  -- piercing
  , ieffects = []
  , idesc    = "Small but heavy bearing ball. Thanks to its size and shape, it doesn't snag when released from the makeshift sling's pouch."
  , ikit     = []
  }

-- ** Exotic thrown weapons

-- Identified, because shape (and name) says it all. Detailed aspects id by use.
-- This is an extremely large value for @Paralyze@. Normally for such values
-- we should instead use condition that disables (almost) all stats,
-- except @SkWait@, so that the player can switch leader and not be
-- helpless nor experience instadeath (unless his party is 1-person
-- or the actor is isolated, but that's usually player's fault).
paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "can"
  , ifreq    = [("common item", 100), ("can of sticky foam", 1)]
  , iflavour = zipPlain [Magenta]
  , icount   = 1 `dL` 4
  , irarity  = [(5, 5), (10, 20)]
  , iverbHit = "glue"
  , iweight  = 1000
  , idamage  = 1 `d` 1
  , iaspects = [ ELabel "of sticky foam"
               , AddSkill SkHurtMelee $ -14 * 5
               , SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- unwieldy
  , ieffects = [Paralyze 15, OnSmash (Explode "glue") ]
  , idesc    = "A can of liquid, fast-setting construction foam."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [("curious item", 100), ("harpoon", 100), ("museum", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 5
  , irarity  = [(7, 5), (10, 5)]
  , iverbHit = "hook"
  , iweight  = 750
  , idamage  = 5 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5]
  , ieffects = [ PullActor (ThrowMod 200 50 1)  -- 1 step, fast
               , Yell ]  -- yell, because brutal
  , idesc    = "A display piece harking back to the Earth's oceanic tourism heyday. The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
harpoon2 = harpoon
  { iname    = "heavy harpoon"
  , ifreq    = [("curious item", 5), ("harpoon", 2)]
  , icount   = 2 `dL` 5
  , iweight  = 1000
  , idamage  = 10 `d` 1
  , idesc    = "A sharpened cargo-hook with high-tension cord."
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1 `dL` 3
  , irarity  = [(5, 5), (10, 7)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , idamage  = 2 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ -14 * 5]
  , ieffects = [ toOrganBad "slowed" (3 + 1 `d` 3)
               , DropItem maxBound 1 CEqp "torso armor"
                   -- only one of each kind is dropped, because no rubbish
                   -- in this group and so no risk of exploit
               , SendFlying (ThrowMod 100 50 1) ]  -- 1 step; painful
  , idesc    = "A large synthetic fibre net with weights affixed along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }

-- ** Lights

light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "torch"
  , ifreq    = [ ("common item", 100), ("light source", 100)
               , ("wooden torch", 1) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 4
  , irarity  = [(1, 15)]
  , iverbHit = "scorch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine 3, AddSkill SkSight (-2)
                   -- not only flashes, but also sparks,
                   -- so unused by AI due to the mixed blessing
               , SetFlag Lobable, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
                   -- not Fragile; reusable flare
  , ieffects = [Burn 1]
  , idesc    = "A puttering torch improvised with polymer sheets soaked in lubricant on a stick."
  , ikit     = []
  }
light2 = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [("common item", 100), ("light source", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1 `dL` 2
  , irarity  = [(6, 10)]
  , iverbHit = "burn"
  , iweight  = 1500
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkShine 3, AddSkill SkSight (-1)
               , SetFlag Lobable, SetFlag Fragile, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
  , ieffects = [ Burn 1
               , toOrganBad "pacified" (3 + 1 `d` 2)
               , OnSmash (Explode "burning oil 3") ]
  , idesc    = "A sizable restaurant glass lamp filled with plant oil feeding a slender wick."
  , ikit     = []
  }
light3 = ItemKind
  { isymbol  = symbolLight
  , iname    = "crank spotlight"
  , ifreq    = [("common item", 100), ("light source", 100)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(5, 1), (10, 5)]
  , iverbHit = "snag"
  , iweight  = 3000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine 4
               , AddSkill SkArmorRanged $ - 1 `d` 3  -- noise & distraction
               , SetFlag Equipable, EqpSlot EqpSlotShine ]
  , ieffects = []
  , idesc    = "Powerful, wide-beam spotlight, powered by a hand-crank. Requires noisy two-handed recharging every few minutes."
  , ikit     = []
  }
blanket = ItemKind
  { isymbol  = symbolLight
  , iname    = "mineral fibre blanket"
  , ifreq    = [("common item", 100), ("light source", 100), ("blanket", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]  -- not every playthrough needs one
  , iverbHit = "swoosh"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine (-10)
               , AddSkill SkArmorMelee 2, AddSkill SkMaxCalm 5
               , SetFlag Lobable, SetFlag Equipable ]
                   -- not Fragile; reusable douse implement;
                   -- douses torch, lamp and lantern in one action,
                   -- both in equipment and when thrown at the floor
  , ieffects = []
  , idesc    = "Flame-retardant synthetic fibres."
  , ikit     = []
  }

-- ** Exploding consumables, often intended to be thrown.

-- Not identified, because they are perfect for the id-by-use fun,
-- due to effects. They are fragile and upon hitting the ground explode
-- for effects roughly corresponding to their normal effects.
-- Whether to hit with them or explode them close to the tartget
-- is intended to be an interesting tactical decision.
--
-- Flasks are often not natural; maths, magic, distillery.
-- In fact, they just cover all conditions, except those for stats.
--
-- There is no flask nor condition of Calm depletion,
-- because Calm reduced often via combat, etc.

flaskTemplate = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [("flask unknown", 1)]
  , iflavour = zipGlassPlain darkCol ++ zipGlassFancy darkCol
               ++ zipLiquid darkCol
                 -- ++ zipPlain darkCol ++ zipFancy darkCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 7), (10, 3)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ HideAs "flask unknown", SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- oily, bad grip
  , ieffects = []
  , idesc    = "A flask of oily liquid of a suspect color. Something seems to be moving inside. Double dose causes twice longer effect."
  , ikit     = []
  }
flask1 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , icount   = 1 `dL` 5
  , irarity  = [(10, 7)]
  , iaspects = ELabel "of strength renewal brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "strengthened" (20 + 1 `d` 5)
               , toOrganNoTimer "regenerating"
               , OnSmash (Explode "dense shower") ]
  }
flask2 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , iaspects = ELabel "of weakness brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganBad "weakened" (20 + 1 `d` 5)
               , OnSmash (Explode "sparse shower") ]
  }
flask3 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , iaspects = ELabel "of melee protective balm"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "protected from melee" (20 + 1 `d` 5)
               , OnSmash (Explode "melee protective balm") ]
  }
flask4 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , iaspects = ELabel "of ranged protective balm"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "protected from ranged" (20 + 1 `d` 5)
               , OnSmash (Explode "ranged protective balm") ]
  }
flask5 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , iaspects = ELabel "of fluorescent paint"
               : iaspects flaskTemplate
  , ieffects = [ toOrganBad "painted" (20 + 1 `d` 5)
               , OnSmash (Explode "fluorescent paint") ]
  }
flask6 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(1, 1)]  -- not every playthrough needs one
  , iaspects = ELabel "of resolution"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "resolute" (500 + 1 `d` 200)  -- long, for scouting
               , RefillCalm 60  -- not to make it a drawback, via @calmEnough@
               , OnSmash (Explode "resolution dust") ]
  }
flask7 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , icount   = 1  -- too poweful en masse
  , iaspects = ELabel "of haste brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "hasted" (20 + 1 `d` 5)
               , OnSmash (Explode "haste spray") ]
  }
flask8 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , iaspects = ELabel "of eye drops"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "far-sighted" (40 + 1 `d` 10)
               , OnSmash (Explode "eye drop") ]
  }
flask9 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 2)]  -- not very useful right now
  , iaspects = ELabel "of smelly concoction"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "keen-smelling" (40 + 1 `d` 10)
               , Detect DetectActor 10  -- make it at least slightly useful
               , OnSmash (Explode "smelly droplet") ]
  }
flask10 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 2)]  -- not very useful right now
  , iaspects = ELabel "of cat tears"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "shiny-eyed" (40 + 1 `d` 10)
               , OnSmash (Explode "eye shine") ]
  }
flask11 = flaskTemplate
  { iname    = "bottle"
  , ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , icount   = 1 `d` 3  -- the only one sometimes giving away its identity
  , iaspects = ELabel "of whiskey"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3, Yell
               , OnSmash (Explode "whiskey spray") ]
  }
flask12 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , icount   = 1
  , iaspects = ELabel "of bait cocktail"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "drunk" (20 + 1 `d` 5)
               , Burn 1, RefillHP 3  -- mild exploit possible, good
               , Summon "mobile animal" 1
               , OnSmash (Summon "mobile animal" 1)
               , OnSmash Impress
               , OnSmash (Explode "waste") ]
  }
-- The player has full control over throwing the flask at his party,
-- so he can milk the explosion, so it has to be much weaker, so a weak
-- healing effect is enough. OTOH, throwing a harmful flask at many enemies
-- at once is not easy to arrange, so these explosions can stay powerful.
flask13 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(1, 2), (10, 10)]
  , iaspects = ELabel "of regeneration brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood "rose-smelling" (80 + 1 `d` 20)
               , toOrganNoTimer "regenerating"
               , toOrganNoTimer "regenerating"  -- x2
               , OnSmash (Explode "youth sprinkle") ]
  }
flask14 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , iaspects = ELabel "of poison"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer "poisoned", toOrganNoTimer "poisoned"  -- x2
               , OnSmash (Explode "poison cloud") ]
  }
flask15 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 3)]
  , iaspects = ELabel "of slow resistance"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer "slow resistant"
               , OnSmash (Explode "anti-slow mist") ]
  }
flask16 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , irarity  = [(10, 3)]
  , iaspects = ELabel "of poison resistance"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer "poison resistant"
               , OnSmash (Explode "antidote mist") ]
  }
flask17 = flaskTemplate
  { ifreq    = [("common item", 100), ("explosive", 100), ("any vial", 100)]
  , iaspects = ELabel "of calamity"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer "poisoned"
               , toOrganBad "weakened" (20 + 1 `d` 5)
               , toOrganBad "painted" (20 + 1 `d` 5)
               , OnSmash (Explode "glass hail") ]  -- enough glass to cause that
  }

-- Potions are often natural, including natural stats.
-- They appear deeper than most flasks. Various configurations of effects.
-- A different class of effects is on scrolls and mechanical items.
-- Some are shared.

potionTemplate = ItemKind
  { isymbol  = symbolPotion
  , iname    = "vial"
  , ifreq    = [("potion unknown", 1)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "splash"
  , iweight  = 200
  , idamage  = 0
  , iaspects = [ HideAs "potion unknown", SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- oily, bad grip
  , ieffects = []
  , idesc    = "A vial of bright, frothing concoction. The best that nature has to offer."
  , ikit     = []
  }
potion1 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1  -- very useful, despite appearances
  , iaspects = ELabel "of rose water"
               : iaspects potionTemplate
  , ieffects = [ Impress, toOrganGood "rose-smelling" (80 + 1 `d` 20)
               , OnSmash ApplyPerfume, OnSmash (Explode "fragrance") ]
  }
potion2 = potionTemplate
  { ifreq    = [("curious item", 100), ("any vial", 100)]
  , icount   = 1
  , irarity  = [(5, 8), (10, 8)]
  , iaspects = [ SetFlag Unique, ELabel "of Attraction"
               , SetFlag Precious, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- identified
  , ieffects = [ Dominate
               , toOrganGood "hasted" (20 + 1 `d` 5)
               , OnSmash (Explode "pheromone")
               , OnSmash (Explode "haste spray") ]
  , idesc    = "The liquid fizzes with energy."
  }
potion3 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , ieffects = [ RefillHP 5, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist") ]
  }
potion4 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , irarity  = [(1, 6), (10, 9)]
  , ieffects = [ RefillHP 10, DropItem 1 maxBound COrgan "poisoned"
               , OnSmash (Explode "healing mist 2") ]
  }
potion5 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1  -- always as many as possible on this level
                         -- without giving away potion identity
  , irarity  = [(1, 10)]
  , ieffects = [ OneOf [ RefillHP 10, RefillHP 5, Burn 5
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganGood "strengthened" (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode "dense shower"
                                , Explode "sparse shower"
                                , Explode "melee protective balm"
                                , Explode "ranged protective balm"
                                , Explode "fluorescent paint" ]) ]
  }
potion6 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 8)]
  , ieffects = [ Impress
               , OneOf [ RefillHP 20, RefillHP 10, Burn 10
                       , DropItem 1 maxBound COrgan "poisoned"
                       , toOrganGood "hasted" (20 + 1 `d` 5)
                       , toOrganBad "impatient" (2 + 1 `d` 2) ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "wounding mist"
                                , Explode "distressing odor"
                                , Explode "impatient mist"
                                , Explode "haste spray"
                                , Explode "slowness mist"
                                , Explode "fragrance"
                                , Explode "violent flash" ]) ]
  }
potion7 = potionTemplate
  { iname    = "ampoule"  -- filled with semi-stabilized high explosive liquid
  , ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , icount   = 3 `dL` 1
  , ieffects = [ DropItem 1 maxBound COrgan "condition"
               , OnSmash (Explode "violent concussion") ]
      -- not fragmentation nor glass hail, because not enough glass
  }
potion8 = potionTemplate
  { ifreq    = [("curious item", 100), ("any vial", 100)]
  , icount   = 1
  , irarity  = [(10, 5)]
  , iaspects = [ SetFlag Unique, ELabel "of Love"
               , SetFlag Precious, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- identified
  , ieffects = [ RefillHP 60, RefillCalm (-60)
               , toOrganGood "rose-smelling" (80 + 1 `d` 20)
               , OnSmash (Explode "healing mist 2")
               , OnSmash (Explode "distressing odor") ]
  , idesc    = "Perplexing swirls of intense, compelling colour."
  }
potion9 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , irarity  = [(10, 5)]
  , iaspects = ELabel "of grenadier focus"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood "more projecting" (40 + 1 `d` 10)
               , toOrganBad "pacified" (5 + 1 `d` 3)
                   -- the malus has to be weak, or would be too good
                   -- when thrown at foes
               , OnSmash (Explode "more projecting dew")
               , OnSmash (Explode "pacified mist") ]
  , idesc    = "Thick, sluggish fluid with violently-bursting bubbles."
  }
potion10 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of frenzy"
               : iaspects potionTemplate
  , ieffects = [ Yell
               , toOrganGood "strengthened" (20 + 1 `d` 5)
               , toOrganBad "retaining" (40 + 1 `d` 10)
               , toOrganBad "frenzied" (40 + 1 `d` 10)
               , OnSmash (Explode "dense shower")
               , OnSmash (Explode "retaining mist")
               , OnSmash (Explode "retaining mist") ]
  }
potion11 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of panic"
               : iaspects potionTemplate
  , ieffects = [ RefillCalm (-30)
               , toOrganGood "hasted" (20 + 1 `d` 5)
               , toOrganBad "weakened" (20 + 1 `d` 5)
               , toOrganBad "withholding" (20 + 1 `d` 5)
               , OnSmash (Explode "haste spray")
               , OnSmash (Explode "sparse shower")
               , OnSmash (Explode "withholding mist") ]
  }
potion12 = potionTemplate
  { ifreq    = [("common item", 100), ("potion", 100), ("any vial", 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of quicksilver"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood "hasted" (20 + 1 `d` 5)
               , toOrganBad "blind" (20 + 1 `d` 5)
               , toOrganBad "immobile" (20 + 1 `d` 5)
               , OnSmash (Explode "haste spray")
               , OnSmash (Explode "iron filing")
               , OnSmash (Explode "immobile mist") ]
  }

-- ** Explosives, with the only effect being @Explode@

fragmentationBomb = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "hand bomb"
      -- improvised bomb filled with iron pellets, nuts, cut nails;
      -- deflagration, not detonation, so large mass and hard container
      -- required not to burn harmlessly; improvised short fuze;
      -- can't be more powerful or would fracture the spaceship's hull
  , ifreq    = [("common item", 100), ("explosive", 200)]
  , iflavour = zipPlain [Red]
  , icount   = 1 `dL` 5  -- many, because not very intricate
  , irarity  = [(5, 7), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 3000  -- low velocity due to weight
  , idamage  = 1 `d` 1  -- heavy and hard
  , iaspects = [SetFlag Lobable, SetFlag Fragile]
  , ieffects = [ Explode "focused fragmentation"
               , OnSmash (Explode "violent fragmentation") ]
  , idesc    = "Shards of brittle metal packed around an explosive core."
  , ikit     = []
  }
concussionBomb = fragmentationBomb
  { iname    = "canister"
      -- slightly stabilized liquid explosive in a soft container, hence
      -- no fragmentation, but huge shock wave despite small size and lack
      -- of strong container to build up pressure; indoors help the shock wave;
      -- unstable enough that no fuze required (or simple electric fuse?);
      -- that's the most potent explosive (a detonating one) to be found
      -- and only in small quantities, due to depressurization hazard
  , iflavour = zipPlain [Magenta]
  , iverbHit = "flap"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- flappy and so slow
  , ieffects = [ Explode "focused concussion"
               , OnSmash (Explode "violent concussion") ]
  , idesc    = "Avoid sudden movements."
  }
-- Not flashbang, because powerful bang without fragmentation is harder
-- to manufacture (requires an oxidizer and steel canister with holes).
-- The bang would also paralyze and/or lower the movement skill
-- (out of balance due to ear trauma).
flashBomb = fragmentationBomb
  { iname    = "powder tube"  -- filled with magnesium flash powder
  , iflavour = zipPlain [BrWhite]
  , iverbHit = "flash"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- bad shape for throwing
  , ieffects = [Explode "focused flash", OnSmash (Explode "violent flash")]
  , idesc    = "For dramatic entrances and urgent exits."
  }
firecrackerBomb = fragmentationBomb
  { iname = "roll"  -- not fireworks, as they require outdoors
  , iflavour = zipPlain [BrMagenta]
  , irarity  = [(1, 5), (5, 6)]  -- a toy, if deadly
  , iverbHit = "crack"  -- a pun, matches the verb from "ItemKindBlast"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [SetFlag Lobable, SetFlag Fragile]
  , ieffects = [Explode "firecracker", OnSmash (Explode "firecracker")]
  , idesc    = "String and paper, concealing a deadly surprise."
  }

-- ** Non-exploding consumables, not specifically designed for throwing

-- Foods require only minimal apply skill to consume. Many animals can eat them.

ediblePlantTemplate = ItemKind
  { isymbol  = symbolFood
  , iname    = "edible plant"
  , ifreq    = [("edible plant unknown", 1)]
  , iflavour = zipPlain stdCol
  , icount   = 1 `dL` 5
  , irarity  = [(1, 12), (10, 6)]  -- let's feed the animals
  , iverbHit = "thump"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [ HideAs "edible plant unknown"
               , toVelocity 30 ]  -- low density, often falling apart
  , ieffects = []
  , idesc    = "Withered but fragrant bits of a colorful plant. Taste tolerably and break down easily, but only eating may reveal the full effects."
  , ikit     = []
  }
ediblePlant1 = ediblePlantTemplate
  { iname    = "enhanced berry"
  , ifreq    = [("common item", 100), ("edible plant", 100)]
  , ieffects = [RefillHP 1, toOrganBad "immobile" (10 + 1 `d` 5)]
  }
ediblePlant2 = ediblePlantTemplate
  { iname    = "frayed fungus"
  , ifreq    = [("common item", 100), ("edible plant", 100)]
  , ieffects = [toOrganNoTimer "poisoned"]
  }
ediblePlant3 = ediblePlantTemplate
  { iname    = "thick leaf"
  , ifreq    = [("common item", 100), ("edible plant", 100)]
  , ieffects = [DropItem 1 maxBound COrgan "poisoned"]
  }
ediblePlant4 = ediblePlantTemplate
  { iname    = "reconfigured fruit"
  , ifreq    = [("common item", 100), ("edible plant", 100)]
  , ieffects = [toOrganBad "blind" (40 + 1 `d` 10)]
  }
ediblePlant5 = ediblePlantTemplate
  { iname    = "fragrant herb"
  , ifreq    = [("common item", 100), ("edible plant", 100)]
  , icount   = 1 `dL` 9
  , irarity  = [(1, 12), (10, 3)]
  , iaspects = ELabel "of lethargy"
               : iaspects ediblePlantTemplate
  , ieffects = [ toOrganBad "slowed" (20 + 1 `d` 5)
               , toOrganNoTimer "regenerating"
               , toOrganNoTimer "regenerating"  -- x2
               , RefillCalm 5 ]
  }
ediblePlant6 = ediblePlantTemplate
  { iname    = "dull flower"
  , ifreq    = [("common item", 100), ("edible plant", 100)]
  , ieffects = [PutToSleep]
  }
ediblePlant7 = ediblePlantTemplate
  { iname    = "spicy bark"
  , ifreq    = [("common item", 100), ("edible plant", 100)]
  , ieffects = [InsertMove 20, toOrganBad "frenzied" (40 + 1 `d` 10)]
  }

-- These require high apply skill to consume.

scrollTemplate = ItemKind
  { isymbol  = symbolScroll
  , iname    = "chip"
  , ifreq    = [("scroll unknown", 1)]
  , iflavour = zipFancy stdCol ++ zipPlain stdCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 14), (10, 7)]
  , iverbHit = "thump"
  , iweight  = 20
  , idamage  = 0
  , iaspects = [ HideAs "scroll unknown"
               , toVelocity 30 ]  -- too small
  , ieffects = []
  , idesc    = "A generic, disposable chip, capable of a one-time holo-display. Some of these also contain a one-time password authorizing a particular spaceship's infrastructure transition. It is unknown how the infrastructure might respond after so many years."
  , ikit     = []
  }
scroll1 = scrollTemplate
  { ifreq    = [("curious item", 100), ("any scroll", 100)]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so found early for a unique
  , iaspects = [SetFlag Unique, ELabel "of Reckless Beacon"]
               ++ iaspects scrollTemplate
  , ieffects = [Summon "hero" 1, Summon "mobile animal" (2 + 1 `d` 2)]
  , idesc    = "This industrial, wide-spectrum alarm broadcaster, if over-amped for a single powerful blast, should be able to cut through the interference and reach any lost crew members, giving them enough positional information to locate us."
  }
scroll2 = scrollTemplate
  { ifreq    = [("curious item", 100), ("any scroll", 100)]
  , irarity  = [(1, 6), (10, 2)]
  , ieffects = [Ascend True]
  }
scroll3 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 14)]
  , ieffects = [OneOf [ Teleport 5, Paralyze 10, InsertMove 30
                      , Detect DetectEmbed 12, Detect DetectHidden 20 ]]
  }
scroll4 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 11)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend False, Ascend True
                       , Summon "hero" 1, Summon "mobile animal" $ 1 `d` 2
                       , Detect DetectLoot 20  -- the most useful of detections
                       , CreateItem CGround "common item" timerNone ] ]
  }
scroll5 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 1  -- too poweful en masse
  , irarity  = [(10, 6)]
  , ieffects = [InsertMove $ 20 + 1 `dL` 20]
  }
scroll6 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 20)]  -- uncommon deep down, where all is known
  , iaspects = ELabel "of scientific explanation"
               : iaspects scrollTemplate
  , ieffects = [Composite [Identify, RefillCalm 10]]
  , idesc    = "The most pressing existential concerns are met with a deeply satisfying scientific answer."
  }
scroll7 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(10, 20)]  -- at endgame a crucial item may be missing
  , iaspects = ELabel "of molecular reconfiguration"
               : iaspects scrollTemplate
  , ieffects = [Composite [PolyItem, Explode "firecracker"]]
  }
scroll8 = scrollTemplate
  { ifreq    = [("curious item", 100), ("any scroll", 100)]
  , icount   = 1
  , irarity  = [(10, 12)]
  , iaspects = [SetFlag Unique, ELabel "of Rescue Proclamation"]
               ++ iaspects scrollTemplate
  , ieffects = [Summon "hero" 1]
  , idesc    = "This lock chip opens a nearby closet containing one of our lost crew members."
  }
scroll9 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(1, 9)]  -- powerful, even if not ideal
  , ieffects = [Detect DetectAll 20]
  }
scroll10 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , iaspects = ELabel "of cue interpretation"
               : iaspects scrollTemplate
  , ieffects = [Detect DetectActor 20]
  }
scroll11 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(10, 11)]
  , ieffects = [PushActor (ThrowMod 400 200 1)]  -- 8 steps, 4 turns
  }
scroll12 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(10, 20)]
  , iaspects = ELabel "of molecular duplication"
               : iaspects scrollTemplate
  , ieffects = [DupItem]
  }
scroll13 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , irarity  = [(10, 20)]
  , iaspects = ELabel "of surface reconfiguration"
               : iaspects scrollTemplate
  , ieffects = [RerollItem]
  }

-- ** Assorted tools

jumpingPole = ItemKind
  { isymbol  = symbolWand
  , iname    = "jumping pole"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "prod"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [ Timeout $ (2 + 1 `d` 2 - 1 `dL` 2) * 5
               , SetFlag Durable ]
  , ieffects = [toOrganGood "hasted" 1]
                 -- safe for AI, because it speeds up, so when AI applies it
                 -- again and again, it gets its time back and is not stuck;
                 -- in total, the explorations speed is unchanged,
                 -- but it's useful when fleeing in the dark to make distance
                 -- and when initiating combat, so it's OK that AI uses it
  , idesc    = "Makes you vulnerable at take-off, but then you are free like a bird."
  , ikit     = []
  }
sharpeningTool = ItemKind
  { isymbol  = symbolTool
  , iname    = "honing steel"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "smack"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee $ (1 `dL` 5) * 5
               , SetFlag Equipable, EqpSlot EqpSlotHurtMelee ]
  , ieffects = []
  , idesc    = "Originally used for realigning the chipped or buckled edges of kitchen knives in the local bars. Now it saves lives by letting you fix your weapons between or even during fights, without the need to set up camp, fish out tools and assemble a proper sharpening workshop."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = symbolFood
  , iname    = "visual sensor"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "gaze at"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ Timeout 3
               , AddSkill SkSight 10, AddSkill SkMaxCalm 30, AddSkill SkShine 2
               , SetFlag Periodic ]
  , ieffects = [ toOrganNoTimer "poisoned"
               , Summon "mobile robot" 1 ]
  , idesc    = "A functioning visual sensor torn out from some sizable robot. The circuitry seem too large for basic signal processing alone. Watch out for the sharp edges and the seeping coolant liquid."
  , ikit     = []
  }
motionScanner = ItemKind
  { isymbol  = symbolTool
  , iname    = "handhelp sonar"
  , ifreq    = [("common item", 100), ("add nocto 1", 20)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "ping"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkNocto 1
               , AddSkill SkArmorMelee (-10 + 1 `dL` 5)
               , AddSkill SkArmorRanged (-10 + 1 `dL` 5)
               , SetFlag Equipable, EqpSlot EqpSlotMiscBonus ]
  , ieffects = []
  , idesc    = "Portable underwater echolocator overdriven to scan dark corridors at the cost of emitting loud pings."
  , ikit     = []
  }

-- ** Periodic jewelry

-- Morally these are the aspects, but we also need to add a fake @Timeout@,
-- to let clients know that the not identified item is periodic jewelry.
iaspects_necklaceTemplate :: [Aspect]
iaspects_necklaceTemplate =
  [ HideAs "necklace unknown"
  , SetFlag Periodic, SetFlag Precious, SetFlag Equipable
  , toVelocity 50 ]  -- not dense enough
gorget = necklaceTemplate
  { iname    = "Old Gorget"
  , ifreq    = [("common item", 25), ("treasure", 25), ("museum", 100)]
  , iflavour = zipFancy [BrCyan]  -- looks exactly the same as one of necklaces,
                                  -- but it's OK, it's an artifact
  , iaspects = [ SetFlag Unique
               , Timeout $ (1 `d` 2) * 2
               , AddSkill SkArmorMelee 3, AddSkill SkArmorRanged 2
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [RefillCalm 1]
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  }
-- Not idenfified, because id by use, e.g., via periodic activations. Fun.
necklaceTemplate = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [("necklace unknown", 1)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(4, 3), (10, 4)]
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = 0
  , iaspects = Timeout 1  -- fake, but won't be displayed
               : iaspects_necklaceTemplate
  , ieffects = []
  , idesc    = "Tingling, rattling chain of flat encrusted links. Eccentric millionaires are known to hide their highly personalized body augmentation packs in such large jewelry pieces."
  , ikit     = []
  }
necklace1 = necklaceTemplate
  { ifreq    = [("curious item", 100), ("any jewelry", 100)]
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 1), (10, 3)]
                 -- prevents camping on lvl 3
  , iaspects = [ SetFlag Unique, ELabel "of Trickle Life"
               , Timeout $ (1 `d` 2) * 20
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [RefillHP 1]
  -- , idesc    = ""
  }
necklace2 = necklaceTemplate
  { ifreq    = [("treasure", 100), ("any jewelry", 100)]
      -- too nasty to call it just a "common item"
  , irarity  = [(10, 3)]
  , iaspects = [ SetFlag Unique, ELabel "of Live Bait"
               , Timeout 30
               , AddSkill SkOdor 2
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ DropItem 1 3 COrgan "condition"  -- mildly useful for applying
               , Impress
               , Summon "mobile animal" $ 1 `d` 2
               , Explode "waste" ]
  , idesc    = "A cord hung with lumps of decaying meat. It's better not to think about the source."
  }
necklace3 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [ ELabel "of fearful listening"
               , Timeout ((1 + 1 `d` 3) * 10)
               , AddSkill SkHearing 2 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectActor 10  -- can be applied; destroys the item
               , RefillCalm (-20) ]
  }
necklace4 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = Timeout ((3 + 1 `d` 3 - 1 `dL` 3) * 2)
               : iaspects_necklaceTemplate
  , ieffects = [Teleport $ 3 `d` 2]
  }
necklace5 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [ ELabel "of escape"
               , Timeout $ (7 - 1 `dL` 5) * 10 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Teleport $ 14 + 3 `d` 3  -- can be applied; destroys the item
               , Detect DetectExit 20
               , Yell ]  -- drawback when used for quick exploring
  , idesc    = "A supple chain that slips through your fingers."
  }
necklace6 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = Timeout (1 + (1 `d` 3) * 2)
               : iaspects_necklaceTemplate
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
                  -- the @50@ is only for the case of very light actor, etc.
  }
necklace7 = necklaceTemplate
  { ifreq    = [("curious item", 100), ("any jewelry", 100)]
  , irarity  = [(10, 1)]  -- powerful and determines tactics for one actor
  , iaspects = [ SetFlag Unique, ELabel "of Overdrive"
               , Timeout 10
               , AddSkill SkMaxHP 10  -- good effects vanish when taken off
               , AddSkill SkSpeed 10
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ RefillCalm (-2)  -- don't spam
               , toOrganBad "pacified" 10]
                 -- The same duration as timeout, to avoid spurious messages
                 -- as well as unlimited accumulation of the duration.
                 -- Timeout lessens temptation to frequently wear and take off,
                 -- to engage in melee, which would lead to micromanagement.
  -- , idesc    = ""
  }
necklace8 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(4, 3)]  -- entirely optional
  , iaspects = Timeout ((1 + 1 `d` 3) * 5)
               : iaspects_necklaceTemplate
  , ieffects = [Explode "spark"]
  }
necklace9 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = Timeout ((1 + 1 `d` 3) * 5)
               : iaspects_necklaceTemplate
  , ieffects = [Explode "fragrance"]
  }
necklace10 = necklaceTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , iaspects = [ ELabel "of greed"
               , Timeout ((3 + 1 `d` 2) * 10) ]
               ++ iaspects_necklaceTemplate
  , ieffects = [Detect DetectLoot 20, Teleport 20]
  }

-- ** Non-periodic jewelry

imageItensifier = ItemKind
  { isymbol  = symbolRing
  , iname    = "noctovisor"
  , ifreq    = [("treasure", 100), ("add nocto 1", 80), ("museum", 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "rattle"
  , iweight  = 700
  , idamage  = 0
  , iaspects = [ AddSkill SkNocto 1, AddSkill SkSight (-1)
               , AddSkill SkArmorMelee $ (1 `dL` 3) * 3
               , SetFlag Precious, SetFlag Equipable
               , EqpSlot EqpSlotMiscBonus ]
  , ieffects = []
  , idesc    = "Sturdy antique night vision goggles of unknown origin. Wired to run on modern micro-cells."
  , ikit     = []
  }
sightSharpening = ringTemplate  -- small and round, so mistaken for a ring
  { iname    = "Autozoom Contact Lens"
  , ifreq    = [("treasure", 20), ("add sight", 1)]
      -- it's has to be very rare, because it's powerful and not unique,
      -- and also because it looks exactly as one of necklaces, so it would
      -- be misleading when seen on the map
  , irarity  = [(7, 1), (10, 12)]  -- low @ifreq@
  , iweight  = 50  -- heavier that it looks, due to glass
  , iaspects = [ AddSkill SkSight $ 1 + 1 `d` 2
               , AddSkill SkHurtMelee $ (1 `d` 2) * 3
               , EqpSlot EqpSlotSight ]
               ++ iaspects ringTemplate
  , idesc    = "Zooms on any movement, distant or close. Requires some getting used to. Never needs to be taken off."
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and inv or even activating a wrong item by mistake.
--
-- By general mechanisms, due to not having effects that could identify
-- them by observing the effect, rings are identified on pickup.
-- That's unlike necklaces, which provide the fun of id-by-use, because they
-- have effects and when the effects are triggered, they get identified.
ringTemplate = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [("ring unknown", 1)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 1)]  -- the default very low
  , iverbHit = "knock"
  , iweight  = 15
  , idamage  = 0
  , iaspects = [HideAs "ring unknown", SetFlag Precious, SetFlag Equipable]
  , ieffects = []
  , idesc    = "A sturdy ring with a softly shining eye. If it contains a body booster unit, beware of the side-effects."
  , ikit     = []
  }
ring1 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(10, 4)]
  , iaspects = [ AddSkill SkSpeed $ 1 `d` 3, AddSkill SkMaxHP (-15)
               , EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  }
ring2 = ringTemplate
  { ifreq    = [("curious item", 100), ("any jewelry", 100)]
  , irarity  = [(10, 2)]
  , iaspects = [ SetFlag Unique, ELabel "of Rush"
               , AddSkill SkSpeed $ (1 `d` 2) * 3
               , AddSkill SkMaxCalm (-40), AddSkill SkMaxHP (-20)
               , SetFlag Durable, EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  -- , idesc    = ""
  }
ring3 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(10, 11)]
  , iaspects = [ AddSkill SkMaxHP $ 10 + (1 `dL` 5) * 2
               , AddSkill SkMaxCalm $ -20 + (1 `dL` 5) * 2
               , EqpSlot EqpSlotMaxHP ]
               ++ iaspects ringTemplate
  }
ring4 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100), ("museum", 100)]
  , irarity  = [(5, 1), (10, 12)]  -- needed after other rings drop Calm
  , iaspects = [ AddSkill SkMaxCalm $ 25 + (1 `dL` 4) * 5
               , EqpSlot EqpSlotMiscBonus ]
               ++ iaspects ringTemplate
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring5 = ringTemplate
  { ifreq    = [("common item", 100), ("any jewelry", 100)]
  , irarity  = [(3, 3), (10, 6)]
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 2 + (1 `dL` 2) * 2 ) * 3
               , AddSkill SkMaxHP $ (-2 - (1 `d` 2) + (1 `dL` 2) * 2) * 3
                   -- !!!
               , EqpSlot EqpSlotHurtMelee ]
               ++ iaspects ringTemplate
  }
ring6 = ringTemplate  -- weak skill per eqp slot, so can be without drawbacks
  { ifreq    = [("common item", 100), ("any jewelry", 100), ("museum", 100)]
  , irarity  = [(10, 4)]
  , iaspects = [ AddSkill SkShine 1
               , EqpSlot EqpSlotShine ]
               ++ iaspects ringTemplate
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring7 = ringTemplate
  { ifreq    = [("common item", 10), ("ring of opportunity sniper", 1)]
  , irarity  = [(10, 5)]  -- low @ifreq@
  , iaspects = [ ELabel "of opportunity sniper"
               , AddSkill SkProject 8
               , EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
  }
ring8 = ringTemplate
  { ifreq    = [("treasure", 100), ("any jewelry", 100)]
  , irarity  = [(10, 2)]
  , iaspects = [ SetFlag Unique, ELabel "of Overwatch"
               , AddSkill SkProject 8  -- TODO: 11, but let player control
                                       -- potion throwing; see capReinforced
               , AddSkill SkMaxHP (-20)
               , SetFlag Durable, EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
  -- , idesc    = ""  -- perhaps the constant trickle of a drug weakens bodily
                      -- resilience and recovery
  }

-- ** Armor

armorLeather = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "spacesuit breastplate"
  , ifreq    = [("common item", 100), ("torso armor", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 7000
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee (-2)
               , AddSkill SkArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (1 + 1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
  , ieffects = []
  , idesc    = "A hard-shell torso segment cut from a disposed off spacesuit. Well ventilated."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "bulletproof vest"
  , ifreq    = [("common item", 100), ("torso armor", 1), ("armor ranged", 50)]
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee (-3)
               , AddSkill SkArmorMelee $ (2 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (4 + 1 `dL` 2) * 3
               , AddSkill SkOdor 2
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorRanged ]
  , ieffects = []
  , idesc    = "A civilian bulletproof vest. Discourages foes from attacking your torso, making it harder for them to land a blow. Really hard to wash due to thickness."
  }
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "construction glove"
  , ifreq    = [("common item", 100), ("misc armor", 1), ("armor ranged", 50)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorRanged $ (1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotHurtMelee
               , toVelocity 50 ]  -- flaps and flutters
  , ieffects = []
  , idesc    = "A flexible construction glove from rough leather ensuring a good grip. Also, quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "spacesuit glove"
  , ifreq    = [("common item", 100), ("misc armor", 1)]
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 9), (10, 3)]
  , iweight  = 300
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , idesc    = "A piece of a hull maintenance spacesuit, padded and reinforced with carbon fibre."
  }
gloveJousting = gloveFencing
  { iname    = "Welding Handgear"
  , ifreq    = [("common item", 100), ("misc armor", 1)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iweight  = 3000
  , idamage  = 3 `d` 1
  , iaspects = [ SetFlag Unique
               , AddSkill SkHurtMelee $ (-7 + 1 `dL` 5) * 3
               , AddSkill SkArmorMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 5
               , AddSkill SkArmorRanged $ (1 + 1 `dL` 2) * 3
                 -- very random on purpose and can even be good on occasion
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , idesc    = "Rigid, bulky handgear embedding a welding equipment, complete with an affixed small shield and a darkened visor. Awe-inspiring."
  }
hatUshanka = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "ushanka hat"
  , ifreq    = [("common item", 100), ("misc armor", 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 6), (10, 2)]
  , iverbHit = "tickle"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ Timeout $ (2 + 1 `d` 2) * 3
               , AddSkill SkArmorMelee 5, AddSkill SkHearing (-10)
               , SetFlag Periodic, SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , ieffects = [RefillCalm 1]
  , idesc    = "Soft and warm fur. It keeps your ears warm."
  , ikit     = []
  }
capReinforced = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "construction cap"
  , ifreq    = [("common item", 100), ("misc armor", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(6, 9), (10, 3)]
  , iverbHit = "cut"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee $ (1 `dL` 2) * 5
               , AddSkill SkProject 1
                   -- the brim shields against blinding by light sources, etc.
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotProject ]
  , ieffects = []
  , idesc    = "A hard plastic shell that might soften a blow."
  , ikit     = []
  }
helmArmored = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "spacesuit helmet"
  , ifreq    = [("common item", 100), ("misc armor", 1)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(6, 9), (10, 3)]
  , iverbHit = "bounce"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (2 + 1 `dL` 2) * 3  -- headshot
               , AddSkill SkHearing (-7), AddSkill SkSight (-1)
               , AddSkill SkSmell (-5)
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorRanged ]
  , ieffects = []
  , idesc    = "Blocks out everything, including your senses."
  , ikit     = []
  }

-- ** Shields

-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
-- Note that AI will pick them up but never wear and will use them at most
-- as a way to push itself (but they won't recharge, not being in eqp).
-- Being @Meleeable@ they will not be use as weapons either.
-- This is OK, using shields smartly is totally beyond AI.
buckler = ItemKind
  { isymbol  = symbolShield
  , iname    = "buckler"
  , ifreq    = [("common item", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 5)]
  , iverbHit = "bash"
  , iweight  = 2000
  , idamage  = 0  -- safe to be used on self
  , iaspects = [ Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 2
               , AddSkill SkArmorMelee 40
                   -- not enough to compensate; won't be in eqp
               , AddSkill SkHurtMelee (-30)
                   -- too harmful; won't be wielded as weapon
               , SetFlag MinorEffects, SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- unwieldy to throw
  , ieffects = [PushActor (ThrowMod 200 50 1)]  -- 1 step, fast
  , idesc    = "Heavy and unwieldy arm protection made from an outer airlock panel. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(8, 4)]  -- the stronger variants add to total probability
  , iflavour = zipPlain [Green]
  , iweight  = 4000
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout $ (3 + 1 `d` 3 - 1 `dL` 3) * 4
               , AddSkill SkArmorMelee 80
                   -- not enough to compensate; won't be in eqp
               , AddSkill SkHurtMelee (-70)
                   -- too harmful; won't be wielded as weapon
               , SetFlag MinorEffects, SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- unwieldy to throw
  , ieffects = [PushActor (ThrowMod 400 50 1)]  -- 2 steps, fast
  , idesc    = "Large and unwieldy rectangle made of anti-meteorite ceramic sheet. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }
shield2 = shield
  { ifreq    = [("common item", 3 * 2), ("museum", 100)]
                  -- very low base rarity
  , iweight  = 5000
  , idamage  = 8 `d` 1
  , idesc    = "A relic of long-past wars, heavy and with a central spike."
  }
shield3 = shield2
  { ifreq    = [("common item", 1 * 2), ("museum", 10)]
                  -- very low base rarity
  , iweight  = 6000
  , idamage  = 12 `d` 1
  }

-- ** Weapons

dagger = ItemKind
  { isymbol  = symbolEdged
  , iname    = "cleaver"
  , ifreq    = [("common item", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(3 * 10/15, 30), (4 * 10/15, 1)]
                 -- no weapons brought by aliens, initially, so cleaver common
  , iverbHit = "stab"
  , iweight  = 1000
  , idamage  = 6 `d` 1
  , iaspects = [ Timeout 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorMelee $ (1 `d` 2) * 5
                   -- very common, so don't make too random
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeapon
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "A heavy professional kitchen blade. Will do fine cutting any kind of meat and bone, as well as parrying blows. Does not penetrate deeply, but is quick to move and hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { iname    = "Double Dagger"
  , ifreq    = [("treasure", 20), ("museum", 100)]
  , irarity  = [(1, 3), (10, 3)]
  , iaspects = [SetFlag Unique]
               ++ iaspects dagger
  , ieffects = [DropBestWeapon, Yell]  -- powerful and low timeout, but makes
                                       -- noise and useless against stupid foes
  , idesc    = "An antique double dagger that a focused fencer can use to catch and twist away an opponent's blade occasionally."
  }
iaspects_hammerTemplate :: [Aspect]
iaspects_hammerTemplate =
  [ HideAs "hammer unknown"
  , SetFlag Durable, SetFlag Meleeable
  , EqpSlot EqpSlotWeapon
  , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
hammer = ItemKind
  { isymbol  = symbolHafted
  , iname    = "demolition hammer"
  , ifreq    = [ ("common item", 100), ("starting weapon", 100)
               , ("hammer unknown", 1) ]
  , iflavour = zipFancy [BrMagenta]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(3 * 10/15, 1), (5, 15), (8, 1)]
                 -- don't make it too common on lvl 3
  , iverbHit = "club"
  , iweight  = 1600
  , idamage  = 8 `d` 1  -- we are lying about the dice here, but the dungeon
                        -- is too small and the extra-dice hammers too rare
                        -- to subdivide this identification class by dice
  , iaspects = [Timeout 5]
               ++ iaspects_hammerTemplate
  , ieffects = []
  , idesc    = "One of many kinds of hammers employed in construction work. The ones with completely blunt heads don't cause grave wounds, but any fitted with a long enough handle can shake and bruise even most armored foes, even though they require more time to recover after a swing. This one looks average at a quick glance."  -- if it's really the average kind, the weak kind, the description stays; if not, it's replaced with one of the descriptions below at identification time
  , ikit     = []
  }
hammer2 = hammer
  { ifreq    = [("common item", 10), ("starting weapon", 1)]
  , iweight  = 1000
  , iaspects = [Timeout 3]
               ++ iaspects_hammerTemplate
  , idesc    = "Upon closer inspection, this hammer turns out particularly handy and well balanced, with a narrowing, sharpened head compensating the modest heft."
  }
hammer3 = hammer
  { ifreq    = [("common item", 3)]
  , iweight  = 2400
  , idamage  = 12 `d` 1
  , iaspects = [Timeout 7]
               ++ iaspects_hammerTemplate
  , idesc    = "This hammer sports a long metal handle that increases durability and momentum of the sharpened head's swing, at the cost of longer recovery."
  }
hammerParalyze = hammer
  { iname    = "Concussion Hammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = [ SetFlag Unique
               , Timeout 7 ]
               ++ iaspects_hammerTemplate
  , ieffects = [Paralyze 10]
  , idesc    = "This exceptionally large demolition hammer leaves no wall and no body standing."
  }
hammerSpark = hammer
  { iname    = "Grand Smithhammer"
  , ifreq    = [("treasure", 20), ("museum", 100)]
  , irarity  = [(5, 1), (8, 6)]
  , iweight  = 2400
  , idamage  = 12 `d` 1
  , iaspects = [ SetFlag Unique
               , Timeout 10
               , AddSkill SkShine 3]
               ++ iaspects_hammerTemplate
  , ieffects = [Explode "spark"]
      -- we can't use a focused explosion, because it would harm the hammer
      -- wielder as well, unlike this one
  , idesc    = "High carbon steel of this old hammer doesn't yield even to the newest alloys and produces fountains of sparks in defiance."
  }
sword = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "sharpened pipe"
  , ifreq    = [("common item", 100), ("starting weapon", 10)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(3, 1), (5, 15)]
  , iverbHit = "slash"
  , iweight  = 2000
  , idamage  = 10 `d` 1
  , iaspects = [ Timeout 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeapon
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "A makeshift weapon of simple design, but great potential. Hard to master, though."
  , ikit     = []
  }
swordImpress = sword
  { isymbol  = symbolEdged
  , iname    = "Master's Sword"
  , ifreq    = [("treasure", 20), ("museum", 100)]
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = [SetFlag Unique]
               ++ iaspects sword
  , ieffects = [Impress]
  , idesc    = "A particularly well-balance museum piece. It has a long history and in the right hands lends itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { isymbol  = symbolEdged
  , iname    = "Roasting rapier"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = [SetFlag Unique]
               ++ iaspects sword
  , ieffects = [ DropItem 1 maxBound COrgan "condition"
               , RefillCalm (-10)
               , Yell ]
  , idesc    = "A thin, acutely sharp steel blade that pierces deeply and sends its victim into abrupt, sobering shock. Originally, an exuberant hand-forged roasting implement, intentionally kept blunt."
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "pole cleaver"
  , ifreq    = [("common item", 100), ("starting weapon", 20)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 0), (8, 15)]
  , iverbHit = "impale"
  , iweight  = 3000
  , idamage  = 12 `d` 1
  , iaspects = [ Timeout 12
               , AddSkill SkHurtMelee (-20)
                   -- useless against armor at game start
               , AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeapon
               , toVelocity 20 ]  -- not balanced
  , ieffects = []
  , idesc    = "An improvised but deadly weapon made of a long, sharp kitchen knife glued and bound to a long pole. Not often one succeeds in making enough space to swing it freely, but even when stuck between terrain obstacles it blocks approaches effectively."
  , ikit     = []
  }
halberd2 = halberd
  { iname    = "Pollaxe"
  , ifreq    = [("common item", 3 * 3), ("starting weapon", 1)]
  , iweight  = 4000
  , idamage  = 18 `d` 1
  , idesc    = "A long-hafted axe: once used for maintenance, now turned to a bloodier purpose."
  }
halberdPushActor = halberd
  { iname    = "Swiss Halberd"
  , ifreq    = [("curious item", 20)]  -- not museum; reenactors
  , irarity  = [(7, 0), (9, 15)]
  , iaspects = [SetFlag Unique]
               ++ iaspects halberd
  , ieffects = [PushActor (ThrowMod 200 100 1)]  -- 2 steps, slow
  , idesc    = "A perfect replica made for a reenactor troupe, hardened, missing only some final sharpening. Versatile, with great reach and leverage. Foes are held at a distance."
  }

-- ** Wands

wandTemplate = ItemKind
  { isymbol  = symbolWand
  , iname    = "injector"
  , ifreq    = [("wand unknown", 1)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , irarity  = []
  , iverbHit = "club"
  , iweight  = 300
  , idamage  = 0
  , iaspects = [ HideAs "wand unknown"
               , AddSkill SkShine 1, AddSkill SkSpeed (-1)
                   -- pulsing with power, distracts
               , SetFlag Durable
               , toVelocity 125 ]  -- sufficiently advanced tech
  , ieffects = []
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."
  , ikit     = []
  }
wand1 = wandTemplate
  { ifreq    = []
  , ieffects = []  -- will be: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '|' and '{'
  }

-- ** Treasure

gemTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gem"
  , ifreq    = [("gem unknown", 1), ("valuable", 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = [(3, 0), (10, 24)]
  , iverbHit = "tap"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [HideAs "gem unknown", SetFlag Precious]
  , ieffects = []
  , idesc    = "Precious, though useless. Worth around 100 gold grains."
  , ikit     = []
  }
gem1 = gemTemplate
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(3, 0), (6, 12), (10, 8)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
                 -- reflects strongly, distracts; so it glows in the dark,
                 -- is visible on dark floor, but not too tempting to wear
               ++ iaspects gemTemplate
  , ieffects = [RefillCalm (-1)]  -- minor effect to ensure no id-on-pickup
  }
gem2 = gem1
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(5, 0), (7, 25), (10, 8)]
  }
gem3 = gem1
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(7, 0), (8, 20), (10, 8)]
  }
gem4 = gem1
  { ifreq    = [ ("treasure", 100), ("gem", 100), ("any jewelry", 100)
               , ("valuable", 100) ]
  , irarity  = [(9, 0), (10, 70)]
  }
gem5 = gem1
  { isymbol  = symbolSpecial
  , iname    = "stimpack"
  , ifreq    = [ ("treasure", 100), ("gem", 25), ("any jewelry", 10)
               , ("valuable", 100) ]
  , iflavour = zipPlain [BrYellow]
  , irarity  = [(1, 40), (10, 10)]
  , iaspects = [ ELabel "of youth", SetFlag Precious  -- not hidden
               , AddSkill SkOdor (-1) ]
  , ieffects = [RefillCalm 10, RefillHP 40]
  , idesc    = "Calms, heals, invigorates, rejuvenates and smells nice. No side-effects. As valuable as precious gems, at 100 gold grains each."
  }
currencyTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold grain"
  , ifreq    = [("currency unknown", 1), ("valuable", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dL` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [HideAs "currency unknown", SetFlag Precious]
  , ieffects = []
  , idesc    = "Reliably valuable in every civilized place."
  , ikit     = []
  }
currency = currencyTemplate
  { ifreq    = [("treasure", 100), ("currency", 100), ("valuable", 1)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
               ++ iaspects currencyTemplate
  , ieffects = [RefillCalm (-1)]
  }

-- * Allure-specific items

needle = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "needle"
  , ifreq    = [("needle", 1), ("common item", 1)]
      -- marked as common to ensure can be polymorphed;
      -- TODO: fast when fired, not thrown
  , iflavour = zipPlain [BrBlue]
  , icount   = 1 + 8 `d` 3
  , irarity  = [(1, 1)]
  , iverbHit = "prick"
  , iweight  = 3
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ -10 * 5
               , SetFlag Fragile
               , ToThrow $ ThrowMod 70 100 3 ]
  , ieffects = []
  , idesc    = "A long hypodermic needle ending in a dried out micro-syringe. It's too light to throw hard, but it penetrates deeply, causing intense pain on movement."
  , ikit     = []
  }
constructionHooter = necklaceTemplate
  { iname    = "construction hooter"
  , ifreq    = [("common item", 1), ("construction hooter", 1)]
                  -- extremely rare, but dropped by decontamination chambers
  , iflavour = zipPlain [BrRed]
  , irarity  = [(1, 1)]
  , iweight  = 1000
  , iaspects = [ AddSkill SkArmorMelee 2
               , SetFlag Durable, toVelocity 50
               , SetFlag Equipable, EqpSlot EqpSlotArmorMelee]
  , ieffects = [Yell, Summon "construction robot" 1]
  , idesc    = "An emergency hooter for alarming human personel in case their life is in danger. Worn by construction robots around their \"neck\", where it's least exposed, but nevertheless it needs to be heavily armored and running on its own power suppply."
  }
wasteContainer = ItemKind
  { isymbol  = symbolTool
  , iname    = "waste container"
  , ifreq    = [("waste container", 1)]
  , iflavour = zipLiquid [Green]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "spill over"
  , iweight  = 30000
  , idamage  = 0
  , iaspects = [ Timeout $ (1 + 1 `d` 2) * 30
               , SetFlag Periodic, SetFlag Equipable ]
  , ieffects = [ Detect DetectLoot 20
               , Summon "mobile animal" $ 1 `dL` 2
               , Explode "waste" ]
  , idesc    = "Waste recognition and utilization subsystem. Detects any stray item not registered as passenger cargo. Leaks a little."
  , ikit     = []
  }
spotlight = ItemKind
  { isymbol  = symbolTool
  , iname    = "spotlight"
  , ifreq    = [("spotlight", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "illuminate"
  , iweight  = 3000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine 4
               , AddSkill SkHurtMelee $ - 1 `d` 3  -- heavy and unwieldy
               , SetFlag Equipable, EqpSlot EqpSlotShine ]
  , ieffects = [Detect DetectHidden 20]
  , idesc    = "Powerful wide-beam spotlight in an unwieldy rack-mounted package. On overdrive, it can shine through thin construction surfaces, underlying fault lines."
  , ikit     = []
  }
scrollAd1 = scrollTemplate
  { ifreq    = [("common item", 100), ("any scroll", 100)]
  , icount   = 1
  , irarity  = [(1, 1)]  -- not every playthrough needs one
  , iaspects = [ELabel "of turist guide"]
               ++ iaspects scrollTemplate
  , ieffects = [ toOrganGood "resolute" (500 + 1 `d` 200)
                   -- a drawback (at least initially) due to @calmEnough@
               , Explode "cruise ad hologram"
               , Detect DetectLoot 5 ]  -- short so useless most of the time
  , idesc    = "Biodegradable self-powered mini-projector displaying holographic ads and shopping hints."
  }
blowtorch = ItemKind
  { isymbol  = symbolLight
  , iname    = "blowtorch"  -- not unique, but almost never generated on floor
  , ifreq    = [("blowtorch", 1), ("valuable", 20), ("curious item", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "scorch"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ Timeout 7
               , AddSkill SkAlter 2
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotAlter ]
  , ieffects = [Burn 2, Impress]
  , idesc    = "A sturdy old-fashioned portable blowtorch for fine cutting or welding of metals. Rather weak, but does not require access codes to high current power outlets."
  , ikit     = []
  }
