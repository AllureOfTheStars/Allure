-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Item definitions.
module Content.ItemKind
  ( -- * Group name patterns
    pattern HARPOON, pattern TORSO_ARMOR, pattern CLOTHING_MISC
  , pattern MUSEAL, pattern FIREPROOF_CLOTH, pattern THICK_CLOTH, pattern COOKED_PLANT, pattern LIQUID_NITROGEN, pattern BREACHING_TOOL, pattern WIRECUTTING_TOOL
  , groupNamesSingleton, groupNames
  , -- * Content
    content, items, otherItemContent
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindEmbed
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Content.RuleKind
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns
groupNamesSingleton :: [GroupName ItemKind]
groupNamesSingleton =
       [FLASK_UNKNOWN, ANY_POTION_UNKNOWN, EDIBLE_PLANT_UNKNOWN, SCROLL_UNKNOWN, NECKLACE_UNKNOWN, RING_UNKNOWN, HAMMER_UNKNOWN, GEM_UNKNOWN, CURRENCY_UNKNOWN]
    ++ [S_GRASS_STITCHER, S_LADIES_FORK, S_SPADE, S_HOE]
    ++ [COOKED_PLANT_UNKNOWN]
    ++ embedsGNSingleton ++ organsGNSingleton ++ blastsGNSingleton
    ++ temporariesGNSingleton

pattern FLASK_UNKNOWN, ANY_POTION_UNKNOWN, EDIBLE_PLANT_UNKNOWN, SCROLL_UNKNOWN, NECKLACE_UNKNOWN, RING_UNKNOWN, HAMMER_UNKNOWN, GEM_UNKNOWN, CURRENCY_UNKNOWN :: GroupName ItemKind

pattern S_GRASS_STITCHER, S_LADIES_FORK, S_SPADE, S_HOE :: GroupName ItemKind

pattern COOKED_PLANT_UNKNOWN :: GroupName ItemKind

groupNames :: [GroupName ItemKind]
groupNames =
       [HARPOON, TORSO_ARMOR, CLOTHING_MISC]
    ++ [MUSEAL, FIREPROOF_CLOTH, THICK_CLOTH, COOKED_PLANT, LIQUID_NITROGEN, BREACHING_TOOL, WIRECUTTING_TOOL]
    ++ embedsGN ++ actorsGN ++ organsGN ++ blastsGN

pattern HARPOON, TORSO_ARMOR, CLOTHING_MISC :: GroupName ItemKind

pattern MUSEAL, FIREPROOF_CLOTH, THICK_CLOTH, COOKED_PLANT, LIQUID_NITROGEN, BREACHING_TOOL, WIRECUTTING_TOOL:: GroupName ItemKind

-- The @UNKNOWN@ patterns don't need to be exported. Used internally.
-- They also represent singleton groups.
pattern FLASK_UNKNOWN = GroupName "flask unknown"
pattern ANY_POTION_UNKNOWN = GroupName "potion unknown"
pattern EDIBLE_PLANT_UNKNOWN = GroupName "edible plant unknown"
pattern SCROLL_UNKNOWN = GroupName "scroll unknown"
pattern NECKLACE_UNKNOWN = GroupName "necklace unknown"
pattern RING_UNKNOWN = GroupName "ring unknown"
pattern HAMMER_UNKNOWN = GroupName "hammer unknown"
pattern GEM_UNKNOWN = GroupName "gem unknown"
pattern CURRENCY_UNKNOWN = GroupName "currency unknown"

pattern HARPOON = GroupName "harpoon"
pattern TORSO_ARMOR = GroupName "torso armor"
pattern CLOTHING_MISC = GroupName "clothing misc"

-- ** Allure-specific

-- The @UNKNOWN@ patterns don't need to be exported. Used internally.
-- They also represent singleton groups.
pattern COOKED_PLANT_UNKNOWN = GroupName "cooked plant unknown"

pattern MUSEAL = GroupName "museal"
pattern FIREPROOF_CLOTH = GroupName "fireproof cloth"
pattern THICK_CLOTH = GroupName "thick cloth"
pattern COOKED_PLANT = GroupName "cooked plant"
pattern LIQUID_NITROGEN = GroupName "liquid nitrogen"
pattern BREACHING_TOOL = GroupName "breaching tool"
pattern WIRECUTTING_TOOL = GroupName "wirecutting tool"

pattern S_GRASS_STITCHER = GroupName "grass stitcher"
pattern S_LADIES_FORK = GroupName "ladies' fork"
pattern S_SPADE = GroupName "spade"
pattern S_HOE = GroupName "hoe"

-- * Content

content :: [ItemKind]
content = items ++ otherItemContent

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
  [sandstoneRock, dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, ediblePlant8, cookedPlantTemplate, cookedPlant1, cookedPlant2, cookedPlant3, cookedPlant4, cookedPlant5, cookedPlant6, cookedPlant7, cookedPlant8, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, scroll14, scroll15, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorLeather2, armorMail, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, buckler, shield, shield2, shield3, dagger, daggerDropBestWeapon, hammerTemplate, hammer1, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberdPushActor, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency]
  -- Allure-specific
  ++ [needle, needleSleep, constructionHooter, wasteContainer, spotlight, scrollAd1, blowtorch, rawMeatChunk, roastedMeatChunk, militaryKnife, militaryBaton, cattleProd, chisel, steelFile, hacksaw, adjustableSpanner, crowbar, catsPaw, diagonalPliers, snips, loppers, boltCutter, grassStitcher, ladiesFork, spade, hoe]

sandstoneRock,    dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, flask15, flask16, flask17, flask18, flask19, flask20, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, ediblePlant8, cookedPlantTemplate, cookedPlant1, cookedPlant2, cookedPlant3, cookedPlant4, cookedPlant5, cookedPlant6, cookedPlant7, cookedPlant8, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, scroll14, scroll15, jumpingPole, sharpeningTool, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorLeather2, armorMail, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, buckler, shield, shield2, shield3, dagger, daggerDropBestWeapon, hammerTemplate, hammer1, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberdPushActor, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency :: ItemKind
-- Allure-specific
needle, needleSleep, constructionHooter, wasteContainer, spotlight, scrollAd1, blowtorch, rawMeatChunk, roastedMeatChunk, militaryKnife, militaryBaton, cattleProd, chisel, steelFile, hacksaw, adjustableSpanner, crowbar, catsPaw, diagonalPliers, snips, loppers, boltCutter, grassStitcher, ladiesFork, spade, hoe :: ItemKind

-- Keep the dice rolls and sides in aspects small so that not too many
-- distinct items are generated (for display in item lore and for narrative
-- impact ("oh, I found the more powerful of the two variants of the item!",
-- instead of "hmm, I found one of the countless variants, a decent one").
-- In particular, for unique items, unless they inherit aspects from
-- a standard item, permit only a couple possible variants.
-- This is especially important if an item kind has multiple random aspects.
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
  , ifreq    = [ (SANDSTONE_ROCK, 1)
               , (UNREPORTED_INVENTORY, 1) ]  -- too weak to spam
  , iflavour = zipPlain [Green]
  , icount   = 1 + 1 `d` 2  -- > 1, to let AI ignore sole pieces
  , irarity  = [(1, 50), (10, 1)]
  , iverbHit = "swat"
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
  , ifreq    = [(COMMON_ITEM, 100), (ANY_ARROW, 50), (WEAK_ARROW, 50)]
  , iflavour = zipPlain [White]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 15), (10, 5)]
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
  , ifreq    = [(COMMON_ITEM, 100), (ANY_ARROW, 50), (WEAK_ARROW, 50)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1 + 4 `dL` 5
  , irarity  = [(1, 10), (10, 10)]
  , iverbHit = "nick"
  , iweight  = 100
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy vs armor
               , SetFlag MinorEffects
               , toVelocity 70 ]  -- hitting with tip costs speed
  , ieffects = [ Explode S_SINGLE_SPARK  -- when hitting enemy
               , OnSmash (Explode S_SINGLE_SPARK) ]  -- at wall hit
      -- this results in a wordy item synopsis, but it's OK, the spark really
      -- is useful in some situations, not just a flavour
  , idesc    = "Not particularly well balanced, but with a laser-sharpened titanium alloy tip and blade."
  , ikit     = []
  }
spike2 = spike
  { iname    = "heavy steak knife"
  , ifreq    = [(COMMON_ITEM, 2), (ANY_ARROW, 1), (WEAK_ARROW, 1)]
  , icount   = 6 `dL` 5
  , iverbHit = "penetrate"
  , iweight  = 150
  , idamage = 4 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
               , SetFlag MinorEffects
               , Odds (10 * 1 `dL` 10) [] [toVelocity 70] ]
                   -- at deep levels sometimes even don't limit velocity
  , idesc    = "Old, slightly discoloured, probably from a genuine steel. A heavy and surprisingly well balanced prop from a posh restaurant."  -- the theme of pre-modern things being more solid and intimidating
  }
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "steel hex nut"
  , ifreq    = [(COMMON_ITEM, 5), (ANY_ARROW, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1 + 3 `dL` 4
  , irarity  = [(1, 1), (10, 20)]
  , iverbHit = "clobber"
  , iweight  = 200
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- heavy, to bludgeon through armor
               , SetFlag MinorEffects
               , toVelocity 150 ]
  , ieffects = [ Explode S_SINGLE_SPARK  -- when hitting enemy
               , OnSmash (Explode S_SINGLE_SPARK) ]  -- at wall hit
  , idesc    = "A large hexagonal fastening nut; due to its angular shape, securely lodging in the pouch of a makeshift string and cloth sling."
  , ikit     = []
  }
slingBullet = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "bearing ball"
  , ifreq    = [(COMMON_ITEM, 5), (ANY_ARROW, 100), (MERCENARY_AMMO, 25)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1 + 6 `dL` 4
  , irarity  = [(1, 1), (10, 15)]
  , iverbHit = "slug"
  , iweight  = 28
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-17 + 1 `d` 2 + 1 `dL` 3) * 5
                   -- not too good against armor
               , ToThrow $ ThrowMod 200 100 2 ]  -- piercing
  , ieffects = []
  , idesc    = "Small but heavy bearing ball. Thanks to its size and shape, it doesn't snag when released from the makeshift sling's pouch. Minimal friction enable it to pierce through flesh when fast enough initially."  -- we lie, it doesn't slow down in our model; but it stops piercing alright
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
  , ifreq    = [ (COMMON_ITEM, 100), (CAN_OF_STICKY_FOAM, 1)
               , (MERCENARY_AMMO, 25) ]
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
  , ieffects = [Paralyze 15, OnSmash (Explode S_GLUE) ]
  , idesc    = "A can of liquid, fast-setting construction foam."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [(CURIOUS_ITEM, 100), (HARPOON, 100), (MUSEAL, 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1  -- durable, so one piece lasts long
  , irarity  = [(7, 5), (10, 5)]
  , iverbHit = "hook"
  , iweight  = 750
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 5
               , AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
               , SetFlag Durable, SetFlag Meleeable ]
                 -- no EqpSlot EqpSlotWeaponBig, because often worse than
                 -- an organ, so a waste of equipment space
  , ieffects = [ PullActor (ThrowMod 200 50 1)  -- 1 step, fast
               , Yell ]  -- cry out in pain, because brutal
  , idesc    = "A display piece harking back to the Earth's oceanic tourism heyday. The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
harpoon2 = harpoon
  { iname    = "heavy harpoon"
  , ifreq    = [(CURIOUS_ITEM, 5), (HARPOON, 2)]
  , iweight  = 1000
  , idamage  = 10 `d` 1
  , iaspects = [EqpSlot EqpSlotWeaponBig]  -- more powerful than most organs
               ++ iaspects harpoon
  , idesc    = "A sharpened cargo-hook with high-tension cord."  -- could use some explanation of why it breaks when thrown, especially that's it's not apparent
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [(COMMON_ITEM, 100), (MERCENARY_AMMO, 25)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1 `dL` 3
  , irarity  = [(5, 5), (10, 7)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , idamage  = 2 `d` 1
  , iaspects = [AddSkill SkHurtMelee $ -14 * 5]
  , ieffects = [ toOrganBad S_SLOWED (3 + 1 `d` 3)
               , DropItem maxBound 1 CEqp TORSO_ARMOR
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
  , ifreq    = [ (COMMON_ITEM, 100), (LIGHT_MANIPULATION, 100)
               , (FIRE_SOURCE, 1), (WOODEN_TORCH, 1) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1 `dL` 4
  , irarity  = [(1, 20), (4, 1)]
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
  , ifreq    = [(COMMON_ITEM, 100), (LIGHT_MANIPULATION, 100)]
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
               , toOrganBad S_PACIFIED (2 + 1 `d` 2)
               , OnSmash (Explode S_BURNING_OIL_2) ]
  , idesc    = "A restaurant table glass lamp filled with plant oil feeding a slender wick."
  , ikit     = []
  }
light3 = ItemKind
  { isymbol  = symbolLight
  , iname    = "brass lantern"
  , ifreq    = [(COMMON_ITEM, 100), (MUSEAL, 100), (LIGHT_MANIPULATION, 20)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(6, 1), (10, 4)]
  , iverbHit = "burn"
  , iweight  = 3000
  , idamage  = 2 `d` 1
  , iaspects = [ AddSkill SkShine 4, AddSkill SkSight (-1)
               , SetFlag Lobable, SetFlag Fragile, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
  , ieffects = [ Burn 1
               , toOrganBad S_PACIFIED (4 + 1 `d` 2)
               , OnSmash (Explode S_BURNING_OIL_4) ]
  , idesc    = "Very old, very bright and very heavy lantern made of hand-polished brass."
  , ikit     = []
  }
blanket = ItemKind
  { isymbol  = symbolLight
  , iname    = "mineral fibre blanket"
  , ifreq    = [ (COMMON_ITEM, 100), (LIGHT_MANIPULATION, 100), (BLANKET, 1)
               , (THICK_CLOTH, 1), (FIREPROOF_CLOTH, 1)
               , (FIRE_FIGHTING_ITEM, 30) ]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]  -- scavenged from walls
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
-- Whether to hit with them or explode them close to the target
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
  , ifreq    = [(FLASK_UNKNOWN, 1)]
  , iflavour = zipGlassPlain darkCol ++ zipGlassFancy darkCol
               ++ zipLiquid darkCol ++ zipFancy darkCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 7), (10, 3)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ PresentAs FLASK_UNKNOWN, SetFlag Lobable, SetFlag Fragile
               , toVelocity 60 ]  -- oily, rather bad grip
  , ieffects = []
  , idesc    = "A flask of oily liquid of a suspect color. Something seems to be moving inside. Double dose causes twice longer effect."
  , ikit     = []
  }
flask1 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , icount   = 1 `dL` 5
  , irarity  = [(10, 10)]
  , iaspects = ELabel "of strength renewal brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_STRENGTHENED (20 + 1 `d` 5)
               , toOrganNoTimer S_REGENERATING
               , OnSmash (Explode S_DENSE_SHOWER) ]
  }
flask2 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of weakness brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganBad S_WEAKENED (20 + 1 `d` 5)
               , OnSmash (Explode S_SPARSE_SHOWER) ]
  }
flask3 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)
               , (OIL_SOURCE, 1) ]
  , iaspects = ELabel "of melee protective balm"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_PROTECTED_FROM_MELEE (20 + 1 `d` 5)
               , OnSmash (Explode S_MELEE_PROTECTIVE_BALM) ]
  , idesc    = "A flask of wrestling balm that adheres to the body, but turns into slick oil when hit. Double dose causes twice longer effect."
  }
flask4 = flaskTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)
               , (OIL_SOURCE, 1) ]
  , iaspects = ELabel "of ranged protective balm"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_PROTECTED_FROM_RANGED (20 + 1 `d` 5)
               , OnSmash (Explode S_RANGE_PROTECTIVE_BALM) ]
  , idesc    = "A flask of durable body and fabric ointment. Its nanostructure hardens under stress. Double dose causes twice longer effect."
  }
flask5 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of fluorescent paint"
               : iaspects flaskTemplate
  , ieffects = [ toOrganBad S_PAINTED (20 + 1 `d` 5)
               , OnSmash (Explode S_PAINT_DROPLET) ]
  }
flask6 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , irarity  = [(1, 1)]  -- not every playthrough needs one
  , iaspects = ELabel "of resolution"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_RESOLUTE (500 + 1 `d` 200)  -- long, for scouting
               , RefillCalm 60  -- not to make it a drawback, via @calmEnough@
               , OnSmash (Explode S_RESOLUTION_DUST) ]
  }
flask7 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , icount   = 1  -- too powerful en masse
  , iaspects = ELabel "of haste brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_HASTED (20 + 1 `d` 5)
               , OnSmash (Explode S_HASTE_SPRAY) ]
  }
flask8 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of eye drops"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_FAR_SIGHTED (40 + 1 `d` 10)
               , OnSmash (Explode S_EYE_DROP) ]
  }
flask9 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 2)]  -- not very useful right now
  , iaspects = ELabel "of smelly concoction"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_KEEN_SMELLING (40 + 1 `d` 10)
               , Detect DetectActor 10  -- make it at least slightly useful
               , OnSmash (Explode S_SMELLY_DROPLET) ]
  }
flask10 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 2)]  -- not very useful right now
  , iaspects = ELabel "of cat tears"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_SHINY_EYED (40 + 1 `d` 10)
               , OnSmash (Explode S_EYE_SHINE) ]
  }
flask11 = flaskTemplate
  { iname    = "bottle"
  , ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , icount   = 1 `d` 3  -- the only one sometimes giving away its identity
  , iaspects = ELabel "of whiskey"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_DRUNK (20 + 1 `d` 5)
               , Burn 1, RefillHP 3, Yell
               , OnSmash (Explode S_WHISKEY_SPRAY) ]
  }
flask12 = flaskTemplate
  { iname    = "flagon"
  , ifreq    = [ (COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)
               , (WATER_SOURCE, 1) ]
  , icount   = 1
  , iaspects = ELabel "of bait cocktail"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_DRUNK (20 + 1 `d` 5)
               , Burn 1, RefillHP 3  -- risky exploit possible, good
               , Summon MOBILE_ANIMAL 1
               , OnSmash (Summon MOBILE_ANIMAL 1)
               , OnSmash Impress  -- mildly useful when thrown
               , OnSmash (Explode S_WASTE) ]
  }
-- The player has full control over throwing the flask at his party,
-- so he can milk the explosion, so it has to be much weaker, so a weak
-- healing effect is enough. OTOH, throwing a harmful flask at many enemies
-- at once is not easy to arrange, so these explosions can stay powerful.
flask13 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , irarity  = [(1, 2), (10, 12)]
  , iaspects = ELabel "of regeneration brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_ROSE_SMELLING (80 + 1 `d` 20)
               , toOrganNoTimer S_REGENERATING
               , toOrganNoTimer S_REGENERATING  -- x2
               , OnSmash (Explode S_YOUTH_SPRINKLE) ]
  }
flask14 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of poison"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_POISONED, toOrganNoTimer S_POISONED  -- x2
               , OnSmash (Explode S_POISON_CLOUD) ]
  }
flask15 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 4)]
  , iaspects = ELabel "of slow resistance"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_POISON_RESISTANT
               , OnSmash (Explode S_ANTI_SLOW_MIST) ]
  }
flask16 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 4)]
  , iaspects = ELabel "of poison resistance"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_SLOW_RESISTANT
               , OnSmash (Explode S_ANTIDOTE_MIST) ]
  }
flask17 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of calamity"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_POISONED
               , toOrganBad S_WEAKENED (20 + 1 `d` 5)
               , toOrganBad S_DEFENSELESS (20 + 1 `d` 5)
               , OnSmash (Explode S_GLASS_HAIL) ]  -- enough glass to cause that
  }

-- Potions are often natural, including natural stats.
-- They appear deeper than most flasks. Various configurations of effects.
-- A different class of effects is on scrolls and mechanical items.
-- Some are shared.

potionTemplate = ItemKind
  { isymbol  = symbolPotion
  , iname    = "vial"
  , ifreq    = [(ANY_POTION_UNKNOWN, 1)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 10), (10, 6)]
  , iverbHit = "splash"
  , iweight  = 200
  , idamage  = 0
  , iaspects = [ PresentAs ANY_POTION_UNKNOWN, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- oily, small momentum due to small size
  , ieffects = []
  , idesc    = "A vial of bright, frothing concoction. The best that nature has to offer."
  , ikit     = []
  }
potion1 = potionTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (PERFUME_POTION, 1)
               , (ANY_POTION, 100), (ANY_GLASS, 100) ]
  , iaspects = ELabel "of perfume"  -- very useful, despite appearances
               : iaspects potionTemplate
  , ieffects = [ Impress, toOrganGood S_ROSE_SMELLING (50 + 1 `d` 10)
               , OnSmash ApplyPerfume, OnSmash (Explode S_FRAGRANCE) ]
  }
potion2 = potionTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_GLASS, 100)]
  , icount   = 1
  , irarity  = [(5, 8), (10, 8)]
  , iaspects = [ SetFlag Unique, ELabel "of Attraction"
               , SetFlag Precious, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- identified
  , ieffects = [ Dominate
               , toOrganGood S_HASTED (20 + 1 `d` 5)
               , Discharge 0
               , OnSmash (Explode S_PHEROMONE)
               , OnSmash (Explode S_HASTE_SPRAY) ]
  , idesc    = "The liquid fizzes with energy."
  }
potion3 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , ieffects = [ RefillHP 5, DropItem 1 maxBound COrgan S_POISONED
               , OnSmash (Explode S_HEALING_MIST) ]
  }
potion4 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(1, 6), (10, 10)]
  , ieffects = [ RefillHP 10
               , DropItem maxBound maxBound COrgan CONDITION
               , DropItem maxBound maxBound COrgan S_HUNGRY
               , OnSmash (Explode S_HEALING_MIST_2) ]
  }
potion5 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 3 `dL` 1  -- always as many as possible on this level
                         -- without giving away potion identity
  , irarity  = [(1, 12)]
  , ieffects = [ OneOf [ RefillHP 10, RefillHP 5, Burn 5
                       , DropItem 1 maxBound COrgan S_POISONED
                       , toOrganGood S_STRENGTHENED (20 + 1 `d` 5) ]
               , OnSmash (OneOf [ Explode S_DENSE_SHOWER
                                , Explode S_SPARSE_SHOWER
                                , Explode S_MELEE_PROTECTIVE_BALM
                                , Explode S_RANGE_PROTECTIVE_BALM
                                , Explode S_DEFENSELESSNESS_RUNOUT ]) ]
  }
potion6 = potionTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 10)]
  , ieffects = [ Impress
               , OneOf [ RefillHP 20, RefillHP 10, Burn 10
                       , DropItem 1 maxBound COrgan S_POISONED
                       , toOrganGood S_HASTED (20 + 1 `d` 5)
                       , toOrganBad S_IMPATIENT (2 + 1 `d` 2) ]
               , OnSmash (OneOf [ Explode S_HEALING_MIST_2
                                , Explode S_WOUNDING_MIST
                                , Explode S_DISTRESSING_ODOR
                                , Explode $ blastNoStatOf S_IMPATIENT
                                , Explode S_HASTE_SPRAY
                                , Explode S_SLOWNESS_MIST
                                , Explode S_FRAGRANCE
                                , Explode S_VIOLENT_FLASH ]) ]
  }
potion7 = potionTemplate
  { iname    = "ampoule"  -- filled with semi-stabilized high explosive liquid
  , ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 3 `dL` 1
  , ieffects = [ DropItem 1 maxBound COrgan CONDITION
               , OnSmash (Explode S_VIOLENT_CONCUSSION) ]
      -- not fragmentation nor glass hail, because not enough glass
  }
potion8 = potionTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_GLASS, 100)]
  , icount   = 1
  , irarity  = [(10, 5)]
  , iaspects = [ SetFlag Unique, ELabel "of Love"
               , SetFlag Precious, SetFlag Lobable, SetFlag Fragile
               , toVelocity 50 ]  -- identified
  , ieffects = [ RefillHP 60, RefillCalm (-60)
               , toOrganGood S_ROSE_SMELLING (80 + 1 `d` 20)
               , OnSmash (Explode S_HEALING_MIST_2)
               , OnSmash (Explode S_DISTRESSING_ODOR) ]
  , idesc    = "Perplexing swirls of intense, compelling colour."
  }
potion9 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 5)]
  , iaspects = ELabel "of grenadier focus"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood S_MORE_PROJECTING (40 + 1 `d` 10)
               , toOrganBad S_PACIFIED (5 + 1 `d` 3)
                   -- the malus has to be weak, or would be too good
                   -- when thrown at foes
               , OnSmash (Explode $ blastBonusStatOf S_MORE_PROJECTING)
               , OnSmash (Explode $ blastNoStatOf S_PACIFIED) ]
  , idesc    = "Thick, sluggish fluid with violently-bursting bubbles."
  }
potion10 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of frenzy"
               : iaspects potionTemplate
  , ieffects = [ Yell
               , toOrganGood S_STRENGTHENED (20 + 1 `d` 5)
               , toOrganBad S_RETAINING (5 + 1 `d` 3)
               , toOrganBad S_FRENZIED (40 + 1 `d` 10)
               , OnSmash (Explode S_DENSE_SHOWER)
               , OnSmash (Explode $ blastNoStatOf S_RETAINING)
               , OnSmash (Explode $ blastNoStatOf S_RETAINING) ]
  }
potion11 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of panic"
               : iaspects potionTemplate
  , ieffects = [ RefillCalm (-30)
               , toOrganGood S_HASTED (20 + 1 `d` 5)
               , toOrganBad S_WEAKENED (20 + 1 `d` 5)
               , toOrganBad S_WITHHOLDING (10 + 1 `d` 5)
               , OnSmash (Explode S_HASTE_SPRAY)
               , OnSmash (Explode S_SPARSE_SHOWER)
               , OnSmash (Explode $ blastNoStatOf S_WITHHOLDING) ]
  }
potion12 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of quicksilver"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood S_HASTED (20 + 1 `d` 5)
               , toOrganBad S_BLIND (10 + 1 `d` 5)
               , toOrganBad S_IMMOBILE (5 + 1 `d` 5)
               , OnSmash (Explode S_HASTE_SPRAY)
               , OnSmash (Explode S_IRON_FILING)
               , OnSmash (Explode $ blastNoStatOf S_IMMOBILE) ]
  }

-- ** Explosives, with the only effect being @Explode@

fragmentationBomb = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "hand bomb"
      -- improvised bomb filled with iron pellets, nuts, cut nails;
      -- deflagration, not detonation, so large mass and hard container
      -- required not to burn harmlessly; improvised short fuze;
      -- can't be more powerful or would fracture the spaceship's hull
  , ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 200)]
  , iflavour = zipPlain [Red]
  , icount   = 1 `dL` 5  -- many, because not very intricate
  , irarity  = [(5, 8), (10, 5)]
  , iverbHit = "thud"
  , iweight  = 3000  -- low velocity due to weight
  , idamage  = 1 `d` 1  -- heavy and hard
  , iaspects = [SetFlag Lobable, SetFlag Fragile]
  , ieffects = [ Explode S_FOCUSED_FRAGMENTATION
               , OnSmash (Explode S_VIOLENT_FRAGMENTATION) ]
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
  , iverbHit = "bonk"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- flappy and so slow
  , ieffects = [ Explode S_FOCUSED_CONCUSSION
               , OnSmash (Explode S_VIOLENT_CONCUSSION) ]
  , idesc    = "Avoid sudden movements."
  }
-- Not flashbang, because powerful bang without fragmentation is harder
-- to manufacture (requires an oxidizer and steel canister with holes).
-- The bang would also paralyze and/or lower the movement skill
-- (out of balance due to ear trauma).
flashBomb = fragmentationBomb
  { iname    = "powder tube"  -- filled with magnesium flash powder
  , iflavour = zipPlain [BrYellow]  -- avoid @BrWhite@; looks wrong in dark
  , iverbHit = "flash"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [ SetFlag Lobable, SetFlag Fragile
               , toVelocity 70 ]  -- bad shape for throwing
  , ieffects = [Explode S_FOCUSED_fLASH, OnSmash (Explode S_VIOLENT_FLASH)]
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
  , ieffects = [Explode S_FIRECRACKER, OnSmash (Explode S_FIRECRACKER)]
  , idesc    = "String and paper, concealing a deadly surprise."
  }

-- ** Non-exploding consumables, not specifically designed for throwing

-- Foods require only minimal apply skill to consume. Many animals can eat them.

ediblePlantTemplate = ItemKind
  { isymbol  = symbolFood
  , iname    = "edible plant"
  , ifreq    = [(EDIBLE_PLANT_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol
  , icount   = 1 `dL` 5
  , irarity  = [(1, 5), (10, 2)]  -- weak, apart of hunger removal
  , iverbHit = "thump"
  , iweight  = 300
  , idamage  = 0
  , iaspects = [ PresentAs EDIBLE_PLANT_UNKNOWN
               , toVelocity 30 ]  -- low density, often falling apart
  , ieffects = []
  , idesc    = "Withered but fragrant bits of a colorful plant. Taste tolerably. Doesn't break down that easily in its raw form, without cooking. Only eating may reveal the full effects."
  , ikit     = []
  }
ediblePlant1 = ediblePlantTemplate
  { iname    = "enhanced berry"
  , ifreq    = [(S_ENCHANCED_BERRY, 1), (COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [ RefillHP 1, toOrganBad S_IMMOBILE (5 + 1 `d` 5)
               , DropItem maxBound 1 COrgan S_HUNGRY ]
  }
ediblePlant2 = ediblePlantTemplate
  { iname    = "frayed fungus"
  , ifreq    = [(S_FRAYED_FUNGUS, 1), (COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [toOrganNoTimer S_POISONED]
  }
ediblePlant3 = ediblePlantTemplate
  { iname    = "thick leaf"
  , ifreq    = [(S_THIC_LEAF, 1), (COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [ DropItem 1 maxBound COrgan S_POISONED
               , DropItem maxBound 2 COrgan S_HUNGRY ]
  }
ediblePlant4 = ediblePlantTemplate
  { iname    = "reconfigured fruit"
  , ifreq    = [ (S_RECONFIGURED_FRUIT, 1), (COMMON_ITEM, 100)
               , (EDIBLE_PLANT, 100) ]
  , ieffects = [ toOrganBad S_BLIND (10 + 1 `d` 10)
               , DropItem maxBound 3 COrgan S_HUNGRY ]
  }
ediblePlant5 = ediblePlantTemplate
  { iname    = "fragrant herb"
  , ifreq    = [(S_FRAGRANT_HERB, 1), (COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , icount   = 1 `dL` 9
  , irarity  = [(1, 4), (10, 1)]  -- powerful; many copies
  , iaspects = ELabel "of lethargy"
               : iaspects ediblePlantTemplate
  , ieffects = [ toOrganBad S_SLOWED (20 + 1 `d` 5)
               , toOrganNoTimer S_REGENERATING
               , toOrganNoTimer S_REGENERATING  -- x2
               , RefillCalm 5 ]  -- too many effects to also add hunger removal
  }
ediblePlant6 = ediblePlantTemplate
  { iname    = "dull flower"
  , ifreq    = [(S_DULL_FLOWER, 1), (COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [ PutToSleep
               , DropItem maxBound 1 COrgan S_HUNGRY ]
  }
ediblePlant7 = ediblePlantTemplate
  { iname    = "spicy bark"
  , ifreq    = [(S_SPICY_BARK, 1), (COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , ieffects = [ InsertMove 20, toOrganBad S_FRENZIED (40 + 1 `d` 10)
               , DropItem maxBound 1 COrgan S_HUNGRY ]
  }
ediblePlant8 = ediblePlantTemplate
  { iname    = "pumpkin"
  , ifreq    = [(S_PUMPKIN, 1), (COMMON_ITEM, 100), (EDIBLE_PLANT, 100)]
  , irarity  = [(1, 2), (10, 5)]  -- solves the hunger problem, but not too soon
  , iweight  = 3000
  , idamage  = 1 `d` 1
  , ieffects = [DropItem maxBound 1 COrgan S_HUNGRY]
  }

cookedPlantTemplate = ediblePlantTemplate
  { iname    = "cooked plant"
  , ifreq    = [(COOKED_PLANT_UNKNOWN, 1)]
  , iflavour = zipPlain stdCol
  , irarity  = [(1, 1)]
  , iaspects = [ PresentAs COOKED_PLANT_UNKNOWN
               , toVelocity 20 ]  -- low density, often falling apart
  , idesc    = "Withered but fragrant bits of a colorful plant. Taste blandly, but break down easily, releasing all nutrients. Only eating may reveal the full effects."
  }
cookedPlant1 = cookedPlantTemplate
  { iname    = "cooked berry"
  , ifreq    = [(S_COOKED_BERRY, 1), (COMMON_ITEM, 1), (COOKED_PLANT, 100)]
  , ieffects = [ RefillHP 1, toOrganBad S_IMMOBILE (5 + 1 `d` 5)
               , DropItem maxBound 2 COrgan S_HUNGRY ]
  }
cookedPlant2 = cookedPlantTemplate
  { iname    = "cooked fungus"
  , ifreq    = [(S_COOKED_FUNGUS, 1), (COMMON_ITEM, 1), (COOKED_PLANT, 100)]
  , ieffects = ieffects ediblePlant2
  }
cookedPlant3 = cookedPlantTemplate
  { iname    = "cooked leaf"
  , ifreq    = [ (S_COOKED_LEAF, 1)
               , (COMMON_ITEM, 1), (COOKED_PLANT, 100), (COOKED_FOOD, 10) ]
  , ieffects = [ DropItem 1 maxBound COrgan S_POISONED
               , DropItem maxBound 3 COrgan S_HUNGRY ]
  }
cookedPlant4 = cookedPlantTemplate
  { iname    = "cooked fruit"
  , ifreq    = [ (S_COOKED_FRUIT, 1)
               , (COMMON_ITEM, 1), (COOKED_PLANT, 100) ]
  , ieffects = [ toOrganBad S_BLIND (10 + 1 `d` 10)
               , DropItem maxBound 4 COrgan S_HUNGRY ]
  }
cookedPlant5 = cookedPlantTemplate
  { iname    = "cooked herb"
  , ifreq    = [ (S_COOKED_HERB, 1)
               , (COMMON_ITEM, 1), (COOKED_PLANT, 100) ]
  , icount   = 1 `dL` 9
  , iaspects = ELabel "of lethargy"
               : iaspects cookedPlantTemplate
  , ieffects = ieffects ediblePlant5
  }
cookedPlant6 = cookedPlantTemplate
  { iname    = "cooked flower"
  , ifreq    = [ (S_COOKED_FLOWER, 1)
               , (COMMON_ITEM, 1), (COOKED_PLANT, 100), (COOKED_FOOD, 10) ]
  , ieffects = [ PutToSleep
               , DropItem maxBound 2 COrgan S_HUNGRY ]
  }
cookedPlant7 = cookedPlantTemplate
  { iname    = "cooked bark"
  , ifreq    = [ (S_COOKED_BARK, 1)
               , (COMMON_ITEM, 1), (COOKED_PLANT, 100), (COOKED_FOOD, 10) ]
  , ieffects = [ InsertMove 20, toOrganBad S_FRENZIED (40 + 1 `d` 10)
               , DropItem maxBound 2 COrgan S_HUNGRY ]
  }
cookedPlant8 = cookedPlantTemplate
  { iname    = "cooked pumpkin"
  , ifreq    = [ (S_COOKED_PUMPKIN, 1)
               , (COMMON_ITEM, 1), (COOKED_PLANT, 100), (COOKED_FOOD, 10) ]
  , iweight  = 3000
  , idamage  = 1 `d` 1
  , ieffects = [DropItem maxBound 5 COrgan S_HUNGRY]
  }

-- These require high apply skill to consume.

scrollTemplate = ItemKind
  { isymbol  = symbolScroll
  , iname    = "chip"
  , ifreq    = [(SCROLL_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol ++ zipPlain stdCol
  , icount   = 1 `dL` 3
  , irarity  = [(1, 14), (10, 7)]
  , iverbHit = "thump"
  , iweight  = 20
  , idamage  = 0
  , iaspects = [ PresentAs SCROLL_UNKNOWN
               , toVelocity 30 ]  -- too small
  , ieffects = []
  , idesc    = "A generic, disposable chip, capable of a one-time holo-display. Some of these also contain a one-time password authorizing a particular spaceship's infrastructure transition. Nobody knows how the infrastructure might respond after so many years."
  , ikit     = []
  }
scroll1 = scrollTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]  -- mixed blessing, so found early for a unique
  , iaspects = [SetFlag Unique, ELabel "of Reckless Beacon"]
               ++ iaspects scrollTemplate
  , ieffects = [Summon HERO 1, Summon MOBILE_ANIMAL (2 + 1 `d` 2)]
  , idesc    = "This industrial, wide-spectrum alarm broadcaster, if over-amped for a single powerful blast, should be able to cut through the interference and reach any lost crew members, giving them enough positional information to locate us."
  }
scroll2 = scrollTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(1, 6), (10, 2)]
  , ieffects = [Ascend True]
  }
scroll3 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 14)]
  , ieffects = [OneOf [ Paralyze 10, InsertMove 30, Discharge 0
                      , Detect DetectEmbed 12, Detect DetectHidden 20 ]]
  }
scroll4 = scrollTemplate
  -- needs to be common to show at least a portion of effects
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(10, 14)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend False, Ascend True
                       , Summon HERO 1, Summon MOBILE_ANIMAL $ 1 `d` 2
                       , Detect DetectLoot 20  -- the most useful of detections
                       , CreateItem Nothing CGround COMMON_ITEM timerNone ] ]
  }
scroll5 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(1, 6)]  -- powerful, but low counts at the depths it appears on
  , ieffects = [InsertMove $ 20 + 1 `dL` 20]
  }
scroll6 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 3 `dL` 1
  , irarity  = [(1, 20)]  -- uncommon deep down, where all is known
  , iaspects = ELabel "of scientific explanation"
               : iaspects scrollTemplate
  , ieffects = [Identify `AndEffect` RefillCalm 10]
  , idesc    = "The most pressing existential concerns are met with a deeply satisfying scientific answer."
  }
scroll7 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 20)]  -- at endgame a crucial item may be missing
  , iaspects = ELabel "of molecular reconfiguration"
               : iaspects scrollTemplate
  , ieffects = [PolyItem `AndEffect` Explode S_FIRECRACKER]
  }
scroll8 = scrollTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 1
  , irarity  = [(10, 12)]
  , iaspects = [SetFlag Unique, ELabel "of Skeleton Key"]
               ++ iaspects scrollTemplate
  , ieffects = [Summon HERO 1]
  , idesc    = "This is a security lock chip that opens all doors in the area, including the hatch to a nearby closet, bulging from the blows of one of our lost crew members."
  }
scroll9 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 4)]  -- powerful, even if not ideal; scares newbies
  , ieffects = [Detect DetectAll 20]
  }
scroll10 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , iaspects = ELabel "of cue interpretation"
               : iaspects scrollTemplate
  , ieffects = [Detect DetectActor 20]
  }
scroll11 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 11)]
  , ieffects = [PushActor (ThrowMod 400 200 1)]  -- 8 steps, 4 turns
  }
scroll12 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 15)]
  , iaspects = ELabel "of molecular duplication"
               : iaspects scrollTemplate
  , ieffects = [DupItem]
  }
scroll13 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 15)]
  , iaspects = ELabel "of surface reconfiguration"
               : iaspects scrollTemplate
  , ieffects = [RerollItem]
  }
scroll14 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , ieffects = [Discharge $ 40 - 1 `d` 20]
  }
scroll15 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , irarity  = [(10, 15)]
  , ieffects = [Discharge 0]
  }

-- ** Assorted tools

jumpingPole = ItemKind
  { isymbol  = symbolWand
  , iname    = "jumping pole"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 3)]
  , iverbHit = "prod"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = [ Timeout $ (2 + 1 `d` 2 - 1 `dL` 2) * 5
               , SetFlag Durable ]
  , ieffects = [toOrganGood S_HASTED 1]
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
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "smack"
  , iweight  = 400
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee $ (1 `dL` 7) * 5
               , SetFlag Equipable
               , EqpSlot EqpSlotHurtMelee ]
  , ieffects = []
  , idesc    = "Originally used for realigning and sharpening dulled edges of kitchen knives in the local restaurants. Now it saves lives by keeping your melee weapons keen and true before each skirmish."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = symbolFood
  , iname    = "visual sensor"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 2)]
  , iverbHit = "gaze at"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ Timeout 3
               , AddSkill SkSight 10  -- a spyglass for quick wields
               , AddSkill SkMaxCalm 30  -- to diminish clipping sight by Calm
               , AddSkill SkShine 2  -- to lit corridors when flying
               , AddSkill SkMaxHP (-30)  -- prevent excessive stacking
               , SetFlag Periodic ]
  , ieffects = [ Detect DetectActor 20  -- rare enough
               , Explode S_SINGLE_SPARK
               , toOrganNoTimer S_POISONED  -- really can't be worn
               , Summon MOBILE_ROBOT 1 ]
  , idesc    = "An oversize visual sensor freshly torn out of some unfortunate robot. It still sends a clear picture to unidentified receivers, even though the coolant liquid seeps from the seized servos and many internal contacts spark loose."
  , ikit     = []
  }
motionScanner = ItemKind
  { isymbol  = symbolTool
  , iname    = "handheld sonar"
  , ifreq    = [(COMMON_ITEM, 100), (ADD_NOCTO_1, 20)]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "ping"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkNocto 1
               , AddSkill SkArmorMelee (-15 + (1 `dL` 3) * 5)
               , AddSkill SkArmorRanged (-15 + (1 `dL` 3) * 5)
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
  [ PresentAs NECKLACE_UNKNOWN
  , SetFlag Periodic, SetFlag Precious, SetFlag Equipable
  , toVelocity 50 ]  -- not dense enough
gorget = necklaceTemplate
  { iname    = "Old Gorget"
  , ifreq    = [(COMMON_ITEM, 25), (TREASURE, 25), (MUSEAL, 100)]
  , iflavour = zipFancy [BrCyan]  -- looks exactly the same as one of necklaces,
                                  -- but it's OK, it's an artifact
  , iaspects = [ SetFlag Unique
               , Timeout $ 5 - 1 `dL` 4
               , AddSkill SkArmorMelee 3, AddSkill SkArmorRanged 2
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [RefillCalm 1]
  , idesc    = "Highly ornamental, cold, large steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old worn engraving reassures the wearer."
  }
-- Not identified, because id by use, e.g., via periodic activations. Fun.
necklaceTemplate = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [(NECKLACE_UNKNOWN, 1)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(4, 3), (10, 4)]
  , iverbHit = "whip"
  , iweight  = 30
  , idamage  = 0
  , iaspects = Timeout 1000000
                 -- fake, needed to display "charging"; the timeout itself
                 -- won't be displayed thanks to periodic; as a side-effect,
                 -- it can't be activated until identified, which is better
                 -- than letting the player try to activate before the real
                 -- cooldown is over and waste turn
               : iaspects_necklaceTemplate
  , ieffects = []
  , idesc    = "Tingling, rattling chain of flat encrusted links. Eccentric millionaires are known to hide their highly personalized body augmentation packs in bulky jewelry pieces such as these."
  , ikit     = []
  }
necklace1 = necklaceTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(3 * 10/15, 0), (4 * 10/15, 1), (10, 3)]
                 -- prevents camping on lvl 3
  , iaspects = [ SetFlag Unique, ELabel "of Spur Life"
               , Timeout $ (4 - 1 `dL` 3) * 10
                   -- priceless, so worth the long wait
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [RefillHP 1, RefillCalm (-5)]
  , idesc    = "This awkward chain, when worn on bare skin, frequently emits mild but highly annoying electric shocks, which apparently stimulate tissue regeneration even in distant parts of the body. A part of the surprising effectiveness of this unique artifact may stem from the desperation of the patients to be quickly healed enough to take it off."
  }
-- no necklace2 of Live Bait, wasteContainer too similar
necklace3 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of fearful listening"
               , Timeout ((1 + 1 `d` 2) * 10)
               , AddSkill SkHearing 2 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectActor 10  -- can be applied; destroys the item
               , RefillCalm (-40) ]
  }
necklace4 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout ((3 + 1 `d` 3 - 1 `dL` 3) * 2)
               : iaspects_necklaceTemplate
  , ieffects = [Teleport $ 3 `d` 2]
  }
necklace5 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of escape"
               , Timeout $ (7 - 1 `dL` 5) * 10 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Teleport $ 14 + 3 `d` 3  -- can be applied; destroys the item
               , Detect DetectExit 20
               , Yell ]  -- drawback when used for quick exploring
  , idesc    = "A supple chain that slips through your fingers."
  }
necklace6 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout (1 + (1 `d` 3) * 2)
               : iaspects_necklaceTemplate
  , ieffects = [PushActor (ThrowMod 100 50 1)]  -- 1 step, slow
                  -- the @50@ is only for the case of very light actor, etc.
  }
necklace7 = necklaceTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 1)]  -- powerful and determines tactics for one actor
  , iaspects = [ SetFlag Unique, ELabel "of Overdrive"
               , Timeout 10
               , AddSkill SkMaxHP 10  -- good effects vanish when taken off
               , AddSkill SkSpeed 10
               , SetFlag Durable ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ RefillCalm (-2)  -- don't spam
               , Discharge 101 ]  -- higher by 1 to ensure others not activated
                 -- Lasting effect lessens temptation to frequently take off
                 -- when engaging in melee, which would lead to micromanagement.
                 -- Quite OOP if worn with the right set of other items, anyway.
  , idesc    = "This whirring augmentation pack stimulates its host beyond any medically advisable or, surely, even legally admissible levels. It can be only speculated what kind of activity it was designed for, but clearly the steady handling of melee weapons was not one of them."
  }
necklace8 = necklaceTemplate
  { iname    = "coil"
  , ifreq    = [ (COMMON_ITEM, 100), (REFRIGERATION_COIL, 1), (ANY_JEWELRY, 100)
               , (COLD_SOURCE, 1) ]
  , iaspects = ELabel "of superconducting refrigeration"
               : Timeout ((1 + 1 `d` 3) * 5)
               : delete (SetFlag Precious) iaspects_necklaceTemplate
  , ieffects = [Explode S_CURRENT_DISCHARGE]
  }
necklace9 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = Timeout ((1 + 1 `d` 3) * 5)
               : iaspects_necklaceTemplate
  , ieffects = [Explode S_FRAGRANCE]
  }
necklace10 = necklaceTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ ELabel "of greed"
               , Timeout ((2 + 1 `d` 3) * 10) ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectLoot 20
               , Teleport 40  -- risky
               , toOrganBad S_PARSIMONIOUS (5 + 1 `d` 3) ]  -- hard to flee
  }

-- ** Non-periodic jewelry

imageItensifier = ItemKind
  { isymbol  = symbolRing
  , iname    = "noctovisor"
  , ifreq    = [(TREASURE, 100), (ADD_NOCTO_1, 80), (MUSEAL, 100)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 1
  , irarity  = [(5, 2)]
  , iverbHit = "rattle"
  , iweight  = 700
  , idamage  = 0
  , iaspects = [ AddSkill SkNocto 1, AddSkill SkSight (-1)
               , AddSkill SkArmorMelee $ (-1 + 1 `dL` 6) * 3
               , SetFlag Precious, SetFlag Equipable
               , EqpSlot EqpSlotMiscBonus ]
  , ieffects = []
  , idesc    = "Sturdy antique night vision goggles of unknown origin. Wired to run on modern micro-cells."
  , ikit     = []
  }
sightSharpening = ringTemplate  -- small and round, so mistaken for a ring
  { iname    = "Autozoom Contact Lens"
  , ifreq    = [(TREASURE, 20), (ADD_SIGHT, 1)]
      -- it's has to be very rare, because it's powerful and not unique,
      -- and also because it looks exactly as one of necklaces, so it would
      -- be misleading when seen on the map
  , irarity  = [(7, 1), (10, 12)]  -- low @ifreq@
  , iweight  = 50  -- heavier that it looks, due to glass
  , iaspects = [ AddSkill SkSight $ 1 + 1 `dL` 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 3) * 3
               , EqpSlot EqpSlotSight ]
               ++ iaspects ringTemplate
  , idesc    = "Zooms on any movement, distant or close. Requires some getting used to. Never needs to be taken off."
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and stash or even activating a wrong item by mistake.
--
-- By general mechanisms, due to not having effects that could identify
-- them by observing the effect, rings are identified on pickup.
-- That's unlike necklaces, which provide the fun of id-by-use, because they
-- have effects and when the effects are triggered, they get identified.
ringTemplate = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [(RING_UNKNOWN, 1)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 2)]  -- the default very low
  , iverbHit = "knock"
  , iweight  = 15
  , idamage  = 0
  , iaspects = [PresentAs RING_UNKNOWN, SetFlag Precious, SetFlag Equipable]
  , ieffects = []
  , idesc    = "A sturdy ring with a softly shining eye. If it contains a body booster unit, beware of the side-effects."
  , ikit     = []
  }
ring1 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(8, 4)]
  , iaspects = [ AddSkill SkSpeed $ 1 `dL` 3, AddSkill SkMaxHP (-10)
               , EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  }
ring2 = ringTemplate
  { ifreq    = [(CURIOUS_ITEM, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ SetFlag Unique, ELabel "of Rush"
               , AddSkill SkSpeed $ (1 + 1 `dL` 2) * 2
               , AddSkill SkMaxCalm (-40), AddSkill SkMaxHP (-20)
               , SetFlag Durable, EqpSlot EqpSlotSpeed ]
               ++ iaspects ringTemplate
  , idesc    = "There creator of this dangerous artifact didn't find time to document its operation. And now it's too late."
  }
ring3 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(10, 8)]
  , iaspects = [ AddSkill SkMaxHP $ 5 + (1 `d` 2 + 1 `dL` 2) * 5
               , AddSkill SkMaxCalm $ -30 + (1 `dL` 3) * 5
               , EqpSlot EqpSlotMaxHP ]
               ++ iaspects ringTemplate
  }
ring4 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100), (MUSEAL, 100)]
  , irarity  = [(5, 1), (10, 9)]  -- needed after other rings drop Calm
  , iaspects = [ AddSkill SkMaxCalm $ 20 + (1 `dL` 4) * 5
               , EqpSlot EqpSlotMiscBonus ]
               ++ iaspects ringTemplate
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with solemn, strangely comforting, worn out words."
  }
ring5 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100)]
  , irarity  = [(3, 4), (10, 8)]
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 2 + (1 `dL` 2) * 2 ) * 3
               , AddSkill SkMaxHP $ (-3 + 1 `dL` 3) * 10
               , EqpSlot EqpSlotHurtMelee ]
               ++ iaspects ringTemplate
  }
ring6 = ringTemplate  -- weak skill per eqp slot, so can be without drawbacks
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100), (MUSEAL, 100)]
  , irarity  = [(10, 3)]
  , iaspects = [ AddSkill SkShine 1
               , EqpSlot EqpSlotShine ]
               ++ iaspects ringTemplate
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring7 = ringTemplate
  { ifreq    = [(COMMON_ITEM, 10), (RING_OF_OPPORTUNITY_SNIPER, 1)]
  , irarity  = [(10, 5)]  -- low @ifreq@
  , iaspects = [ ELabel "of opportunity sniper"
               , AddSkill SkProject 8
               , EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
  , idesc    = "This mil-grade communication equipment feeds the aggregated enemy position information to the wearer, even when he is not the pointman of the team and so the team is not intentionally spotting for him. With proper training this permits ranged attacks, even indirect fire, without neglecting the simultaneous squad doctrine obligation of covering the approach of the pointman."
  }
ring8 = ringTemplate
  { ifreq    = [(TREASURE, 100), (ANY_JEWELRY, 100)]
  , iaspects = [ SetFlag Unique, ELabel "of Overwatch"
               , AddSkill SkProject 8  -- TODO: 11, but let player control
                                       -- potion throwing; see capReinforced
               , AddSkill SkMaxHP (-20)
               , SetFlag Durable, EqpSlot EqpSlotProject ]
               ++ iaspects ringTemplate
  , idesc    = "This exceptional medical contraption constantly transforms and re-injects minuscule amounts of blood serum, synthesizing powerful drugs that greatly enhance spacial awareness and focus, at the cost of weakening bodily resilience and recovery. With this boost, indirect fire becomes possible, even for a non-pointman team member."
  }

-- ** Armor

armorLeather = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "spacesuit breastplate"
  , ifreq    = [(COMMON_ITEM, 100), (TORSO_ARMOR, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 4), (10, 2)]
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
armorLeather2 = armorLeather  -- for now, purely flavour, for better messages
  { isymbol  = symbolMiscArmor
  , iname    = "pair"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iaspects = ELabel "of spacesuit trousers" : iaspects armorLeather
  , idesc    = "Segmented trousers for open space work, with the hermetically sealed boots cut off. Surprisingly flexible and airy, yet micro-meteorite-proof."
  }
armorMail = armorLeather
  { iname    = "bulletproof vest"
  , ifreq    = [ (COMMON_ITEM, 100), (TORSO_ARMOR, 1), (ARMOR_RANGED, 50)
               , (BULLTEPROOF_VEST, 1) ]
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
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1), (ARMOR_RANGED, 50)]
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
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1)]
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
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iverbHit = "rasp"
  , iweight  = 3000
  , idamage  = 3 `d` 1
  , iaspects = [ SetFlag Unique
               , AddSkill SkHurtMelee $ (-7 + 1 `dL` 5) * 3
               , AddSkill SkArmorMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 5
               , AddSkill SkArmorRanged $ (1 + 1 `dL` 2) * 3
                 -- very random on purpose and can even be good on occasion
                 -- or when ItemRerolled enough times
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 50 ]  -- flaps and flutters
  , idesc    = "Rigid, bulky handgear embedding a welding equipment, complete with an affixed small shield and a darkened visor. Awe-inspiring."
  }
hatUshanka = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "ushanka hat"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_MISC, 1), (CLOTHING_MISC, 1)
               , (THICK_CLOTH, 1) ]
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
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(6, 9), (10, 3)]
  , iverbHit = "cut"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkArmorMelee $ (1 `d` 2) * 5
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
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1)]
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
  , ifreq    = [(COMMON_ITEM, 100)]
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
  { ifreq    = [(COMMON_ITEM, 3 * 2), (MUSEAL, 100)]
                  -- very low base rarity
  , iweight  = 5000
  , idamage  = 8 `d` 1
  , idesc    = "A relic of long-past wars, heavy and with a central spike."
  }
shield3 = shield2
  { ifreq    = [(COMMON_ITEM, 1 * 2), (MUSEAL, 10)]
                  -- very low base rarity
  , iweight  = 6000
  , idamage  = 12 `d` 1
  }

-- ** Weapons

dagger = ItemKind
  { isymbol  = symbolEdged
  , iname    = "cleaver"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 200)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(3 * 10/15, 40), (4 * 10/15, 1)]
                 -- no weapons brought by aliens, initially, so cleaver common
  , iverbHit = "cut"
  , iweight  = 1000
  , idamage  = 6 `d` 1
  , iaspects = [ Timeout 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorMelee $ (1 `d` 2) * 5
                   -- very common, so don't make too random
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "A heavy professional kitchen blade. Will do fine cutting any kind of meat and bone, as well as parrying blows. Does not penetrate deeply, but is quick to move and hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { iname    = "Double Dagger"
  , ifreq    = [(TREASURE, 20), (MUSEAL, 100)]
  , irarity  = [(1, 3), (10, 3)]
  , iaspects = [SetFlag Unique]
               ++ iaspects dagger
  , ieffects = [DropBestWeapon, Yell]  -- powerful and low timeout, but makes
                                       -- noise and useless against stupid foes
  , idesc    = "An antique double dagger that a focused fencer can use to catch and twist away an opponent's blade."
  }
hammerTemplate = ItemKind
  { isymbol  = symbolHafted
  , iname    = "demolition hammer"
  , ifreq    = [(HAMMER_UNKNOWN, 1)]
  , iflavour = zipFancy [BrMagenta]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(5 * 10/15, 15), (8 * 10/15, 1)]
                 -- don't make it too common on lvl 3
  , iverbHit = "club"
  , iweight  = 1600
  , idamage  = 8 `d` 1  -- we are lying about the dice here, but the dungeon
                        -- is too small and the extra-dice hammers too rare
                        -- to subdivide this identification class by dice
  , iaspects = [ PresentAs HAMMER_UNKNOWN
               , SetFlag Durable, SetFlag Meleeable
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "One of many kinds of hammers employed in construction work. The ones with completely blunt heads don't cause grave wounds, but any fitted with a long enough handle can shake and bruise even most armored foes. However, such large hammers require more time to recover after a swing. This one looks average at a quick glance."  -- if it's really the average kind, the weak kind, the description stays; if not, it's replaced with one of the descriptions below at identification time
  , ikit     = []
  }
hammer1 = hammerTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (STARTING_WEAPON, 70)
               , (FIRE_FIGHTING_ITEM, 7) ]
  , iaspects = [Timeout 5, EqpSlot EqpSlotWeaponBig]
               ++ iaspects hammerTemplate
  }
hammer2 = hammerTemplate
  { ifreq    = [ (COMMON_ITEM, 25), (STARTING_WEAPON, 7)
               , (FIRE_FIGHTING_ITEM, 2) ]
  , iverbHit = "gouge"
  , iaspects = [Timeout 3, EqpSlot EqpSlotWeaponFast]
               ++ iaspects hammerTemplate
  , idesc    = "Upon closer inspection, this hammer turns out particularly handy and well balanced, with a narrowing, sharpened head compensating the modest size."
  }
hammer3 = hammerTemplate
  { ifreq    = [ (COMMON_ITEM, 5), (STARTING_WEAPON, 1)
               , (FIRE_FIGHTING_ITEM, 1) ]
  , iverbHit = "puncture"
  , iweight  = 2400  -- weight gives it away
  , idamage  = 12 `d` 1
  , iaspects = [ Timeout 12  -- balance, or @DupItem@ would break the game
               , EqpSlot EqpSlotWeaponBig]
               ++ delete (PresentAs HAMMER_UNKNOWN) (iaspects hammerTemplate)
  , idesc    = "This hammer sports a long metal handle that increases the momentum of the sharpened head's swing, at the cost of long recovery."
  }
hammerParalyze = hammerTemplate
  { iname    = "Concussion Hammer"
  , ifreq    = [(TREASURE, 20)]
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = [ SetFlag Unique
               , Timeout 5
               , EqpSlot EqpSlotWeaponBig ]
               ++ iaspects hammerTemplate
  , ieffects = [Paralyze 10]
  , idesc    = "This exquisite demolition hammer with a titanium head and excepthionally long synthetic handle leaves no wall and no body standing."
  }
hammerSpark = hammerTemplate
  { iname    = "Grand Smithhammer"
  , ifreq    = [(TREASURE, 20), (MUSEAL, 100)]
  , irarity  = [(5, 1), (8, 6)]
  , iweight  = 2400  -- weight gives it away
  , idamage  = 12 `d` 1
  , iaspects = [ SetFlag Unique
               , Timeout 10
               , EqpSlot EqpSlotWeaponBig
               , AddSkill SkShine 3]
               ++ delete (PresentAs HAMMER_UNKNOWN) (iaspects hammerTemplate)
  , ieffects = [Explode S_SPARK]
      -- we can't use a focused explosion, because it would harm the hammer
      -- wielder as well, unlike this one
  , idesc    = "High carbon steel of this heavy old hammer doesn't yield even to the newest alloys and produces fountains of sparks in defiance."
  }
sword = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "sharpened pipe"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 30)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(3, 1), (6, 20)]
  , iverbHit = "stab"
  , iweight  = 2000
  , idamage  = 10 `d` 1
  , iaspects = [ Timeout 7
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "A makeshift weapon of simple design, but great potential. Hard to master, though."
  , ikit     = []
  }
swordImpress = sword
  { isymbol  = symbolEdged
  , iname    = "Master's Sword"
  , ifreq    = [(TREASURE, 20), (MUSEAL, 100)]
  , irarity  = [(5, 1), (8, 7)]
  , iverbHit = "slash"
  , iaspects = [SetFlag Unique]
               ++ iaspects sword
  , ieffects = [Impress]
  , idesc    = "A particularly well-balance museum piece. It has a long history and in the right hands lends itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { isymbol  = symbolEdged
  , iname    = "Roasting Rapier"
  , ifreq    = [(TREASURE, 20)]
  , iverbHit = "pierce"
  , irarity  = [(5, 1), (8, 7)]
  , iaspects = [SetFlag Unique, Timeout 3, EqpSlot EqpSlotWeaponFast]
               ++ (iaspects sword \\ [Timeout 7, EqpSlot EqpSlotWeaponBig])
  , ieffects = [ DropItem 1 maxBound COrgan CONDITION
               , RefillCalm (-10)
               , Yell ]
  , idesc    = "A thin, acutely sharp steel blade that pierces deeply and sends its victim into abrupt, sobering shock. Originally, an exuberant hand-forged roasting implement, intentionally and wisely kept blunt."
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "pole cleaver"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 20)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 1), (8, 15)]
  , iverbHit = "impale"
  , iweight  = 3000
  , idamage  = 12 `d` 1
  , iaspects = [ Timeout 10
               , AddSkill SkHurtMelee $ (-5 + 1 `dL` 3) * 5
                   -- useless against armor at game start
               , AddSkill SkArmorMelee 20
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 20 ]  -- not balanced
  , ieffects = []
  , idesc    = "An improvised but deadly weapon made of a long, sharp kitchen knife glued and bound to a long pole. Not often one succeeds in making enough space to swing it freely, but even when stuck between terrain obstacles it blocks approaches effectively and makes using other weapons difficult, both by friends and foes."
  , ikit     = []
  }
halberd2 = halberd
  { iname    = "pollaxe"
  , ifreq    = [(COMMON_ITEM, 3 * 3), (STARTING_WEAPON, 1)]
  , iverbHit = "carve"
  , iweight  = 4000
  , iaspects = [AddSkill SkHurtMelee $ (-6 + 1 `dL` 4) * 10]
                 -- balance, or @DupItem@ would break the game;
                 -- together with @RerollItem@, it's allowed to, though
               ++ (iaspects halberd
                   \\ [AddSkill SkHurtMelee $ (-6 + 1 `dL` 4) * 5])
  , idamage  = 18 `d` 1
  , idesc    = "A long-hafted axe: once used for maintenance, now turned to a bloodier purpose."
  }
halberdPushActor = halberd
  { iname    = "Swiss Halberd"
  , ifreq    = [(CURIOUS_ITEM, 20)]  -- not museum; reenactors
  , irarity  = [(7, 0), (9, 15)]
  , iaspects = [SetFlag Unique]
               ++ iaspects halberd
  , ieffects = [PushActor (ThrowMod 200 100 1)]  -- 2 steps, slow
  , idesc    = "A perfect replica made for a reenactor troupe, hardened, missing only some final sharpening. Versatile, with great reach and leverage. Foes are held at a distance."
  }

-- ** Treasure

gemTemplate = ItemKind
  { isymbol  = symbolGold
  , iname    = "gem"
  , ifreq    = [(GEM_UNKNOWN, 1), (VALUABLE, 100)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = [(3, 0), (10, 24)]
  , iverbHit = "tap"
  , iweight  = 50
  , idamage  = 0
  , iaspects = [PresentAs GEM_UNKNOWN, SetFlag Precious]
  , ieffects = []
  , idesc    = "Precious, though useless. Worth around 100 gold grains."
  , ikit     = []
  }
gem1 = gemTemplate
  { ifreq    = [ (TREASURE, 100), (GEM, 100), (ANY_JEWELRY, 100)
               , (VALUABLE, 100) ]
  , irarity  = [(3, 0), (6, 12), (10, 8)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
                 -- reflects strongly, distracts; so it glows in the dark,
                 -- is visible on dark floor, but not too tempting to wear
               ++ iaspects gemTemplate
  }
gem2 = gem1
  { ifreq    = [ (TREASURE, 100), (GEM, 100), (ANY_JEWELRY, 100)
               , (VALUABLE, 100) ]
  , irarity  = [(5, 0), (7, 25), (10, 8)]
  }
gem3 = gem1
  { ifreq    = [ (TREASURE, 100), (GEM, 100), (ANY_JEWELRY, 100)
               , (VALUABLE, 100) ]
  , irarity  = [(7, 0), (8, 20), (10, 8)]
  }
gem4 = gem1
  { ifreq    = [ (TREASURE, 100), (GEM, 100), (ANY_JEWELRY, 100)
               , (VALUABLE, 100) ]
  , irarity  = [(9, 0), (10, 70)]
  }
gem5 = gem1
  { isymbol  = symbolSpecial
  , iname    = "stimpack"
  , ifreq    = [ (TREASURE, 100), (GEM, 25), (ANY_JEWELRY, 10)
               , (VALUABLE, 100) ]
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
  , ifreq    = [(CURRENCY_UNKNOWN, 1), (VALUABLE, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + 1 `d` 20 + 1 `dL` 20
  , irarity  = [(1, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [PresentAs CURRENCY_UNKNOWN, SetFlag Precious]
  , ieffects = []
  , idesc    = "Reliably valuable in every civilized place."
  , ikit     = []
  }
currency = currencyTemplate
  { ifreq    = [(TREASURE, 100), (S_CURRENCY, 100), (VALUABLE, 1)]
  , iaspects = [AddSkill SkShine 1, AddSkill SkSpeed (-1)]
               ++ iaspects currencyTemplate
  }

-- * Allure-specific items

needle = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "needle"
  , ifreq    = [ (NEEDLE, 1), (COMMON_ITEM, 1)
                   -- marked as common to ensure can be polymorphed
               , (UNREPORTED_INVENTORY, 1) ]  -- too weak to spam
  , iflavour = zipPlain [Blue]
  , icount   = 1 + 8 `d` 3
  , irarity  = [(1, 1)]
  , iverbHit = "prick"
  , iweight  = 3
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ -10 * 5
               , SetFlag Fragile  -- breaks easily despite being piercing
               , ToThrow $ ThrowMod 70 100 3 ]  -- piercing; good shape
  , ieffects = []
  , idesc    = "A long sturdy hypodermic needle ending in a dried out micro-syringe that is easy to break off. It's too thin to cause great harm, but it passes through flesh easily."
  , ikit     = []
  }
needleSleep = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "tranquillizer dart"
  , ifreq    = [ (TRANQUILIZER_DART, 1), (COMMON_ITEM, 1)
               , (MERCENARY_AMMO, 25) ]
                   -- marked as common to ensure can be polymorphed
  , iflavour = zipPlain [BrBlue]
  , icount   = 1 `dL` 3
  , irarity  = [(1, 1)]
  , iverbHit = "prick"
  , iweight  = 10
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ -10 * 5
               , SetFlag Fragile
               , toVelocity 70 ]  -- syringe blocks piercing; slender fins
  , ieffects = [PutToSleep]
  , idesc    = "A long hypodermic needle ending in a micro-syringe with residues of the sleeping agent."
  , ikit     = []
  }
constructionHooter = necklaceTemplate
  { iname    = "construction hooter"
  , ifreq    = [ (CONSTRUCTION_HOOTER, 1), (COMMON_ITEM, 1)
                   -- extremely rare, but dropped by decontamination chambers
               , (UNREPORTED_INVENTORY, 1) ]
  , iflavour = zipPlain [BrRed]
  , irarity  = [(1, 1)]
  , iweight  = 1000
  , iaspects = [ AddSkill SkArmorMelee 2
               , SetFlag Durable, toVelocity 50
               , SetFlag Equipable, EqpSlot EqpSlotArmorMelee]
  , ieffects = [Yell, Summon CONSTRUCTION_ROBOT 1]
  , idesc    = "An emergency hooter for alarming human personnel in case their life is in danger. Worn by construction robots around their \"neck\", where it's least exposed, but even there it needs to be heavily armored and running on its own power supply."
  }
wasteContainer = ItemKind
  { isymbol  = symbolTool
  , iname    = "waste container"
  , ifreq    = [ (WASTE_CONTAINER, 1), (UNREPORTED_INVENTORY, 1)
               , (WATER_SOURCE, 1) ]
  , iflavour = zipLiquid [Green]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "spill over"
  , iweight  = 30000
  , idamage  = 0
  , iaspects = [ Timeout $ (1 `d` 2) * 30  -- robots should not summon too often
               , AddSkill SkArmorMelee 20  -- tempting
               , AddSkill SkMaxCalm (-30)  -- prevent excessive stacking
               , SetFlag Periodic, SetFlag Equipable ]
  , ieffects = [ Detect DetectLoot 20
               , Summon MOBILE_ANIMAL $ 1 `dL` 2
               , Explode S_WASTE ]
                   -- very important effect that disables item movement
                   -- and so makes wielding seeingItem a tiny bit risky,
                   -- and so not a no-brainer
  , idesc    = "Waste recognition and utilization subsystem. Detects any stray item not registered as passenger cargo. Leaks a little."
  , ikit     = []
  }
spotlight = ItemKind
  { isymbol  = symbolLight
  , iname    = "spotlight"
  , ifreq    = [(SPOTLIGHT, 1), (UNREPORTED_INVENTORY, 1)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "illuminate"
  , iweight  = 3000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine 4
               , AddSkill SkHurtMelee (-3)  -- heavy and unwieldy
               , SetFlag Equipable, EqpSlot EqpSlotShine ]
  , ieffects = [Detect DetectHidden 20]
  , idesc    = "Powerful wide-beam spotlight in an unwieldy rack-mounted package. On overdrive, it can shine through thin construction surfaces, underlying fault lines."
  , ikit     = []
  }
flask18 = flaskTemplate
  { iname    = "cartridge"
  , ifreq    = [ (COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)
               , (LIQUID_NITROGEN, 1), (COLD_SOURCE, 1)
               , (FIRE_FIGHTING_ITEM, 60) ]
  , irarity  = [(1, 1)]  -- scavenged from walls
  , iaspects = ELabel "of liquid nitrogen"
               : iaspects flaskTemplate
  , ieffects = [ Burn 1  -- sensory ambiguity between hot and cold
               , toOrganBad S_SLOWED (3 + 1 `d` 3)
               , OnSmash (Explode S_NITROGEN_MIST) ]
  }
flask19 = flaskTemplate
  { iname    = "galon"  -- diluted perfume; almost same effects
  , ifreq    = [ (COMMON_ITEM, 100), (ROSE_WATER_FLASK, 1)
               , (EXPLOSIVE, 100), (ANY_GLASS, 100), (WATER_SOURCE, 1) ]
  , irarity  = [(1, 1)]  -- mostly obtained through crafting
  , iaspects = ELabel "of rose water"
               : iaspects flaskTemplate
  , ieffects = [ Impress, toOrganGood S_ROSE_SMELLING (100 + 1 `d` 20)
               , OnSmash ApplyPerfume, OnSmash (Explode S_FRAGRANCE) ]
  }
flask20 = flaskTemplate
  { iname    = "galon"
  , ifreq    = [ (COMMON_ITEM, 100), (ANY_GLASS, 100)
               , (WATER_SOURCE, 1) ]
  , irarity  = [(1, 1)]  -- usually self-made (TODO)
  , iaspects = ELabel "of water"
               : iaspects flaskTemplate
  }
scrollAd1 = scrollTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_SCROLL, 100)]
  , icount   = 1
  , irarity  = [(1, 1)]  -- not every playthrough needs one
  , iaspects = [ELabel "of tourist guide"]
               ++ iaspects scrollTemplate
  , ieffects = [ Impress
               , toOrganGood S_RESOLUTE (500 + 1 `d` 200)
                   -- a drawback (at least initially) due to @calmEnough@
               , Explode S_RHINO_HOLOGRAM
               , Detect DetectLoot 5 ]  -- short so useless most of the time
  , idesc    = "Biodegradable self-powered mini-projector displaying holographic ads and shopping hints."
  }
blowtorch = ItemKind
  { isymbol  = symbolLight
  , iname    = "blowtorch"  -- not unique, but almost never generated on floor
  , ifreq    = [ (BLOWTORCH, 1), (VALUABLE, 20), (CURIOUS_ITEM, 1)
               , (BREACHING_TOOL, 1), (FIRE_SOURCE, 1) ]
                 -- infinite use, but harmful
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "scorch"
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ Timeout 4
               , AddSkill SkAlter 2
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotAlter ]
  , ieffects = [Burn 2, Impress]
      -- is used for melee in precedence to fists, but not to cleavers;
      -- so if player wants to hit with it, it's enough to pack other gear
  , idesc    = "A sturdy old-fashioned portable blowtorch for fine cutting or welding of metals. Rather weak, but does not require access codes to high current power outlets. If you can patiently suffer the heat, it can be used as a clumsy breaching tool."
  , ikit     = []
  }
rawMeatChunk = ItemKind
  { isymbol  = symbolFood
  , iname    = "raw meat chunk"
  , ifreq    = [(RAW_MEAT_CHUNK, 100), (COMMON_ITEM, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "slap"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [toVelocity 50]
  , ieffects = [DropItem maxBound 1 COrgan S_HUNGRY]
  , idesc    = "A scrap of edible animal meat. Not very tasty nor nourishing. Cooking would make it more palatable."
  , ikit     = []
  }
roastedMeatChunk = rawMeatChunk
  { iname    = "roasted meat chunk"
  , ifreq    = [ (ROASTED_MEAT_CHUNK, 100), (COOKED_FOOD, 60)
               , (COMMON_ITEM, 1) ]
  , iflavour = zipPlain [Brown]
  , ieffects = [DropItem maxBound 3 COrgan S_HUNGRY]
  , idesc    = "Delicious and filling chunk of meat. The thermal processing released flavour and made it easier to digest."
  }
militaryKnife = dagger
  { iname    = "military knife"
  , ifreq    = [(COMMON_ITEM, 1), (MERCENARY_WEAPON, 70)]
  , irarity  = [(10, 10)]
  , idamage  = 8 `d` 1
  , iaspects = [ Timeout 2
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = [DropItem 1 maxBound COrgan CONDITION]
  , idesc    = "Millitary design laser-sharpened alloy blade able to cleanly open an artery at the lightest touch through layers of fabric."
  }
militaryBaton = ItemKind
  { isymbol  = symbolHafted
  , iname    = "military stun gun"
  , ifreq    = [(COMMON_ITEM, 1), (MERCENARY_WEAPON, 30)]
  , iflavour = zipFancy [Magenta]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "prod"
  , iweight  = 1000
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout 7
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]
  , ieffects = [Paralyze 10, Discharge $ 40 - 1 `d` 20, RefillCalm (-30)]
  , idesc    = "A direct contact electroshock weapon with unlimited and fast recharging. Ideal for close quarter fights inside space habitats, where preserving the integrity of the outer hull is paramount."
  , ikit     = []
  }
cattleProd = militaryBaton
  { iname    = "electric cattle prod"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipFancy [Brown]
  , irarity  = [(10, 10)]
  , idamage  = 2 `d` 1
  , ieffects = [Discharge $ 40 - 1 `d` 20, RefillCalm (-30)]
  , idesc    = "Used for subduing unruly zoo animals."
  }
chisel = ItemKind  -- ignored by AI, but that's fine, others suffice
  { isymbol  = symbolTool
  , iname    = "chisel"
  , ifreq    = [(COMMON_ITEM, 100), (BREACHING_TOOL, 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(1, 6)]
  , iverbHit = "dismantle"
  , iweight  = 500
  , idamage  = 0
  , iaspects = []  -- lost after one use; a consumable
  , ieffects = []
  , idesc    = "It is a breaching tool."  -- TODO: https://en.wikipedia.org/wiki/Chisel
                   -- also say light and cheap, but not durable; one time use
  , ikit     = []
  }
steelFile = chisel
  { iname    = "steel file"
  , idesc    = "It is a breaching tool."  -- TODO: https://en.wikipedia.org/wiki/File_(tool)
  }
hacksaw = chisel
  { iname    = "hacksaw"
  , idesc    = "It is a breaching tool."  -- TODO: https://en.wikipedia.org/wiki/Hacksaw
  }
adjustableSpanner = chisel
  { iname    = "adjustable spanner"
  , idesc    = "It is a breaching tool."  -- TODO: https://en.wikipedia.org/wiki/Adjustable_spanner
  }
crowbar = chisel
  { iname    = "crowbar"
  , iflavour = zipPlain [BrCyan]
  , iverbHit = "gouge"
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 4
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 30 ]
  , idesc    = "It is sharpened to be usable as an improvised melee weapon, but it can be still employed as a breaching tool, though rather injurious."  -- TODO: https://en.wikipedia.org/wiki/Crowbar_(tool)
  }
catsPaw = chisel
  { iname    = "cat's paw"
  , iflavour = zipPlain [BrCyan]
  , iverbHit = "paw"
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout 3
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 50 ]
  , idesc    = "It is sharpened to be usable as an improvised melee weapon, but it can be still employed as a breaching tool, though rather injurious."  -- TODO: https://en.wikipedia.org/wiki/Cat%27s_paw_(nail_puller)
  }
diagonalPliers = chisel
  { iname    = "pair"
  , ifreq    = [(COMMON_ITEM, 100), (WIRECUTTING_TOOL, 1)]
  , iflavour = zipPlain [Brown]
  , iaspects = [ELabel "of diagonal pliers"]
  , idesc    = "It is a wirecutting tool."  -- TODO: https://en.wikipedia.org/wiki/Diagonal_pliers
  }
snips = diagonalPliers
  { iname    = "pair"
  , iaspects = [ELabel "of snips"]
  , idesc    = "It is a wirecutting tool."  -- TODO: https://en.wikipedia.org/wiki/Snips
  }
loppers = diagonalPliers
  { iname    = "pair"
  , iaspects = [ELabel "of loppers"]
  , idesc    = "It is a wirecutting tool."  -- TODO: https://en.wikipedia.org/wiki/Loppers
  }
boltCutter = diagonalPliers
  { iname    = "bolt cutter"
  , iaspects = []
  , idesc    = "It is a wirecutting tool."  -- TODO: https://en.wikipedia.org/wiki/Bolt_cutter
  }
gardenMsg :: Effect
gardenMsg = VerbMsg "feel the gardening tool fracture"
grassStitcher = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "grass stitcher"
  , ifreq    = [(COMMON_ITEM, 100), (S_GRASS_STITCHER, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 10), (3 * 10/15, 3), (4 * 10/15, 1)]
  , iverbHit = "stab"
  , iweight  = 500
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout 3  -- light and can hit with any side
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]
  , ieffects = -- TODO: instead introduce items with finite number of charges?
               [OneOf [ DestroyItem 1 1 CEqp S_GRASS_STITCHER
                      , gardenMsg, gardenMsg, gardenMsg ]]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Grass_Stitcher
  , ikit     = []
  }
ladiesFork = grassStitcher
  { iname    = "ladies' fork"
  , ifreq    = [(COMMON_ITEM, 100), (S_LADIES_FORK, 1)]
  , iflavour = zipPlain [Green]
  , iweight  = 1000
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]
  , ieffects = [OneOf [ DestroyItem 1 1 CEqp S_LADIES_FORK
                      , gardenMsg, gardenMsg
                      , gardenMsg, gardenMsg, gardenMsg ]]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Garden_fork
  }
spade = grassStitcher
  { isymbol  = symbolHafted  -- swinging much more deadly than gouging
  , iname    = "spade"
  , ifreq    = [(COMMON_ITEM, 100), (S_SPADE, 1)]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "cut"
  , iweight  = 2000
  , idamage  = 7 `d` 1
  , iaspects = [ Timeout 7
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]
  , ieffects = [OneOf [ DestroyItem 1 1 CEqp S_SPADE
                      , gardenMsg, gardenMsg, gardenMsg, gardenMsg
                      , gardenMsg, gardenMsg, gardenMsg ]]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Spade
  }
hoe = grassStitcher
  { isymbol  = symbolHafted
  , iname    = "hoe"
  , ifreq    = [(COMMON_ITEM, 100), (S_HOE, 1)]
  , iflavour = zipPlain [Brown]
  , iverbHit = "hack"
  , iweight  = 1000
  , idamage  = 6 `d` 1  -- neither sharp nor heavy
  , iaspects = [ Timeout 6
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]
  , ieffects = [OneOf [ DestroyItem 1 1 CEqp S_HOE
                      , gardenMsg, gardenMsg, gardenMsg
                      , gardenMsg, gardenMsg, gardenMsg ]]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Hoe_(tool)
  }
