-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2020 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Item definitions.
module Content.ItemKind
  ( -- * Group name patterns
    pattern HARPOON, pattern TORSO_ARMOR, pattern CLOTHING_MISC
  , pattern FIREPROOF_CLOTH, pattern COOKED_PLANT, pattern LIQUID_NITROGEN
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
    ++ embedsGNSingleton ++ actorsGNSingleton ++ organsGNSingleton
    ++ blastsGNSingleton ++ temporariesGNSingleton

pattern FLASK_UNKNOWN, ANY_POTION_UNKNOWN, EDIBLE_PLANT_UNKNOWN, SCROLL_UNKNOWN, NECKLACE_UNKNOWN, RING_UNKNOWN, HAMMER_UNKNOWN, GEM_UNKNOWN, CURRENCY_UNKNOWN :: GroupName ItemKind

pattern S_GRASS_STITCHER, S_LADIES_FORK, S_SPADE, S_HOE :: GroupName ItemKind

pattern COOKED_PLANT_UNKNOWN :: GroupName ItemKind

groupNames :: [GroupName ItemKind]
groupNames =
       [HARPOON, TORSO_ARMOR, CLOTHING_MISC]
    ++ [FIREPROOF_CLOTH, COOKED_PLANT, LIQUID_NITROGEN, GARDENING_TOOL]
    ++ embedsGN ++ actorsGN ++ organsGN ++ blastsGN

pattern HARPOON, TORSO_ARMOR, CLOTHING_MISC :: GroupName ItemKind

pattern FIREPROOF_CLOTH, COOKED_PLANT, LIQUID_NITROGEN, GARDENING_TOOL :: GroupName ItemKind

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
pattern CLOTHING_MISC = GroupName "miscellaneous clothing"

-- ** Allure-specific

-- The @UNKNOWN@ patterns don't need to be exported. Used internally.
-- They also represent singleton groups.
pattern COOKED_PLANT_UNKNOWN = GroupName "cooked plant unknown"

pattern FIREPROOF_CLOTH = GroupName "fireproof cloth"
pattern COOKED_PLANT = GroupName "cooked plant"
pattern LIQUID_NITROGEN = GroupName "liquid nitrogen"
pattern GARDENING_TOOL = GroupName "gardening tool"

pattern S_GRASS_STITCHER = GroupName "grass stitcher"
pattern S_LADIES_FORK = GroupName "ladies' fork"
pattern S_HOE = GroupName "hoe"
pattern S_SPADE = GroupName "spade"

-- * Content

content :: [ItemKind]
content = items ++ otherItemContent

otherItemContent :: [ItemKind]
otherItemContent = embeds ++ actors ++ organs ++ blasts ++ temporaries

items :: [ItemKind]
items =
  [sandstoneRock, dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, potion13, potion14, potion15, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, ediblePlant8, cookedPlantTemplate, cookedPlant1, cookedPlant2, cookedPlant3, cookedPlant4, cookedPlant5, cookedPlant6, cookedPlant7, cookedPlant8, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, scroll14, scroll15, jumpingPole, meleeEnhancement, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorLeather2, armorMail, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, buckler, shield, shield2, shield3, knife, daggerDropBestWeapon, hammerTemplate, hammer1, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberdPushActor, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency]
  -- Allure-specific
  ++ [steelScrap, needle, needleSleep, harpoon3, flask15, flask16, flask17, rawMeatChunk, roastedMeatChunk, scrollAd1, blowtorch, chisel, hacksaw, adjustableSpanner, steelFile, honingSteel, whetstone, laserSharpener, diagonalPliers, snips, loppers, boltCutter, solderingIron, duckTape, heavyBoot, spacesuit, spacesuitTorn, crowbar, catsPaw, fireAxe, dagger, hammer4, hammer5, swordNullifySharp, oxTongue, halberdPushActorSharp, militaryKnife, militaryBaton, cattleProd, grassStitcher, ladiesFork, hoe, spade, treePruner, cleaningPole, staff, pipe, longPole, constructionHooter, wasteContainer, spotlight, ragTangle]

sandstoneRock,    dart, spike, spike2, slingStone, slingBullet, paralizingProj, harpoon, harpoon2, net, light1, light2, light3, blanket, flaskTemplate, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, potionTemplate, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, potion10, potion11, potion12, potion13, potion14, potion15, fragmentationBomb, concussionBomb, flashBomb, firecrackerBomb, ediblePlantTemplate, ediblePlant1, ediblePlant2, ediblePlant3, ediblePlant4, ediblePlant5, ediblePlant6, ediblePlant7, ediblePlant8, cookedPlantTemplate, cookedPlant1, cookedPlant2, cookedPlant3, cookedPlant4, cookedPlant5, cookedPlant6, cookedPlant7, cookedPlant8, scrollTemplate, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, scroll12, scroll13, scroll14, scroll15, jumpingPole, meleeEnhancement, seeingItem, motionScanner, gorget, necklaceTemplate, necklace1, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, necklace10, imageItensifier, sightSharpening, ringTemplate, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, armorLeather, armorLeather2, armorMail, gloveFencing, gloveGauntlet, gloveJousting, hatUshanka, capReinforced, helmArmored, buckler, shield, shield2, shield3, knife, daggerDropBestWeapon, hammerTemplate, hammer1, hammer2, hammer3, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberd2, halberdPushActor, gemTemplate, gem1, gem2, gem3, gem4, gem5, currencyTemplate, currency :: ItemKind
-- Allure-specific
steelScrap,       needle, needleSleep, harpoon3, flask15, flask16, flask17, rawMeatChunk, roastedMeatChunk, scrollAd1, blowtorch, chisel, hacksaw, adjustableSpanner, steelFile, honingSteel, whetstone, laserSharpener, diagonalPliers, snips, loppers, boltCutter, solderingIron, duckTape, heavyBoot, spacesuit, spacesuitTorn, crowbar, catsPaw, fireAxe, dagger, hammer4, hammer5, swordNullifySharp, oxTongue, halberdPushActorSharp, militaryKnife, militaryBaton, cattleProd, grassStitcher, ladiesFork, hoe, spade, treePruner, cleaningPole, staff, pipe, longPole, constructionHooter, wasteContainer, spotlight, ragTangle :: ItemKind

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

symbolProjectile, _symbolLauncher, symbolLight, symbolTool, symbolSpecial, symbolGold, symbolNecklace, symbolRing, symbolPotion, symbolFlask, symbolScroll, symbolTorsoArmor, symbolMiscArmor, symbolClothes, symbolShield, symbolPolearm, symbolEdged, symbolHafted, symbolWand, _symbolStaff, symbolFood :: Char

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
symbolClothes    = '['
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
  , ifreq    = [ (S_SANDSTONE_ROCK, 1)
               , (UNREPORTED_INVENTORY, 1) ]  -- too weak to spam
  , iflavour = zipPlain [Green]
  , icount   = 1 + 1 `d` 2  -- > 1, to let AI ignore sole pieces
  , irarity  = [(1, 1)]
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
  , icount   = 1 + 1 `d` 2 + 4 `dL` 5
  , irarity  = [(1, 25)]
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
  , iflavour = zipPlain [BrCyan]
  , icount   = 1 + 1 `d` 2 + 3 `dL` 5
  , irarity  = [(1, 15), (10, 10)]
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
  , idesc    = "Not durable nor particularly well balanced, but with a laser-sharpened titanium alloy tip and blade."
  , ikit     = []
  }
spike2 = spike
  { iname    = "heavy steak knife"
  , ifreq    = [(COMMON_ITEM, 2), (ANY_ARROW, 1), (WEAK_ARROW, 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1 + 5 `dL` 5
  , iverbHit = "penetrate"
  , iweight  = 150
  , idamage = 4 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (-10 + 1 `d` 2 + 1 `dL` 3) * 5
               , SetFlag MinorEffects
               , Odds (10 * 1 `dL` 10) [] [toVelocity 70] ]
                   -- at deep levels sometimes even don't limit velocity
  , idesc    = "Old, slightly discoloured, probably from a genuine steel. A heavy and surprisingly well balanced prop from a posh restaurant. It won't survive any rough treatment, though."  -- the theme of pre-modern things being more solid and intimidating
  }
slingStone = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "steel hex nut"
  , ifreq    = [(COMMON_ITEM, 5), (ANY_ARROW, 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1 + 3 `dL` 4
  , irarity  = [(8, 25)]
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
  , iflavour = zipPlain [BrBlue]
  , icount   = 1 + 6 `dL` 4
  , irarity  = [(8, 20)]
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
               , (MERCENARY_AMMO, 25), (BONDING_TOOL, 1) ]
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
  , idesc    = "A can of liquid, fast-setting construction foam. Often used as a glue."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [(COMMON_ITEM, 100), (HARPOON, 100), (S_HARPOON_CARGO, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 1  -- durable, so one piece lasts long
  , irarity  = [(1, 6)]
  , iverbHit = "hook"
  , iweight  = 1400
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 5
               , AddSkill SkHurtMelee $ (-7 + 1 `d` 2 + 1 `dL` 3) * 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig ]
  , ieffects = [PullActor (ThrowMod 200 50 1)]  -- 1 step, fast
  , idesc    = "A cargo-hook with a high-tension cord that makes the entangled victim easy to unbalance with a strong pull."
  , ikit     = []
  }
harpoon2 = harpoon
  { iname    = "sharp harpoon"
  , ifreq    = [(COMMON_ITEM, 3), (HARPOON, 2), (S_HARPOON_SHARP, 1)]
  , idamage  = 7 `d` 1
  , idesc    = "A cord ending in a sharpened cargo-hook that, in addition to entangling the victim, gains purchase biting into the body."
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
  , irarity  = [(3, 12), (4, 1)]  -- crafted, so less rare
  , iverbHit = "scorch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine 3, AddSkill SkSight (-2)
                   -- not only flashes, but also sparks,
                   -- so unused by AI due to the mixed blessing
               , SetFlag Lobable, SetFlag Equipable
               , EqpSlot EqpSlotShine ]
                   -- not Fragile; reusable flare;
                   -- the staff culled when crafting, so no velocity malus
  , ieffects = [Burn 1]
  , idesc    = "A puttering torch improvised with rags on a stick, soaked in any lubricant or oil or resin or tar that could be scavenged in a hurry."
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
  , iflavour = zipPlain [Magenta]
  , icount   = 1
  , irarity  = [(1, 1)]  -- scavenged from walls
  , iverbHit = "swoosh"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkShine (-10)
               , AddSkill SkArmorMelee 2, AddSkill SkMaxCalm 5
               , SetFlag Lobable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
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
-- In fact, they cover all temporary conditions, except those for stats
-- resistance and regeneration. They never heal, directly nor indirectly
-- (regen), so may be thrown without the risk of wasting precious HP.
--
-- There is no flask nor condition that only does Calm or max Calm depletion,
-- because Calm reduced often via combat, etc.

flaskTemplate = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [(FLASK_UNKNOWN, 1)]
  , iflavour = zipGlassPlain darkCol ++ zipGlassFancy darkCol
               ++ zipLiquid darkCol
  , icount   = 1 `d` 2 + 1 `dL` 3
  , irarity  = [(1, 7), (10, 2)]
  , iverbHit = "splash"
  , iweight  = 500
  , idamage  = 0
  , iaspects = [ PresentAs FLASK_UNKNOWN, SetFlag Lobable, SetFlag Fragile
               , toVelocity 60 ]  -- oily, rather bad grip
  , ieffects = []
  , idesc    = "A flask of oily liquid of a suspect color. Something seems to be moving inside. Double dose causes twice longer effect. Triple dose is not advisable, since the active substance is never without unhealty side-efects and often dissolved in large volumes of alcohol."
  , ikit     = []
  }
flask1 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of strength brew"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_STRENGTHENED (20 + 1 `d` 5)
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
  , iaspects = ELabel "of resolution spirit"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_RESOLUTE (500 + 1 `d` 200)  -- long, for scouting
               , RefillCalm 60  -- not to make it a drawback, via @calmEnough@
               , OnSmash (Explode S_RESOLUTION_DUST) ]
  }
flask7 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , icount   = 1 `d` 2  -- too powerful en masse
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
  , icount   = 1 `d` 2 + 1 `d` 3
  , iaspects = ELabel "of whiskey"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_DRUNK (20 + 1 `d` 5)
               , Burn 10, RefillHP 10, Yell
               , OnSmash (Explode S_WHISKEY_SPRAY) ]
  }
flask12 = flaskTemplate
  { iname    = "flagon"
  , ifreq    = [ (COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)
               , (WATER_SOURCE, 1) ]
  , icount   = 1 `d` 2  -- too powerful, for aliens, en masse
  , iaspects = ELabel "of bait cocktail"
               : iaspects flaskTemplate
  , ieffects = [ toOrganGood S_DRUNK (20 + 1 `d` 5)
               , Summon MOBILE_ANIMAL 1
               , OnSmash (Summon MOBILE_ANIMAL 1)
               , OnSmash Impress  -- mildly useful when thrown
               , OnSmash (Explode S_WASTE) ]
  }
flask13 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of poison"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_POISONED, toOrganNoTimer S_POISONED  -- x2
               , OnSmash (Explode S_POISON_CLOUD) ]
  }
flask14 = flaskTemplate
  { ifreq    = [(COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)]
  , iaspects = ELabel "of calamity mixture"
               : iaspects flaskTemplate
  , ieffects = [ toOrganNoTimer S_POISONED
               , toOrganBad S_WEAKENED (20 + 1 `d` 5)
               , toOrganBad S_DEFENSELESS (20 + 1 `d` 5)
               , OnSmash (Explode S_GLASS_HAIL) ]  -- enough glass to cause that
  }

-- Vials are often natural, including natural stat boosts.
-- They also include the only healing consumables in the game,
-- apart of stimpacks and, to a limited extent, fruits.
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
  , idesc    = "A vial of bright, frothing concoction. The best medicine that nature has to offer for wounds, ailments and mood swings."
  , ikit     = []
  }
potion1 = potionTemplate
  { ifreq    = [ (COMMON_ITEM, 100), (S_PERFUME_POTION, 1)
               , (ANY_POTION, 100), (ANY_GLASS, 100) ]
  , icount   = 3 `dL` 1  -- very useful, despite appearances;
                         -- AI heroes can't craft and so die horribly without it
  , iaspects = ELabel "of perfume"
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
                                , Explode S_VIOLENT_SLOWNESS_MIST
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
               , OnSmash (Explode $ blastNoStatOf S_RETAINING)    -- more
               , OnSmash (Explode $ blastNoStatOf S_RETAINING) ]  -- explosion
  }
potion11 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 8)]
  , iaspects = ELabel "of panic"
               : iaspects potionTemplate
  , ieffects = [ RefillCalm (-60)
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
               , Discharge $ 40 - 1 `d` 20
               , toOrganBad S_IMMOBILE (5 + 1 `d` 5)
               , OnSmash (Explode S_HASTE_SPRAY)
               , OnSmash (Explode S_IRON_FILING)
               , OnSmash (Explode $ blastNoStatOf S_IMMOBILE) ]
  }
potion13 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 4)]
  , iaspects = ELabel "of slow resistance"
               : iaspects potionTemplate
  , ieffects = [ toOrganNoTimer S_POISON_RESISTANT
               , OnSmash (Explode S_ANTI_SLOW_MIST) ]
  }
potion14 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , irarity  = [(10, 4)]
  , iaspects = ELabel "of poison resistance"
               : iaspects potionTemplate
  , ieffects = [ toOrganNoTimer S_SLOW_RESISTANT
               , OnSmash (Explode S_ANTIDOTE_MIST) ]
  }
-- The player has full control over throwing the vial at his party,
-- so he can milk the explosion, so it has to be much weaker, so a weak
-- healing effect is enough. OTOH, throwing a harmful flask at many enemies
-- at once is not easy to arrange, so these explosions can stay powerful.
potion15 = potionTemplate
  { ifreq    = [(COMMON_ITEM, 100), (ANY_POTION, 100), (ANY_GLASS, 100)]
  , icount   = 1 `dL` 5
  , irarity  = [(1, 2), (10, 12)]
  , iaspects = ELabel "of regeneration"
               : iaspects potionTemplate
  , ieffects = [ toOrganGood S_ROSE_SMELLING (80 + 1 `d` 20)
               , toOrganNoTimer S_REGENERATING
               , toOrganNoTimer S_REGENERATING  -- x2
               , OnSmash (Explode S_YOUTH_SPRINKLE) ]
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
  , ieffects = [Explode S_FOCUSED_FLASH, OnSmash (Explode S_VIOLENT_FLASH)]
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
  , irarity  = [(1, 4)]  -- powerful; many copies
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
                       , OneOf [Summon HERO 1, Summon MOBILE_ANIMAL $ 1 `d` 2]
                           -- gaining a hero particularly uncommon
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
  , idesc    = "This is a security lock chip that opens all doors in the area, including the hatch to a nearby closet, resounding from the blows of, as it turns out, one of our lost crew members."
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
  , ifreq    = [(COMMON_ITEM, 100), (POLE, 10)]
  , iflavour = zipFancy [White]
  , icount   = 1
  , irarity  = [(6, 12)]
  , iverbHit = "prod"
  , iweight  = 4000
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
meleeEnhancement = ItemKind
  { isymbol  = symbolTool
  , iname    = "barebones exoskeleton"
  , ifreq    = [(COMMON_ITEM, 100)]
  , iflavour = zipFancy [Blue]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "zip"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee $ (1 `dL` 7) * 5
               , SetFlag Equipable
               , EqpSlot EqpSlotHurtMelee ]
  , ieffects = []
  , idesc    = "A minimal frame from carbon fibre, designed to prevent injuries when lifting and operating heavy construction equipment. Cheap, light, disposable."
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
               , Timeout 30
                   -- has to be as large as Calm drain or item not removable
               , AddSkill SkHearing 2 ]
               ++ iaspects_necklaceTemplate
  , ieffects = [ Detect DetectActor 10  -- can be applied; destroys the item
               , RefillCalm (-30) ]
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
  , irarity  = [(1, 1)]  -- powerful and determines tactics for one actor
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
  , ifreq    = [ (COMMON_ITEM, 10)  -- crafted, so can be rare
               , (S_REFRIGERATION_COIL, 1), (ANY_JEWELRY, 100)
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
  , idesc    = "Sturdy antique night vision goggles of unknown origin."
  , ikit     = []
  }
sightSharpening = ringTemplate  -- small and round, so mistaken for a ring
  { iname    = "Autozoom Contact Lens"
  , ifreq    = [(TREASURE, 40), (ADD_SIGHT, 1)]
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
  , idesc    = "The creator of this dangerous artifact didn't find time to document its operation. And now it's too late."
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
  { ifreq    = [(COMMON_ITEM, 100), (ANY_JEWELRY, 100), (MUSEAL, 50)]
  , irarity  = [(3, 3)]
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
  , iname    = "spacesuit jacket"
  , ifreq    = [ (COMMON_ITEM, 100), (S_SPACESUIT_JACKET, 1)
               , (SPACESUIT_PART, 1), (TORSO_ARMOR, 1) ]
  , iflavour = zipFancy [Blue]
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
  , idesc    = "A hard-shell torso segment of a disposed off spacesuit. Well ventilated through the air tank outlets."
  , ikit     = []
  }
armorLeather2 = armorLeather  -- for now, purely flavour, for better messages
  { isymbol  = symbolMiscArmor
  , iname    = "pair"
  , ifreq    = [ (COMMON_ITEM, 100), (S_SPACESUIT_TROUSERS, 1)
               , (SPACESUIT_PART, 1) ]
  , irarity  = [(3, 7), (10, 4)]
  , iaspects = ELabel "of spacesuit trousers" : iaspects armorLeather
  , idesc    = "Segmented trousers for open space work, with the hermetically sealed boots cut off. Surprisingly flexible and airy, yet micro-meteorite-proof."
  }
armorMail = armorLeather
  { iname    = "bulletproof vest"
  , ifreq    = [ (COMMON_ITEM, 100), (TORSO_ARMOR, 1), (ARMOR_RANGED, 1)
               , (S_BULLTEPROOF_VEST, 1) ]
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
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1), (ARMOR_RANGED, 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(6, 9), (10, 5)]
  , iverbHit = "flap"
  , iweight  = 100
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkHurtMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 3
               , AddSkill SkArmorRanged $ (1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotHurtMelee
               , toVelocity 40 ]  -- flaps and flutters
  , ieffects = []
  , idesc    = "A flexible construction glove from rough leather ensuring a good grip. Also, quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "spacesuit glove"
  , ifreq    = [ (COMMON_ITEM, 100), (S_SPACESUIT_GLOVE, 1)
               , (SPACESUIT_PART, 2), (ARMOR_MISC, 1) ]
  , iflavour = zipFancy [White]
  , irarity  = [(1, 9), (5 * 10/15, 9), (6 * 10/15, 1)]
  , iverbHit = "mow"
  , iweight  = 500
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout 3
               , AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 40 ]  -- flaps and flutters
  , idesc    = "A piece of a hull maintenance spacesuit, padded, reinforced with carbon fibre, with extruding titan manipulators."
  }
gloveJousting = gloveFencing
  { iname    = "Welding Handgear"
  , ifreq    = [(COMMON_ITEM, 100), (ARMOR_MISC, 1)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 6), (10, 3)]
  , iverbHit = "ram"
  , iweight  = 3000
  , idamage  = 5 `d` 1
  , iaspects = [ SetFlag Unique
               , Timeout 3
               , AddSkill SkHurtMelee $ (-7 + 1 `dL` 5) * 3
               , AddSkill SkArmorMelee $ (2 + 1 `d` 2 + 1 `dL` 2) * 5
               , AddSkill SkArmorRanged $ (1 + 1 `dL` 2) * 3
                 -- very random on purpose and can even be good on occasion
                 -- or when ItemRerolled enough times
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast  -- hope to replace with better soon
               , toVelocity 50 ]  -- flaps and flutters
  , idesc    = "Rigid, bulky handgear embedding a defunct welding equipment, complete with an affixed small shield and a darkened visor. Awe-inspiring."
  }
hatUshanka = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "ushanka hat"
  , ifreq    = [ (COMMON_ITEM, 100), (ARMOR_MISC, 1), (CLOTHING_MISC, 1)
               , (THICK_CLOTH, 1) ]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(3, 7), (10, 4)]
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
  , ifreq    = [ (COMMON_ITEM, 100), (S_SPACESUIT_HELMET, 1)
               , (SPACESUIT_PART, 1), (ARMOR_MISC, 1), (ARMOR_RANGED, 1) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(1, 11), (4 * 10/15, 11), (5 * 10/15, 1)]
  , iverbHit = "headbutt"
  , iweight  = 2000
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout 3
               , AddSkill SkArmorMelee $ (1 + 1 `dL` 4) * 5
               , AddSkill SkArmorRanged $ (2 + 1 `dL` 2) * 3  -- headshot
               , AddSkill SkSight (-1)
               , AddSkill SkHearing (-7), AddSkill SkSmell (-5)
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotArmorRanged
               , toVelocity 40 ]  -- unwieldy
  , ieffects = []
  , idesc    = "Blocks out everything, including your senses."
  , ikit     = []
  }
-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
-- Note that AI will pick them up but never wear and will use them at most
-- as a way to push itself. Despite being @Meleeable@, they will not be used
-- as weapons either. This is OK, using shields smartly is totally beyond AI.
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
               , EqpSlot EqpSlotArmorMelee ]  -- unwieldy to throw
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
  { ifreq    = [(COMMON_ITEM, 3 * 2), (MUSEAL, 50), (S_SHIELD_BLUNT, 1)]
                  -- very low base rarity
  , iweight  = 6000
  , idamage  = 5 `d` 1
  , idesc    = "A relic of long-past wars, heavy and with a central spike, which is however misaligned and dull."
  }
shield3 = shield2
  { ifreq    = [(COMMON_ITEM, 2), (MUSEAL, 10), (S_SHIELD_SHARP, 1)]
  , idamage  = 10 `d` 1
  , idesc    = "A relic of long-past wars, heavy and with a sharp central spike."
  }

-- ** Weapons

-- Generally, weapons on long poles have highest damage and defence,
-- but longest timeout. Weapons with handles are middling. Weapons
-- without area weakest, but lowest timeout and highest global melee bonus.
-- Weapons of a given group tend to share the weakest representative's
-- characteristics, even when upgraded. Sharpening of weapons usually
-- just increases their damage.

knife = ItemKind
  { isymbol  = symbolEdged
  , iname    = "cleaver"
  , ifreq    = [(COMMON_ITEM, 100), (STARTING_WEAPON, 200), (S_CLEAVER, 1)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 5), (5, 25), (9, 1)]
                 -- useful initially and for crafting mid-game
  , iverbHit = "cut"
  , iweight  = 1000
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 3
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 3 + 1 `dL` 2) * 3
                   -- very common, so don't make too random
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "A heavy professional kitchen blade. Will do fine cutting any kind of meat, bone and an occasional metal can. Does not penetrate deeply, but is quick to move and hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = knife
  { iname    = "Double Dagger"
  , ifreq    = [(TREASURE, 20), (MUSEAL, 100)]
  , irarity  = [(1, 3), (10, 3)]
  , iaspects = [SetFlag Unique]
               ++ iaspects knife
  , ieffects = [DropBestWeapon, Yell]  -- powerful and low timeout, but makes
                                       -- noise and useless against stupid foes
  , idesc    = "An antique double dagger that a focused fencer can use to catch and twist away an opponent's blade."
  }
hammerTemplate = ItemKind  -- properly hafted *and* glued to handle/pole
  { isymbol  = symbolHafted
  , iname    = "sledgehammer"  -- "demolition hammer" is Br. Eng. for jackhammer
  , ifreq    = [(HAMMER_UNKNOWN, 1)]
                 -- not @BREACHING_TOOL@, because it trigger traps
                 -- and destroys treasure, instead of opening; generally
                 -- a very aggressive weapon, bad for defense even when long
  , iflavour = zipFancy [BrMagenta]  -- avoid "pink"
  , icount   = 1
  , irarity  = [(7, 18), (9, 1)]
                 -- not too common on lvl 3 and late, when crafting done already
  , iverbHit = "club"
  , iweight  = 4000
  , idamage  = 0  -- all damage independent of melee skill; this also helps
                  -- not to lie about damage of unindentified items
  , iaspects = [ PresentAs HAMMER_UNKNOWN
               , SetFlag Durable, SetFlag Meleeable, EqpSlot EqpSlotWeaponBig
               , toVelocity 0 ]  -- totally unbalanced and so @RefillHP@ fails
  , ieffects = []
  , idesc    = "One of many kinds of hammers employed in construction work. The usual ones with blunt heads don't cause grave wounds, but enough weigth on a long handle can shake and bruise even most armored foes. However, larger hammers require more time to recover after a swing. This one looks average at a quick glance."  -- if it's really the average kind, the weak kind, the description stays; if not, it's replaced with one of the descriptions below at identification time
  , ikit     = []
  }
hammer1 = hammerTemplate  -- 1m handle, blunt
  { ifreq    = [ (COMMON_ITEM, 100), (HANDLE_AND_STEEL, 1)
               , (STARTING_WEAPON, 70), (STARTING_HAMMER, 80)
               , (S_BLUNT_SHORT_HAMMER, 1) ]
  , iaspects = [Timeout 5]
               ++ iaspects hammerTemplate
  , ieffects = [RefillHP (-8)]
  }
hammer2 = hammerTemplate  -- 0.75m handle, sharp
  { ifreq    = [(COMMON_ITEM, 30), (STARTING_WEAPON, 10), (STARTING_HAMMER, 15)]
  , irarity  = [(7, 20)]  -- common early, because not guaranteed;
                          -- common also late, because not crafted
  , iverbHit = "puncture"
  , iaspects = [Timeout 3, EqpSlot EqpSlotWeaponFast]
               ++ (iaspects hammerTemplate \\ [EqpSlot EqpSlotWeaponBig])
  , ieffects = [RefillHP (-8)]
  , idesc    = "Upon closer inspection, this hammer, or pick, turns out particularly well balanced. The profiled handle seamlessly joins the head, which focuses the blow at a sharp point, compensating for the tool's modest size."
  }
hammer3 = hammerTemplate  -- 2m pole, blunt
  { ifreq    = [ (COMMON_ITEM, 5), (BONDING_TOOL, 1), (POLE_AND_STEEL, 1)
               , (STARTING_WEAPON, 5), (S_BLUNT_LONG_HAMMER, 1) ]
  , iweight  = 6000  -- pole weight almost gives it away
  , iaspects = [Timeout 12]  -- balance, or @DupItem@ would break the game
               ++ iaspects hammerTemplate
  , ieffects = [RefillHP (-12)]
  , idesc    = "This hammer sports a long pole that increases the momentum of the blunt head's swing, at the cost of long recovery. It's capable of smashing objects together, though the required careful positioning often means hands are smashed as well."
  }
hammerParalyze = hammerTemplate
  { iname    = "Concussion Hammer"
  , ifreq    = [(TREASURE, 20), (STARTING_HAMMER, 5)]
  , irarity  = [(5, 1), (8, 6)]
  , iaspects = [ SetFlag Unique
               , Timeout 5 ]  -- 2m, but light head and pole
               ++ iaspects hammerTemplate
  , ieffects = [RefillHP (-8), Paralyze 10]
  , idesc    = "This exquisite demolition hammer with a titanium head and excepthionally long synthetic handle leaves no wall and no body standing."
  }
hammerSpark = hammerTemplate  -- the only hammer with significantly heavier head
  { iname    = "Grand Smithhammer"
  , ifreq    = [(TREASURE, 20), (BONDING_TOOL, 1), (MUSEAL, 100)]
  , irarity  = [(5, 1), (8, 6)]
  , iweight  = 5000  -- weight and shape/damage gives it away; always identified
  , idamage  = 8 `d` 1  -- different from all other hammers
  , iaspects = [ SetFlag Unique
               , Timeout 8  -- 1.5m handle and heavy, but unique
               , EqpSlot EqpSlotWeaponBig
               , SetFlag Durable, SetFlag Meleeable
               , toVelocity 0 ]  -- totally unbalanced and so @RefillHP@ fails
  , ieffects = [ Explode S_SPARK
                   -- we can't use a focused explosion, because it would harm
                   -- the hammer wielder as well, unlike this one
               , RefillHP (-3) ]
                   -- @RefillHP@ to avoid a no-brainer of durable tool use
  , idesc    = "High carbon steel of this heavy old hammer doesn't yield even to the newest alloys and produces fountains of sparks in defiance. Whatever it forge-welds together, stays together."
  }
sword = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "sharpened pipe"
  , ifreq    = [ (COMMON_ITEM, 5), (STARTING_WEAPON, 30)
               , (S_SHARPENED_PIPE, 1) ]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , irarity  = [(3, 1), (6, 20)]
  , iverbHit = "stab"
  , iweight  = 2000
  , idamage  = 11 `d` 1  -- with high melee bonus, much better than sharp hammer
  , iaspects = [ Timeout 7, EqpSlot EqpSlotWeaponBig
               , SetFlag Durable, SetFlag Meleeable
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
  , iname    = "Blunt Roasting Rapier"
  , ifreq    = [(TREASURE, 20), (S_RAPIER_BLUNT, 1)]
  , iverbHit = "pierce"
  , irarity  = [(5, 1), (8, 7)]
  , idamage  = 8 `d` 1
  , iaspects = [ SetFlag Unique, Timeout 3, EqpSlot EqpSlotWeaponFast
               , SetFlag Durable, SetFlag Meleeable
               , toVelocity 40 ]  -- ensuring it hits with the tip costs speed
  , ieffects = []
  , idesc    = "An exuberant hand-forged roasting implement, intentionally and wisely kept blunt."
  }
halberd = ItemKind  -- long pole
  { isymbol  = symbolPolearm
  , iname    = "pole cleaver"
  , ifreq    = [ (COMMON_ITEM, 5), (POLE_AND_STEEL, 1)
               , (STARTING_WEAPON, 5), (S_POLE_CLEAVER, 1) ]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 1), (8, 15)]
  , iverbHit = "slice"
  , iweight  = 3500
  , idamage  = 11 `d` 1  -- bad, until sharpened
  , iaspects = [ Timeout 10
               , AddSkill SkHurtMelee $ (-5 + 1 `dL` 3) * 5
                   -- useless against armor at game start
               , AddSkill SkArmorMelee 20
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 20 ]  -- not balanced
  , ieffects = []
  , idesc    = "An improvised but deadly weapon made of a long kitchen cleaver glued and bound to a long pole. Not often one succeeds in making enough space to swing it freely, but even when stuck between terrain obstacles it blocks approaches effectively and makes using other weapons difficult, both by friends and foes."
  , ikit     = []
  }
halberd2 = halberd
  { iname    = "pollaxe"
  , ifreq    = [(COMMON_ITEM, 1), (POLE_AND_STEEL, 1), (S_POLL_AXE, 1)]
                 -- not @BREACHING_TOOL@, because it triggers traps
                 -- and destroys treasure, instead of opening
  , iflavour = zipPlain [BrRed]
  , irarity  = [(1, 1)]
  , iverbHit = "carve"
  , iweight  = 4500
  , idamage  = 16 `d` 1
  , iaspects = [ Timeout 16
               , AddSkill SkHurtMelee $ (-7 + 1 `dL` 5) * 5
                   -- balance, or @DupItem@ would break the game;
                   -- together with @RerollItem@, it's allowed to, though
               , AddSkill SkArmorMelee 20
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 20 ]  -- not balanced
  , idesc    = "A long-hafted axe: once used for fire fighting, now turned to a bloodier purpose."
  }
halberdPushActor = halberd
  { iname    = "Blunt Swiss Halberd"
  , ifreq    = [(CURIOUS_ITEM, 20), (S_HALBERD_BLUNT, 1)]
                 -- not in a museum; reenactors' gear
  , irarity  = [(7, 0), (9, 15)]
  , iaspects = [SetFlag Unique]
               ++ iaspects halberd
  , ieffects = [PushActor (ThrowMod 200 100 1)]  -- 2 steps, slow
  , idesc    = "A perfect replica made for a reenactor troupe, hardened, but missing any sharpening. Versatile, with great reach and leverage. Foes are held at a distance."
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

-- ** Thrown weapons

steelScrap = sandstoneRock
  { iname    = "steel scrap"
  , ifreq    = [ (STEEL_SCRAP, 1)
               , (UNREPORTED_INVENTORY, 1) ]  -- too weak to spam
  , iflavour = zipPlain [Cyan]
  , iverbHit = "grate"
  , idesc    = " A bent lump of steel scrap."
  }
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
               , ToThrow $ ThrowMod 60 100 3 ]  -- piercing; good shape
  , ieffects = []
  , idesc    = "A long sturdy hypodermic needle ending in a dried out micro-syringe that is easy to break off. It's too thin to cause great harm, but it passes through flesh easily."
  , ikit     = []
  }

-- ** Exotic thrown weapons

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
               , toVelocity 60 ]  -- syringe blocks piercing; slender fins
  , ieffects = [PutToSleep]
  , idesc    = "A long hypodermic needle ending in a micro-syringe with residues of the sleeping agent."
  , ikit     = []
  }
harpoon3 = harpoon
  { iname    = "whaling harpoon"
  , ifreq    = [(TREASURE, 50), (MUSEAL, 100)]
  , iflavour = zipFancy [Red]
  , irarity  = [(8, 4)]
  , idamage  = 9 `d` 1
  , iaspects = [SetFlag Unique] ++ iaspects harpoon
  , ieffects = Yell  -- evoke a cry from pain; brutal
               : ieffects harpoon
  , idesc    = "A display piece harking back to the Earth's oceanic tourism heyday. Surprising sharp for its age. The cruel, barbed head lodges in its victim so painfully that the weakest tug of the rope sends the victim flying."
  }

-- ** Lights

-- ** Exploding consumables, often intended to be thrown.

flask15 = flaskTemplate
  { iname    = "cartridge"
  , ifreq    = [ (COMMON_ITEM, 100), (EXPLOSIVE, 100), (ANY_GLASS, 100)
               , (LIQUID_NITROGEN, 1), (COLD_SOURCE, 1)
               , (FIRE_FIGHTING_ITEM, 60) ]
  , irarity  = [(1, 3)]  -- scavenged from walls
  , iaspects = ELabel "of liquid nitrogen"
               : iaspects flaskTemplate
  , ieffects = [ Burn 1  -- sensory ambiguity between hot and cold
               , toOrganBad S_SLOWED (3 + 1 `d` 3)
               , OnSmash (Explode S_FOCUSED_SLOWNESS_MIST) ]
  }
flask16 = flaskTemplate
  { iname    = "galon"  -- diluted perfume; almost same effects
  , ifreq    = [ (COMMON_ITEM, 100), (S_ROSE_WATER_FLASK, 1)
               , (EXPLOSIVE, 100), (ANY_GLASS, 100), (WATER_SOURCE, 1) ]
  , irarity  = [(1, 1)]  -- mostly obtained through crafting
  , iaspects = ELabel "of rose water"
               : iaspects flaskTemplate
  , ieffects = [ Impress, toOrganGood S_ROSE_SMELLING (100 + 1 `d` 20)
               , OnSmash ApplyPerfume, OnSmash (Explode S_FRAGRANCE) ]
  }
flask17 = flaskTemplate
  { iname    = "galon"
  , ifreq    = [ (COMMON_ITEM, 100), (ANY_GLASS, 100)
               , (WATER_SOURCE, 1) ]
  , irarity  = [(1, 1)]  -- usually self-made (TODO)
  , iaspects = ELabel "of water"
               : iaspects flaskTemplate
  }

-- ** Explosives, with the only effect being @Explode@

-- ** Non-exploding consumables, not specifically designed for throwing

rawMeatChunk = ItemKind
  { isymbol  = symbolFood
  , iname    = "raw meat chunk"
  , ifreq    = [ (RAW_MEAT_CHUNK, 100), (COMMON_ITEM, 1)
               , (UNREPORTED_INVENTORY, 1) ]  -- no "fondles a trinket"
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

-- ** Assorted tools

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
               , EqpSlot EqpSlotAlter
               , toVelocity 0 ]  -- @Burn@ not effective when thrown
  , ieffects = [ Burn 3  -- ensure heroes wear initially, so they reach lvl 4
               , Impress ]
      -- is used for melee in precedence to fists, but not to cleavers;
      -- so if player wants to hit with it, it's enough to pack other gear;
      -- is also the low bar for self-inflicted damage from durable breaching
      -- tool and fire source use so that other tool-weapons need only
      -- do that many non-armor affected damage to dissuade the player
      -- from using them without careful thought
  , idesc    = "A sturdy old-fashioned portable blowtorch for fine cutting or welding of metals. Rather weak, but does not require access codes to high current power outlets. If you can patiently suffer the heat, it can be used as a clumsy breaching or bonding tool."
  , ikit     = []
  }
chisel = ItemKind  -- ignored by AI, but that's fine, others suffice
  { isymbol  = symbolTool
  , iname    = "chisel"
  , ifreq    = [(CURIOUS_ITEM, 100), (BREACHING_TOOL, 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , irarity  = [(2, 10)]
  , iverbHit = "dismantle"
  , iweight  = 500
  , idamage  = 0  -- not a missile to avoid wasting, before a worskhop found
  , iaspects = []  -- lost after one use; a consumable
  , ieffects = []
  , idesc    = "It is a breaching tool."  -- TODO: https://en.wikipedia.org/wiki/Chisel
                   -- also say light and cheap, but not durable; one time use
  , ikit     = []
  }
hacksaw = chisel
  { iname    = "hacksaw"
  , idesc    = "It is a breaching tool."  -- TODO: https://en.wikipedia.org/wiki/Hacksaw
  }
adjustableSpanner = chisel
  { iname    = "adjustable spanner"
  , idesc    = "It is a breaching tool."  -- TODO: https://en.wikipedia.org/wiki/Adjustable_spanner
  }
steelFile = chisel
  { iname    = "steel file"
  , ifreq    = [(CURIOUS_ITEM, 100), (BREACHING_TOOL, 1), (SHARPENING_TOOL, 1)]
  , irarity  = [(2, 15)]
  , iflavour = zipPlain [Blue]
  , iverbHit = "grate"
  , idesc    = "It is a breaching and sharpening tool."  -- TODO: https://en.wikipedia.org/wiki/File_(tool)
  }
honingSteel = chisel
  { iname    = "honing steel"
  , ifreq    = [(CURIOUS_ITEM, 100), (SHARPENING_TOOL, 1)]
  , iflavour = zipFancy [Blue]
  , iverbHit = "hone"
  , idesc    = "Originally used for realigning and sharpening dulled edges of kitchen knives in the local restaurants. Now it turns utensils into weapons."
  }
whetstone = honingSteel
  { iname    = "whetstone"
  , iverbHit = "rub"
  , idesc    = "A portable sharpening stone that can transforms a dull piece of scrap into a keen and true blade."
  }
laserSharpener = honingSteel
  { iname    = "laser sharpener"
  , ifreq    = [(CURIOUS_ITEM, 100), (SHARPENING_TOOL, 1)]
  , iflavour = zipFancy [BrBlue]
  , irarity  = [(10, 8)]
  , iweight  = 2000
  , idamage  = 0
  , iaspects = [ SetFlag Unique, Timeout 10
               , SetFlag Durable, SetFlag Meleeable, EqpSlot EqpSlotWeaponBig
               , toVelocity 0 ]  -- @Burn@ not effective when thrown
  , ieffects = [Burn 5]  -- really harmful when used as a sharpener; intended
  , idesc    = "Laser ablation is the safest and most accurate of sharpening method. Misaligned optics with broken shielding, however, change the situation dramatically, enabling stray laser pulses to escape at unpredictable angles."  -- hence short range and so melee weapon; TODO: long range weapon with instant projectiles and no risk of hull breach
  }
diagonalPliers = chisel
  { iname    = "pair"
  , ifreq    = [(CURIOUS_ITEM, 100), (WIRECUTTING_TOOL, 1)]
  , iflavour = zipPlain [Brown]
  , iverbHit = "cut"
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
solderingIron = chisel
  { iname    = "soldering iron"
  , ifreq    = [(CURIOUS_ITEM, 100), (BONDING_TOOL, 50)]
  , iflavour = zipPlain [White]
  , iverbHit = "soldier"
  , idesc    = "It is a bonding tool."  -- TODO: wikipedia
  }
duckTape = solderingIron
  { iname    = "duck tape"
  , ifreq    = [(CURIOUS_ITEM, 100), (BONDING_TOOL, 1)]
  , icount   = 1 `d` 4
  , irarity  = [(2, 15)]
  , iverbHit = "catch"
  , idesc    = "It is a bonding tool."  -- TODO: https://en.wikipedia.org/wiki/Duct_tape
  }

-- ** Periodic jewelry

-- ** Non-periodic jewelry

-- ** Armor

heavyBoot = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "spacesuit boot"
  , ifreq    = [ (COMMON_ITEM, 100), (S_SPACESUIT_BOOT, 1)
               , (SPACESUIT_PART, 2), (ARMOR_MISC, 1) ]
  , iflavour = zipFancy [Magenta]
  , icount   = 1
  , irarity  = [(1, 13), (3 * 10/15, 13), (4 * 10/15, 1)]
  , iverbHit = "sock"
  , iweight  = 100000  -- including the fake gravity mass
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 3
               , AddSkill SkHurtMelee (-20)
               , AddSkill SkArmorMelee $ (1 + 1 `dL` 3) * 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast  -- hope to replace with better soon
               , toVelocity 500 ]  -- the fake mass not counted for throwing
  , ieffects = []
  , idesc    = "An armored boot, cut-off from a spacesuit. The in-built micro-suction machinery for maintaining traction in the absence of gravity gives stability equivalent to an extra 100kg of mass. Kicks get abrupt acceleration millimeters short of the target."
  , ikit     = []
  }
spacesuit = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "spacesuit"
  , ifreq    = [(S_SPACESUIT, 1)]
  , iflavour = zipFancy [BrWhite]
  , icount   = 1
  , irarity  = [(10, 10)]
  , iverbHit = "hug"
  , iweight  = 250000  -- including the fake gravity mass from two boots
  , idamage  = 0
  , iaspects = [ AddSkill SkHurtMelee (-30)  -- easier when boots integrated
               , AddSkill SkSight (-1)
               , AddSkill SkHearing (-10), AddSkill SkSmell (-10)  -- worse now
               , AddSkill SkArmorMelee $ (6 + 1 `d` 3) * 20
               , AddSkill SkArmorRanged $ (4 + 1 `d` 3) * 12
                   -- twice better than from the set, because enemy
                   -- can't aim at the gaps between pieces
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
  , ieffects = []
  , idesc    = "A heavy spacesuit, with micro-suction machinery build into its boots, but requiring an external air tank for space walking."
  , ikit     = []
  }
spacesuitTorn = spacesuit
  { iname    = "torn spacesuit"
  , ifreq    = [(CURIOUS_ITEM, 100), (S_SPACESUIT_TORN, 1)]
  , irarity  = [(1, 15)]
  , iverbHit = "entangle"
  , iweight  = 10000
  , iaspects = [ AddSkill SkHurtMelee (-30)  -- easier when boots integrated
               , AddSkill SkSight (-1)  -- obstructed despite the tears
               , AddSkill SkArmorMelee $ (1 `d` 3) * 20
               , AddSkill SkArmorRanged $ (1 `d` 3) * 12
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
  , idesc    = "A badly torn spacesuit. Perhaps two decent wearable pieces could be salvaged by extracting, matching and patching components on a suitable workbench using scissors of some kind."
  }

-- ** Weapons

crowbar = chisel  -- no melee bonus, awkward to combine with other weapons
  { iname    = "crowbar"
  , ifreq    = (S_CROWBAR, 1) : ifreq chisel
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 6), (3 * 10/15, 6), (4 * 10/15, 1)]
  , iverbHit = "gouge"
  , idamage  = 2 `d` 1
  , iaspects = [ Timeout $ 3 + 1 `d` 2
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 30 ]
  , ieffects = [RefillHP (-3)]
                 -- @RefillHP@ to avoid a no-brainer of durable tool use;
                 -- (idamage ignored to avoid the exploit of tool use in armor)
  , idesc    = "This is a heavy and pointy piece of steel that can be employed as an improvised melee weapon. It is also usable as a breaching tool, though rather injurious."  -- TODO: https://en.wikipedia.org/wiki/Crowbar_(tool)
  }
catsPaw = chisel
  { iname    = "cat's paw"
  , iflavour = zipFancy [BrCyan]
  , irarity  = [(1, 12), (3 * 10/15, 12), (4 * 10/15, 1)]
  , iverbHit = "paw"
  , idamage  = 2 `d` 1
  , iaspects = [ Timeout $ 2 + 1 `d` 2
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 40 ]
  , ieffects = [RefillHP (-2)]
                 -- @RefillHP@ to avoid a no-brainer of durable tool use;
                 -- also quite attractive as a ranged weapon
  , idesc    = "This is a sturdy and pointy piece of steel that can be employed as an improvised melee weapon. It is also usable as a breaching tool, though not a particularly safe one."  -- TODO: https://en.wikipedia.org/wiki/Cat%27s_paw_(nail_puller)
  }
fireAxe = ItemKind
  { isymbol  = symbolHafted
  , iname    = "fire axe"
  , ifreq    = [ (COMMON_ITEM, 30), (HANDLE_AND_STEEL, 1), (S_FIRE_AXE, 1)
               , (FIRE_FIGHTING_ITEM, 10) ]
                 -- not @BREACHING_TOOL@, because it trigger traps
                 -- and destroys treasure, instead of opening
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "gouge"
  , iweight  = 1600
  , idamage  = 11 `d` 1  -- same as sharpened pipe, but upgradable
  , iaspects = [ Timeout 7, EqpSlot EqpSlotWeaponBig  -- 1m handle
               , SetFlag Durable, SetFlag Meleeable
               , toVelocity 40 ]  -- ensuring it hits with the blade costs speed
  , ieffects = []
  , idesc    = "An axe with a spike: once used for fire fighting, now turned to a bloodier purpose."
  , ikit     = []
  }
dagger = knife
  { iname    = "dagger"
  , ifreq    = [(COMMON_ITEM, 5), (S_DAGGER, 1), (STARTING_WEAPON, 5)]
  , iverbHit = "open"
  , irarity  = [(7, 20)]  -- like hammer, not knife, to prevent excess
  , idamage  = 7 `d` 1
  , idesc    = "A double-edged knife with a sharp tip that penetrates the smallest defence gaps, making it especially useful in conjunction with a larger but less nible weapon."
  }
hammer4 = hammer1  -- 1m handle, sharp
  { ifreq    = [ (COMMON_ITEM, 5), (HANDLE_AND_STEEL, 1)
               , (STARTING_WEAPON, 5), (S_SHARP_SHORT_HAMMER, 1) ]
  , iverbHit = "cleave"
  , ieffects = [RefillHP (-10)]
  , idesc    = "This hammer's head has it's protruding edges sharpened. Otherwise, it's pretty ordinary."
 }
hammer5 = hammer3  -- 2m pole, sharp
  { ifreq    = [ (COMMON_ITEM, 1), (POLE_AND_STEEL, 1)
               , (S_SHARP_LONG_HAMMER, 1) ]
  , iverbHit = "cleave"
  , ieffects = [RefillHP (-14)]
  , idesc    = "This long-hafter hammer sports a head with the edge of the narrow end sharpened for cutting."
  }
swordNullifySharp = swordNullify
  { iname    = "Roasting Rapier"
  , ifreq    = [(S_RAPIER_SHARP, 1)]
  , idamage  = 10 `d` 1
  , ieffects = [ DropItem 1 maxBound COrgan CONDITION
               , RefillCalm (-10)
               , Yell ]
  , idesc    = "A thin, acutely sharp steel blade that pierces deeply and sends its victim into abrupt, sobering shock. Originally, an exuberant hand-forged roasting implement, intentionally and wisely kept blunt."
  }
oxTongue = halberd  -- long pole, because glued 1m handle worse than nothing
  { iname    = "long spear"
  , ifreq    = [(COMMON_ITEM, 1), (POLE_AND_STEEL, 1), (S_LONG_SPEAR, 1)]
  , iverbHit = "impale"
  , idamage  = 13 `d` 1
  , idesc    = "An improvised but deadly weapon made of a long, sharp dagger glued and bound to a long pole. Not often one succeeds in making enough space to swing it freely, but even when stuck between terrain obstacles it blocks approaches effectively and makes using other weapons difficult, both by friends and foes."
  }
halberdPushActorSharp = halberdPushActor
  { iname    = "Swiss Halberd"
  , ifreq    = [(S_HALBERD_SHARP, 1)]
  , idamage  = 13 `d` 1
  , idesc    = "A perfect replica made for a reenactor troupe, hardened, sharpened. Versatile, with great reach and leverage. Foes are held at a distance."
  }
militaryKnife = knife
  { iname    = "military knife"
  , ifreq    = [ (COMMON_ITEM, 1), (WIRECUTTING_TOOL, 1), (MERCENARY_WEAPON, 70)
               , (STARTING_WEAPON, 3) ]
  , iflavour = zipFancy [Green]
  , irarity  = [(10, 10)]
  , iweight  = 500  -- too small to attach to a pole
  , idamage  = 7 `d` 1
  , iaspects = [ Timeout 2
               , AddSkill SkHurtMelee $ (-1 + 1 `d` 3 + 1 `dL` 2) * 3
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 60 ]  -- designed also for throwing
  , ieffects = [ RefillHP (-1)
                   -- @RefillHP@ to avoid a no-brainer of durable tool use
               , DropItem 1 maxBound COrgan CONDITION]
                   -- useful for AI who is the main user of this weapon
  , idesc    = "Millitary design laser-sharpened alloy blade able to cleanly open an artery at the lightest touch through layers of fabric. Despite its modest size, it defeats barbed wire in one slice."
  }
militaryBaton = ItemKind
  { isymbol  = symbolHafted
  , iname    = "military stun gun"
  , ifreq    = [(COMMON_ITEM, 2), (MERCENARY_WEAPON, 30)]
  , iflavour = zipFancy [Green]
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
  , iflavour = zipPlain [Brown]
  , irarity  = [(7, 12)]
  , idamage  = 2 `d` 1
  , ieffects = [Discharge $ 40 - 1 `d` 20, RefillCalm (-30)]
  , idesc    = "Used for subduing unruly zoo animals."
  }
gardenMsg :: Effect
gardenMsg = VerbMsgFail "feel the gardening tool fracture"
gardenDestruct :: Int -> GroupName ItemKind -> Effect
gardenDestruct n grp =
  OnUser $ OneOf $
    DestroyItem 1 1 CEqp grp
    `AndEffect`
    SeqEffect [ CreateItem Nothing CStash HANDLE timerNone
              , CreateItem Nothing CStash STEEL_SCRAP timerNone ]
    : replicate n gardenMsg
grassStitcher = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "grass stitcher"
  , ifreq    = [ (COMMON_ITEM, 100), (HANDLE_AND_STEEL, 1)
               , (GARDENING_TOOL, 1), (S_GRASS_STITCHER, 1) ]
  , iflavour = zipPlain [Green]
  , icount   = 1
  , irarity  = [(1, 10), (3 * 10/15, 10), (4 * 10/15, 1)]
      -- beyond level 3 they mostly appear with treePruner
  , iverbHit = "stab"
  , iweight  = 500
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout 3  -- light and can hit with any side
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponFast
               , toVelocity 30 ]
  , ieffects = [gardenDestruct 3 S_GRASS_STITCHER]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Grass_Stitcher
  , ikit     = [(GARDENING_TOOL, CGround), (GARDENING_TOOL, CGround)]
  }
ladiesFork = grassStitcher
  { iname    = "ladies' fork"
  , ifreq    = [ (COMMON_ITEM, 100), (HANDLE_AND_STEEL, 1)
               , (GARDENING_TOOL, 1), (S_LADIES_FORK, 1) ]
  , iflavour = zipFancy [Green]
  , iweight  = 1000
  , idamage  = 5 `d` 1
  , iaspects = [ Timeout 5
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]
  , ieffects = [gardenDestruct 5 S_LADIES_FORK]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Garden_fork
  , ikit     = [(GARDENING_TOOL, CGround)]
  }
hoe = grassStitcher
  { isymbol  = symbolHafted
  , iname    = "hoe"
  , ifreq    = [ (COMMON_ITEM, 100), (HANDLE_AND_STEEL, 1)
               , (GARDENING_TOOL, 1), (S_HOE, 1) ]
  , iflavour = zipFancy [Cyan]
  , iverbHit = "hack"
  , iweight  = 1000
  , idamage  = 6 `d` 1  -- neither sharp nor heavy
  , iaspects = [ Timeout 7
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 40 ]
  , ieffects = [gardenDestruct 7 S_HOE]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Hoe_(tool)
  , ikit     = [(GARDENING_TOOL, CGround)]
  }
spade = grassStitcher
  { isymbol  = symbolHafted  -- swinging much more deadly than gouging
  , iname    = "spade"
  , ifreq    = [ (COMMON_ITEM, 100), (HANDLE_AND_STEEL, 1)
               , (GARDENING_TOOL, 1), (S_SPADE, 1) ]
  , iflavour = zipPlain [Cyan]
  , iverbHit = "cut"
  , iweight  = 2000
  , idamage  = 7 `d` 1
  , iaspects = [ Timeout 9
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 50 ]
  , ieffects = [gardenDestruct 9 S_SPADE]
  , idesc    = ""  -- TODO: https://en.wikipedia.org/wiki/Spade
  , ikit     = []  -- most powerful, most likely to come alone
  }
treePruner = grassStitcher
  { iname    = "long reach tree pruner"
  , ifreq    = [(COMMON_ITEM, 100), (POLE_AND_STEEL, 1)]
  , iflavour = zipFancy [BrRed]
  , irarity  = [(8, 12)]
  , iweight  = 4500
  , idamage  = 4 `d` 1
  , iaspects = [ Timeout 10
               , AddSkill SkArmorMelee 20  -- sharp
               , SetFlag Durable, SetFlag Meleeable
               , EqpSlot EqpSlotWeaponBig
               , toVelocity 20 ]
  , ieffects = []
  , idesc    = "A heavy tree lopper on a sturdy long pole."
  , ikit     = [(GARDENING_TOOL, CGround)]
  }
cleaningPole = grassStitcher
  { iname    = "window cleaning pole"
  , ifreq    = [(COMMON_ITEM, 100), (POLE_AND_STEEL, 1)]
  , iflavour = zipPlain [Blue]
  , irarity  = [(8, 8)]
  , iweight  = 3500
  , idamage  = 1 `d` 1
  , iaspects = [ AddSkill SkArmorMelee 10  -- not sharp, so weaker
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 20 ]
  , ieffects = []
  , idesc    = "A cleaning contraption for glass surfaces, mounted on a long synthetic pole."
  , ikit     = []
  }
staff = grassStitcher
  { isymbol  = symbolHafted
  , iname    = "wooden staff"
  , ifreq    = [(HANDLE, 80), (POLE_OR_HANDLE, 55), (S_STAFF, 1)]
  , iflavour = zipPlain [Brown]
  , iverbHit = "prod"
  , iweight  = 1000
  , idamage  = 1 `d` 1
  , iaspects = [ SetFlag Durable
               , toVelocity 30 ]  -- a weak missile and that's all
  , ieffects = []
  , idesc    = ""  -- doh
  , ikit     = []
  }
pipe = staff
  { iname    = "metal pipe"
  , ifreq    = [(HANDLE, 20), (POLE_OR_HANDLE, 15), (S_PIPE, 1)]
  , iflavour = zipFancy [BrBlue]
  , idesc    = "Around a meter long, light, strong and hard alloy pipe. With one or both ends cut diagonally and sharpened, this would become a formidable weapon."
  }
longPole = staff
  { iname    = "long pole"
  , ifreq    = [(POLE, 90), (POLE_OR_HANDLE, 30)]
  , iflavour = zipPlain [BrYellow]
  , iweight  = 3000
  , iaspects = [ AddSkill SkArmorMelee 10  -- not sharp, so weaker
               , SetFlag Durable, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee
               , toVelocity 20 ]
  , idesc    = "Over two meters long, strong and light pole."
  }

-- ** Treasure

-- ** Usable robot body parts

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
               , AddSkill SkMaxCalm (-30)  -- punishes excessive stacking
               , SetFlag Periodic, SetFlag Equipable
               , EqpSlot EqpSlotArmorMelee ]
  , ieffects = [ Detect DetectLoot 20
               , Summon MOBILE_ANIMAL $ 1 `dL` 2
               , Explode S_WASTE ]
  , idesc    = "Waste recognition and utilization subsystem. Detects any stray item not registered as a passenger's cargo. Leaks a little."
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

-- ** Misc

ragTangle = sandstoneRock
  { isymbol  = symbolClothes
  , iname    = "tangle"
  , ifreq    = [(S_RAG_TANGLE, 1), (THICK_CLOTH, 1), (UNREPORTED_INVENTORY, 1)]
  , iflavour = zipPlain [Brown]
  , iverbHit = "touch"
  , iweight  = 200
  , idamage  = 0
  , iaspects = [ELabel "of rags", SetFlag Fragile]
  , idesc    = ""
  }
