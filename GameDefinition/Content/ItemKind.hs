-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Item and treasure definitions.
module Content.ItemKind ( cdefs ) where

import qualified Data.EnumMap.Strict as EM
import Data.List

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindOrgan
import Content.ItemKindTemporary
import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validateSingle = validateSingleItemKind
  , validateAll = validateAllItemKind
  , content = items ++ organs ++ blasts ++ actors ++ temporaries
  }

items :: [ItemKind]
items =
  [dart, dart200, paralizingProj, harpoon, net, needle, jumpingPole, sharpeningTool, seeingItem, light1, light2, light3, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, constructionHooter, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, currency]

dart,    dart200, paralizingProj, harpoon, net, needle, jumpingPole, sharpeningTool, seeingItem, light1, light2, light3, gorget, necklace1, necklace2, necklace3, necklace4, necklace5, necklace6, necklace7, necklace8, necklace9, sightSharpening, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, potion1, potion2, potion3, potion4, potion5, potion6, potion7, potion8, potion9, flask1, flask2, flask3, flask4, flask5, flask6, flask7, flask8, flask9, flask10, flask11, flask12, flask13, flask14, constructionHooter, scroll1, scroll2, scroll3, scroll4, scroll5, scroll6, scroll7, scroll8, scroll9, scroll10, scroll11, armorLeather, armorMail, gloveFencing, gloveGauntlet, gloveJousting, buckler, shield, dagger, daggerDropBestWeapon, hammer, hammerParalyze, hammerSpark, sword, swordImpress, swordNullify, halberd, halberdPushActor, wand1, wand2, gem1, gem2, gem3, gem4, currency :: ItemKind

necklace, ring, potion, flask, scroll, wand, gem :: ItemKind  -- generic templates

-- * Item group symbols, from Angband and variants

symbolProjectile, _symbolLauncher, symbolLight, symbolTool, symbolGem, symbolGold, symbolNecklace, symbolRing, symbolPotion, symbolFlask, symbolScroll, symbolTorsoArmor, symbolMiscArmor, _symbolClothes, symbolShield, symbolPolearm, symbolEdged, symbolHafted, symbolWand, _symbolStaff, _symbolFood :: Char

symbolProjectile = '{'
_symbolLauncher  = '}'
symbolLight      = '~'
symbolTool       = '~'
symbolGem        = '*'
symbolGold       = '$'
symbolNecklace   = '"'
symbolRing       = '='
symbolPotion     = '!'  -- concoction, bottle, jar, vial, canister
symbolFlask      = '!'
symbolScroll     = '?'  -- book, note, tablet, remote, chip, card
symbolTorsoArmor = '['
symbolMiscArmor  = ']'
_symbolClothes   = '('
symbolShield     = ')'
symbolPolearm    = '/'
symbolEdged      = '|'
symbolHafted     = '\\'
symbolWand       = '-'  -- magical rod, transmitter, pistol, rifle
_symbolStaff     = '_'  -- scanner
_symbolFood      = ','  -- too easy to miss?

-- * Thrown weapons

dart = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "steak knife"
  , ifreq    = [("useful", 100), ("any arrow", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 4 * d 3
  , irarity  = [(1, 30), (10, 10)]
  , iverbHit = "prick"
  , iweight  = 100
  , iaspects = [AddHurtRanged (d 6 + dl 6 |*| 10)]
  , ieffects = [Hurt (3 * d 1)]
  , ifeature = [toVelocity 75, Identified]  -- no fins, no special balance
  , idesc    = "Not particularly well balanced, but with a laser-sharpened titanium tip and blade."
  , ikit     = []
  }
dart200 = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "billiard ball"
  , ifreq    = [("useful", 100), ("any arrow", 50)]  -- TODO: until arrows added
  , iflavour = zipPlain [BrWhite]
  , icount   = 4 * d 3
  , irarity  = [(4, 20), (10, 10)]
  , iverbHit = "strike"
  , iweight  = 300
  , iaspects = [AddHurtRanged (d 6 + dl 6 |*| 10)]
  , ieffects = [Hurt (2 * d 1)]
  , ifeature = [toVelocity 150, Identified]
  , idesc    = "Ideal shape, size and weight for throwing."
  , ikit     = []
  }

-- * Exotic thrown weapons

paralizingProj = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "can"
  , ifreq    = [("useful", 100), ("can of sticky foam", 1)]
  , iflavour = zipPlain [Magenta]
  , icount   = dl 4
  , irarity  = [(5, 5), (10, 20)]
  , iverbHit = "glue"
  , iweight  = 1500
  , iaspects = []
  , ieffects = [ NoEffect "of sticky foam", Paralyze (5 + d 7)
               , OnSmash (Explode "glue")]
  , ifeature = [toVelocity 50, Identified, Fragile]  -- unwieldy
  , idesc    = "A can of liquid, fast-setting, construction foam."
  , ikit     = []
  }
harpoon = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "harpoon"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = dl 5
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "hook"
  , iweight  = 4000
  , iaspects = [AddHurtRanged (d 2 + 2 * dl 5 |*| 10)]
  , ieffects = [Hurt (4 * d 1), PullActor (ThrowMod 200 50)]
  , ifeature = []
  , idesc    = "A display piece harking back to the Earth's oceanic tourism hayday. The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  , ikit     = []
  }
net = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "net"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = dl 3
  , irarity  = [(3, 5), (10, 4)]
  , iverbHit = "entangle"
  , iweight  = 1000
  , iaspects = []
  , ieffects = [ toOrganGameTurn "slow 10" (3 + d 3)
               , DropItem CEqp "torso armor" False ]
  , ifeature = [Identified]
  , idesc    = "A large synthetic fibre net with weights affixed along the edges. Entangles armor and restricts movement."
  , ikit     = []
  }
needle = ItemKind
  { isymbol  = symbolProjectile
  , iname    = "needle"
  , ifreq    = [("needle", 1)]  -- special
  , iflavour = zipPlain [BrBlue]
  , icount   = 9 * d 3
  , irarity  = [(1, 1)]
  , iverbHit = "prick"
  , iweight  = 1
  , iaspects = [AddHurtRanged (d 3 + dl 3 |*| 10)]
  , ieffects = [Hurt (1 * d 1)]
  , ifeature = [toVelocity 200, Fragile]
  , idesc    = "The hypodermic needle part of a micro-syringe. Without the payload, it flies far and penetrates deeply, causing intense pain on movement."
  , ikit     = []
  }

-- * Assorted tools

jumpingPole = ItemKind
  { isymbol  = symbolTool
  , iname    = "jumping pole"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(1, 2), (10, 1)]
  , iverbHit = "prod"
  , iweight  = 10000
  , iaspects = [Timeout $ d 2 + 2 - dl 2 |*| 10]
  , ieffects = [Recharging (toOrganActorTurn "fast 20" 1)]
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
  , iaspects = [AddHurtMelee $ d 10 |*| 3]
  , ieffects = []
  , ifeature = [EqpSlot EqpSlotAddHurtMelee "", Identified]
  , idesc    = "Originally used for realigning the bent or buckled edges of kitchen knives in the local bars. Now it saves lives by letting you fix your weapons between or even during fights, without the need to set up camp, fish out tools and assemble a proper sharpening workshop."
  , ikit     = []
  }
seeingItem = ItemKind
  { isymbol  = '%'
  , iname    = "visual sensor"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(10, 1)]
  , iverbHit = "gaze at"
  , iweight  = 500
  , iaspects = [ AddSight 10, AddMaxCalm 60, AddLight 2
               , Periodic, Timeout $ 1 + d 2 ]
  , ieffects = [ Recharging (toOrganNone "poisoned")
               , Recharging (Summon [("robot", 1)] 1) ]
  , ifeature = [Identified]
  , idesc    = "A functioning visual sensor torn out from some giant robot. The circuitry is too big to serve just the basic signal processing. Watch out for the sharp edges and the seeping coolant liquid."
  , ikit     = []
  }
-- * Lights

light1 = ItemKind
  { isymbol  = symbolLight
  , iname    = "candle"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 8), (3, 6)]
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
light2 = ItemKind
  { isymbol  = symbolLight
  , iname    = "oil lamp"
  , ifreq    = [("useful", 100), ("light source", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 5), (10, 5)]
  , iverbHit = "burn"
  , iweight  = 1000
  , iaspects = [AddLight 3, AddSight (-1)]
  , ieffects = [Burn 3, Paralyze 3, OnSmash (Explode "burning oil 3")]
  , ifeature = [ toVelocity 70  -- hard not to spill the oil while throwing
               , Fragile, EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "A sizable glass lamp filled with plant oil feeding a wick."
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
  , iweight  = 2400
  , iaspects = [AddLight 4, AddArmorRanged $ - d 3]  -- noise and busy hands
  , ieffects = []
  , ifeature = [ EqpSlot EqpSlotAddLight "", Identified ]
  , idesc    = "Powerful, wide-beam spotlight, powered by a hand-crank. Requires noisy two-handed recharging every few minutes."
  , ikit     = []
  }

-- * Periodic jewelry

gorget = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "Old Gorget"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 1
  , irarity  = [(4, 3), (10, 3)]  -- weak, shallow
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [ Unique
               , Periodic
               , Timeout $ d 3 + 3 - dl 3 |*| 10
               , AddArmorMelee $ 2 + d 3
               , AddArmorRanged $ 2 + d 3 ]
  , ieffects = [Recharging (RefillCalm 1)]
  , ifeature = [ Durable, Precious, EqpSlot EqpSlotPeriodic ""
               , Identified, toVelocity 50 ]  -- not dense enough
  , idesc    = "Highly ornamental, cold, large, steel medallion on a chain. Unlikely to offer much protection as an armor piece, but the old, worn engraving reassures you."
  , ikit     = []
  }
necklace = ItemKind
  { isymbol  = symbolNecklace
  , iname    = "necklace"
  , ifreq    = [("useful", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain brightCol
  , icount   = 1
  , irarity  = [(10, 2)]
  , iverbHit = "whip"
  , iweight  = 30
  , iaspects = [Periodic]
  , ieffects = []
  , ifeature = [ Precious, EqpSlot EqpSlotPeriodic ""
               , toVelocity 50 ]  -- not dense enough
  , idesc    = "Tingling, rattling chain of flat encrusted links. Eccentric millionaires are known to hide their highly personalized body augmentation packs in such large jewelry pieces."
  , ikit     = []
  }
necklace1 = necklace
  { ifreq    = [("treasure", 100)]
  , iaspects = [Unique, Timeout $ d 3 + 4 - dl 3 |*| 10]
               ++ iaspects necklace
  , ieffects = [NoEffect "of Trickle Life", Recharging (RefillHP 1)]
  , ifeature = Durable : ifeature necklace
  }
necklace2 = necklace
  { ifreq    = [("treasure", 100)]  -- just too nasty to call it useful
  , irarity  = [(10, 1)]
  , iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 10) : iaspects necklace
  , ieffects = [ Recharging Impress
               , Recharging (DropItem COrgan "temporary conditions" True)
               , Recharging (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , Recharging (Explode "waste") ]
  }
necklace3 = necklace
  { iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 10) : iaspects necklace
  , ieffects = [Recharging (Paralyze $ 5 + d 5 + dl 5)]
  }
necklace4 = necklace
  { iaspects = (Timeout $ d 4 + 4 - dl 4 |*| 2) : iaspects necklace
  , ieffects = [Recharging (Teleport $ d 2 * 3)]
  }
necklace5 = necklace
  { iaspects = (Timeout $ d 3 + 4 - dl 3 |*| 10) : iaspects necklace
  , ieffects = [Recharging (Teleport $ 14 + d 3 * 3)]
  }
necklace6 = necklace
  { iaspects = (Timeout $ d 4 |*| 10) : iaspects necklace
  , ieffects = [Recharging (PushActor (ThrowMod 100 50))]
  }
necklace7 = necklace  -- TODO: teach AI to wear only for fight
  { ifreq    = [("treasure", 100)]
  , iaspects = [ Unique, AddMaxHP $ 5 + d 5
               , AddArmorMelee 20, AddArmorRanged 20
               , Timeout $ d 2 + 5 - dl 3 ]
               ++ iaspects necklace
  , ieffects = [ NoEffect "of Overdrive"
               , Recharging (InsertMove $ 1 + d 2)
               , Recharging (RefillHP (-1))
               , Recharging (RefillCalm (-1)) ]
  , ifeature = Durable : ifeature necklace
  }
necklace8 = necklace
  { iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 5) : iaspects necklace
  , ieffects = [Recharging $ Explode "spark"]
  }
necklace9 = necklace
  { iaspects = (Timeout $ d 3 + 3 - dl 3 |*| 5) : iaspects necklace
  , ieffects = [Recharging $ Explode "fragrance"]
  }

-- * Non-periodic jewelry

sightSharpening = ItemKind
  { isymbol  = symbolRing
  , iname    = "Autozoom Contact Lens"
  , ifreq    = [("treasure", 100)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , irarity  = [(7, 3), (10, 3)]  -- medium weak, medium shallow
  , iverbHit = "rap"
  , iweight  = 50
  , iaspects = [Unique, AddSight $ d 2, AddHurtMelee $ d 2 |*| 3]
  , ieffects = []
  , ifeature = [ Precious, Identified, Durable
               , EqpSlot EqpSlotAddSight "" ]
  , idesc    = "Zooms on any movement, distant or close. Requires some getting used to. Never needs to be taken off."
  , ikit     = []
  }
-- Don't add standard effects to rings, because they go in and out
-- of eqp and so activating them would require UI tedium: looking for
-- them in eqp and inv or even activating a wrong item via letter by mistake.
ring = ItemKind
  { isymbol  = symbolRing
  , iname    = "ring"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain stdCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(10, 3)]
  , iverbHit = "knock"
  , iweight  = 15
  , iaspects = []
  , ieffects = [Explode "blast 2"]
  , ifeature = [Precious, Identified]
  , idesc    = "A sturdy ring with a softly shining eye. If it contains a body booster unit, beware of the side-effects."
  , ikit     = []
  }
ring1 = ring
  { irarity  = [(10, 2)]
  , iaspects = [AddSpeed $ d 2, AddMaxHP $ dl 3 - 5 - d 3]
  , ieffects = [Explode "distortion"]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddSpeed ""]
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
ring4 = ring
  { irarity  = [(3, 6), (10, 6)]
  , iaspects = [AddHurtMelee $ d 5 + dl 5 |*| 3, AddMaxHP $ dl 3 - 4 - d 2]
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddHurtMelee ""]
  }
ring5 = ring  -- by the time it's found, probably no space in eqp
  { irarity  = [(5, 0), (10, 1)]
  , iaspects = [AddLight $ d 2]
  , ieffects = [Explode "distortion"]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot EqpSlotAddLight ""]
  , idesc    = "A sturdy ring with a large, shining stone."
  }
ring6 = ring
  { ifreq    = [("treasure", 100)]
  , irarity  = [(10, 2)]
  , iaspects = [ Unique, AddSpeed $ 3 + d 4
               , AddMaxCalm $ - 20 - d 20, AddMaxHP $ - 20 - d 20 ]
  , ieffects = [NoEffect "of Rush"]  -- no explosion, because Durable
  , ifeature = ifeature ring ++ [Durable, EqpSlot EqpSlotAddSpeed ""]
  }
ring7 = ring
  { ifreq    = [("useful", 100), ("ring of opportunity sniper", 1) ]
  , irarity  = [(10, 1)]
  , iaspects = [AddSkills $ EM.fromList [(AbProject, 9)]]
  , ieffects = [ NoEffect "of opportunity sniper"
               , Explode "distortion" ]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot (EqpSlotAddSkills AbProject) ""]
  }
ring8 = ring
  { ifreq    = [("useful", 100), ("ring of opportunity grenadier", 1) ]
  , irarity  = [(10, 1)]
  , iaspects = [AddSkills $ EM.fromList [(AbProject, 10)]]
  , ieffects = [ NoEffect "of opportunity grenadier"
               , Explode "distortion" ]  -- strong magic
  , ifeature = ifeature ring ++ [EqpSlot (EqpSlotAddSkills AbProject) ""]
  }

-- * Ordinary exploding consumables, often intended to be thrown

potion = ItemKind
  { isymbol  = symbolPotion
  , iname    = "vial"
  , ifreq    = [("useful", 100)]
  , iflavour = zipLiquid brightCol ++ zipPlain brightCol ++ zipFancy brightCol
  , icount   = 1
  , irarity  = [(1, 12), (10, 9)]
  , iverbHit = "splash"
  , iweight  = 200
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 50  -- oily, bad grip
               , Applicable, Fragile ]
  , idesc    = "A vial of bright, frothing concoction."  -- purely natural; no nano, no alien tech
  , ikit     = []
  }
potion1 = potion
  { ieffects = [ NoEffect "of rose water", Impress, RefillCalm (-3)
               , OnSmash ApplyPerfume, OnSmash (Explode "fragrance") ]
  }
potion2 = potion
  { ifreq    = [("treasure", 100)]
  , irarity  = [(6, 10), (10, 10)]
  , iaspects = [Unique]
  , ieffects = [ NoEffect "of Attraction", Impress, OverfillCalm (-20)
               , OnSmash (Explode "pheromone") ]
  }
potion3 = potion
  { irarity  = [(1, 5), (10, 5)]
  , ieffects = [RefillHP 5, OnSmash (Explode "healing mist")]
  }
potion4 = potion
  { irarity  = [(10, 5)]
  , ieffects = [RefillHP 10, OnSmash (Explode "healing mist 2")]
  }
potion5 = potion
  { ieffects = [ OneOf [ Impress, OverfillHP 10, Burn 5
                       , DropItem COrgan "poisoned" True ]
               , OnSmash (OneOf [ Explode "healing mist"
                                , Explode "wounding mist"
                                , Explode "fragrance"
                                , Explode "blast 10" ]) ]
  }
potion6 = potion
  { irarity  = [(3, 3), (10, 6)]
  , ieffects = [ Impress
               , OneOf [ OverfillCalm (-60)
                       , toOrganActorTurn "fast 20" (20 + d 5)
                       , OverfillHP 20, Burn 10
                       , DropItem COrgan "temporary conditions" True ]
               , OnSmash (OneOf [ Explode "healing mist 2"
                                , Explode "calming mist"
                                , Explode "distressing odor"
                                , Explode "distortion"  -- outlier, OK
                                , Explode "blast 20" ]) ]
  }
potion7 = potion
  { irarity  = [(1, 15), (10, 5)]
  , ieffects = [ DropItem COrgan "poisoned" True
               , OnSmash (Explode "antidote mist") ]
  }
potion8 = potion
  { irarity  = [(1, 5), (10, 15)]
  , ieffects = [ DropItem COrgan "temporary conditions" True
               , OnSmash (Explode "blast 10") ]
  }
potion9 = potion
  { ifreq    = [("treasure", 100)]
  , irarity  = [(10, 5)]
  , iaspects = [Unique]
  , ieffects = [ NoEffect "of Love", OverfillHP 60
               , Impress, OverfillCalm (-60)
               , OnSmash (Explode "healing mist 2")
               , OnSmash (Explode "pheromone") ]
  }

-- * Exploding consumables with temporary aspects, can be thrown
-- TODO: dip projectiles in those
-- TODO: add flavour and realism as in, e.g., "flask of whiskey",
-- which is more flavourful and believable than "flask of strength"

flask = ItemKind
  { isymbol  = symbolFlask
  , iname    = "flask"
  , ifreq    = [("useful", 100), ("flask", 100)]
  , iflavour = zipLiquid darkCol ++ zipPlain darkCol ++ zipFancy darkCol
  , icount   = 1
  , irarity  = [(1, 9), (10, 6)]
  , iverbHit = "splash"
  , iweight  = 500
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 50  -- oily, bad grip
               , Applicable, Fragile ]
  , idesc    = "A flask of oily liquid of a suspect color."
  , ikit     = []
  }
flask1 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of strength brew"
               , toOrganActorTurn "strengthened" (20 + d 5)
               , OnSmash (Explode "strength mist") ]
  }
flask2 = flask
  { ieffects = [ NoEffect "of weakness brew"
               , toOrganGameTurn "weakened" (20 + d 5)
               , OnSmash (Explode "weakness mist") ]
  }
flask3 = flask
  { ieffects = [ NoEffect "of protecting balm"
               , toOrganActorTurn "protected" (20 + d 5)
               , OnSmash (Explode "protecting balm") ]
  }
flask4 = flask
  { ieffects = [ NoEffect "of red paint"
               , toOrganGameTurn "painted red" (20 + d 5)
               , OnSmash (Explode "red paint") ]
  }
flask5 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of haste brew"
               , toOrganActorTurn "fast 20" (20 + d 5)
               , OnSmash (Explode "haste spray") ]
  }
flask6 = flask
  { ieffects = [ NoEffect "of lethargy brew"
               , toOrganGameTurn "slow 10" (20 + d 5)
               , toOrganNone "regenerating"
               , RefillCalm 3
               , OnSmash (Explode "slowness spray") ]
  }
flask7 = flask  -- sight can be reduced from Calm, drunk, etc.
  { irarity  = [(10, 7)]
  , ieffects = [ NoEffect "of eye drops"
               , toOrganActorTurn "far-sighted" (20 + d 5)
               , OnSmash (Explode "eye drop") ]
  }
flask8 = flask
  { irarity  = [(10, 3)]
  , ieffects = [ NoEffect "of smelly concoction"
               , toOrganActorTurn "keen-smelling" (20 + d 5)
               , OnSmash (Explode "smelly droplet") ]
  }
flask9 = flask
  { ieffects = [ NoEffect "of bait cocktail"
               , toOrganActorTurn "drunk" (5 + d 5)
               , Impress
               , OnSmash (Summon [("mobile animal", 1)] $ 1 + dl 2)
               , OnSmash (Explode "waste") ]
  }
flask10 = flask
  { ieffects = [ NoEffect "of whiskey"
               , toOrganActorTurn "drunk" (20 + d 5)
               , Impress, Burn 2, RefillHP 4
               , OnSmash (Explode "whiskey spray") ]
  }
flask11 = flask
  { irarity  = [(1, 20), (10, 6)]
  , ieffects = [ NoEffect "of regeneration brew"
               , toOrganNone "regenerating"
               , OnSmash (Explode "healing mist") ]
  }
flask12 = flask  -- but not flask of Calm depletion, since Calm reduced often
  { ieffects = [ NoEffect "of poison"
               , toOrganNone "poisoned"
               , OnSmash (Explode "wounding mist") ]
  }
flask13 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of slow resistance"
               , toOrganNone "slow resistant"
               , OnSmash (Explode "anti-slow mist") ]
  }
flask14 = flask
  { irarity  = [(10, 5)]
  , ieffects = [ NoEffect "of poison resistance"
               , toOrganNone "poison resistant"
               , OnSmash (Explode "antidote mist") ]
  }

-- * Non-exploding consumables, not specifically designed for throwing

constructionHooter = scroll
  { iname    = "construction hooter"
  , ifreq    = [("useful", 1), ("construction hooter", 1)]  -- extremely rare
  , iflavour = zipPlain [BrRed]
  , irarity  = [(1, 1)]
  , iaspects = []
  , ieffects = [Summon [("construction robot", 1)] $ 1 + dl 2]
  , ifeature = ifeature scroll ++ [Identified]
  , idesc    = "The single-use electronic overdrive hooter that construction robots use to warn about danger and call help in extreme emergency."
  , ikit     = []
  }
scroll = ItemKind
  { isymbol  = symbolScroll
  , iname    = "chip"
  , ifreq    = [("useful", 100), ("any scroll", 100)]
  , iflavour = zipFancy stdCol ++ zipPlain darkCol  -- arcane and old
  , icount   = 1
  , irarity  = [(1, 15), (10, 12)]
  , iverbHit = "thump"
  , iweight  = 20
  , iaspects = []
  , ieffects = []
  , ifeature = [ toVelocity 25  -- too little
               , Applicable ]
  , idesc    = "A generic, diposable chip, capable of a one-time holo-display. Some of these also contain a one-time password authorizing a particular spaceship's infrastructure transition. It is unknown how the infrastructure might respond after so many years."
  , ikit     = []
  }
scroll1 = scroll
  { ifreq    = [("treasure", 100)]
  , irarity  = [(5, 10), (10, 10)]  -- mixed blessing, so available early
  , iaspects = [Unique]
  , ieffects = [ NoEffect "of Reckless Beacon"
               , CallFriend 1, Summon standardSummon (2 + d 2) ]
  }
scroll2 = scroll
  { irarity  = []
  , ieffects = []
  }
scroll3 = scroll
  { irarity  = [(1, 5), (10, 3)]
  , ieffects = [Ascend 1]
  }
scroll4 = scroll
  { ieffects = [OneOf [ Teleport 5, RefillCalm 5, RefillCalm (-5)
                      , InsertMove 5, Paralyze 10 ]]
  }
scroll5 = scroll
  { irarity  = [(10, 15)]
  , ieffects = [ Impress
               , OneOf [ Teleport 20, Ascend (-1), Ascend 1
                       , Summon standardSummon 2, CallFriend 1
                       , RefillCalm 5, OverfillCalm (-60)
                       , CreateItem CGround "useful" TimerNone ] ]
  }
scroll6 = scroll
  { ieffects = [Teleport 5]
  }
scroll7 = scroll
  { ieffects = [Teleport 20]
  }
scroll8 = scroll
  { irarity  = [(10, 3)]
  , ieffects = [InsertMove $ 1 + d 2 + dl 2]
  }
scroll9 = scroll  -- TODO: remove Calm when server can tell if anything IDed
  { irarity  = [(1, 15), (10, 10)]
  , ieffects = [ NoEffect "of scientific explanation"
               , Identify, OverfillCalm 3 ]
  }
scroll10 = scroll  -- TODO: firecracker only if an item really polymorphed?
                   -- But currently server can't tell.
  { irarity  = [(10, 10)]
  , ieffects = [ NoEffect "molecular reconfiguration"
               , PolyItem, Explode "firecracker 7" ]
  }
scroll11 = scroll
  { ifreq    = [("treasure", 100)]
  , irarity  = [(6, 10), (10, 10)]
  , iaspects = [Unique]
  , ieffects = [NoEffect "of Prisoner Release", CallFriend 1]
  }

standardSummon :: Freqs ItemKind
standardSummon = [ ("alien", 20)
                 , ("mobile animal", 50)
                 , ("mobile robot", 30) ]

-- * Armor

armorLeather = ItemKind
  { isymbol  = symbolTorsoArmor
  , iname    = "spacesuit breastplate"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Brown]
  , icount   = 1
  , irarity  = [(1, 9), (10, 3)]
  , iverbHit = "thud"
  , iweight  = 7000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ 1 + d 2 + dl 2 |*| 5
               , AddArmorRanged $ 1 + d 2 + dl 2 |*| 5 ]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- unwieldy to throw and blunt
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "A hard-shell torso segment cut from a disposed off spacesuit."
  , ikit     = []
  }
armorMail = armorLeather
  { iname    = "bulletproof vest"
  , iflavour = zipPlain [Cyan]
  , irarity  = [(6, 9), (10, 3)]
  , iweight  = 12000
  , iaspects = [ AddHurtMelee (-3)
               , AddArmorMelee $ 2 + d 2 + dl 3 |*| 5
               , AddArmorRanged $ 2 + d 2 + dl 3 |*| 5 ]
  , idesc    = "A civilian bulletproof vest. Discourages foes from attacking your torso, making it harder for them to land a blow."
  }
gloveFencing = ItemKind
  { isymbol  = symbolMiscArmor
  , iname    = "construction glove"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(5, 9), (10, 9)]
  , iverbHit = "flap"
  , iweight  = 100
  , iaspects = [ AddHurtMelee $ (d 2 + dl 10) * 3
               , AddArmorRanged $ d 2 |*| 5 ]
  , ieffects = []
  , ifeature = [ toVelocity 30  -- flaps and flutters
               , Durable, EqpSlot EqpSlotAddArmorRanged "", Identified ]
  , idesc    = "A flexible construction glove from rough leather ensuring a good grip. Also, quite effective in deflecting or even catching slow projectiles."
  , ikit     = []
  }
gloveGauntlet = gloveFencing
  { iname    = "spacesuit glove"
  , iflavour = zipPlain [BrCyan]
  , irarity  = [(1, 9), (10, 3)]
  , iweight  = 300
  , iaspects = [ AddArmorMelee $ 1 + dl 2 |*| 5
               , AddArmorRanged $ 1 + dl 2 |*| 5 ]
  , idesc    = "A piece of a hull maintenance spacesuit, padded and reinforced with carbon fibre."
  }
gloveJousting = gloveFencing
  { iname    = "Welding Handgear"
  , iflavour = zipFancy [BrRed]
  , irarity  = [(1, 3), (10, 3)]
  , iweight  = 500
  , iaspects = [ Unique
               , AddHurtMelee $ dl 4 - 6 |*| 3
               , AddArmorMelee $ 2 + dl 2 |*| 5
               , AddArmorRanged $ 2 + dl 2 |*| 5 ]
  , idesc    = "Rigid, bulky handgear embedding a welding equipment, complete with an affixed small shield and a darkened visor. Awe-inspiring."
  }

-- * Shields

-- Shield doesn't protect against ranged attacks to prevent
-- micromanagement: walking with shield, melee without.
buckler = ItemKind
  { isymbol  = symbolShield
  , iname    = "buckler"
  , ifreq    = [("useful", 100)]
  , iflavour = zipPlain [Blue]
  , icount   = 1
  , irarity  = [(4, 6)]
  , iverbHit = "bash"
  , iweight  = 2000
  , iaspects = [ AddArmorMelee 40
               , AddHurtMelee (-30)
               , Timeout $ d 3 + 3 - dl 3 |*| 2 ]
  , ieffects = [ Hurt (1 * d 1)  -- to display xdy everywhre in Hurt
               , Recharging (PushActor (ThrowMod 200 50)) ]
  , ifeature = [ toVelocity 40  -- unwieldy to throw
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Heavy and unwieldy arm protection made from an outer airlock panel. Absorbs a percentage of melee damage, both dealt and sustained. Too small to intercept projectiles with."
  , ikit     = []
  }
shield = buckler
  { iname    = "shield"
  , irarity  = [(8, 3)]
  , iflavour = zipPlain [Green]
  , iweight  = 3000
  , iaspects = [ AddArmorMelee 80
               , AddHurtMelee (-70)
               , Timeout $ d 6 + 6 - dl 6 |*| 2 ]
  , ieffects = [Hurt (1 * d 1), Recharging (PushActor (ThrowMod 400 50))]
  , ifeature = [ toVelocity 30  -- unwieldy to throw
               , Durable, EqpSlot EqpSlotAddArmorMelee "", Identified ]
  , idesc    = "Large and unwieldy rectangle made of anti-meteorite ceramic sheet. Absorbs a percentage of melee damage, both dealt and sustained. Too heavy to intercept projectiles with."
  }

-- * Weapons

dagger = ItemKind
  { isymbol  = symbolEdged
  , iname    = "cleaver"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrCyan]
  , icount   = 1
  , irarity  = [(1, 18), (10, 4)]
  , iverbHit = "stab"
  , iweight  = 1000
  , iaspects = [AddHurtMelee $ d 3 + dl 3 |*| 3, AddArmorMelee $ d 2 |*| 5]
  , ieffects = [Hurt (6 * d 1)]
  , ifeature = [ toVelocity 40  -- ensuring it hits with the tip costs speed
               , Durable, Identified ]
  , idesc    = "A heavy professional kitchen blade. Will do fine cutting any kind of meat and bone, as well as parrying blows. Does not penetrate deeply, but is hard to block. Especially useful in conjunction with a larger weapon."
  , ikit     = []
  }
daggerDropBestWeapon = dagger
  { iname    = "Double Dagger"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(1, 2), (10, 4)]
  -- The timeout has to be small, so that the player can count on the effect
  -- occuring consistently in any longer fight. Otherwise, the effect will be
  -- absent in some important fights, leading to the feeling of bad luck,
  -- but will manifest sometimes in fights where it doesn't matter,
  -- leading to the feeling of wasted power.
  -- If the effect is very powerful and so the timeout has to be significant,
  -- let's make it really large, for the effect to occur only once in a fight:
  -- as soon as the item is equipped, or just on the first strike.
  , iaspects = [Unique, Timeout $ d 3 + 4 - dl 3 |*| 2]
  , ieffects = ieffects dagger ++ [Recharging DropBestWeapon]
  , idesc    = "A knife with a forked blade that a focused fencer can use to catch and twist an opponent's weapon occasionally."
  }
hammer = ItemKind
  { isymbol  = symbolHafted
  , iname    = "demolition hammer"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(3, 9), (4, 18), (10, 2)]
  , iverbHit = "club"
  , iweight  = 1500
  , iaspects = [AddHurtMelee $ d 2 + dl 2 |*| 3]
  , ieffects = [Hurt (8 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the sharp tip costs
               , Durable, Identified ]
  , idesc    = "A hammer on a long handle used for construction work. It may not cause grave wounds, but neither does it ricochet or glance off armor. Great sidearm for opportunistic blows against armored foes."
  , ikit     = []
  }
hammerParalyze = hammer
  { iname    = "Concussion Hammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(4, 2), (10, 4)]
  , iaspects = [Unique, Timeout $ d 2 + 3 - dl 2 |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Paralyze 5]
  }
hammerSpark = hammer
  { iname    = "Grand Smithhammer"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(4, 2), (10, 4)]
  , iaspects = [Unique, Timeout $ d 4 + 4 - dl 4 |*| 2]
  , ieffects = ieffects hammer ++ [Recharging $ Explode "spark"]
  }
sword = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "sharpened pipe"
  , ifreq    = [("useful", 100), ("starting weapon", 100)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(3, 1), (6, 16), (10, 8)]
  , iverbHit = "slash"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (10 * d 1)]
  , ifeature = [ toVelocity 20  -- ensuring it hits with the tip costs speed
               , Durable, Identified ]
  , idesc    = "A makeshift weapon of simple design, but great potential. Hard to master, though."
  , ikit     = []
  }
swordImpress = sword
  { isymbol  = symbolEdged
  , iname    = "Master's Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(3, 1), (10, 4)]
  , iaspects = [Unique, Timeout $ d 4 + 5 - dl 4 |*| 2]
  , ieffects = ieffects sword ++ [Recharging Impress]
  , idesc    = "An old, dull, but well-balance blade, lending itself to impressive shows of fencing skill."
  }
swordNullify = sword
  { isymbol  = symbolEdged
  , iname    = "Gutting Sword"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(5, 1), (10, 4)]
  , iaspects = [Unique, Timeout $ d 4 + 5 - dl 4 |*| 2]
  , ieffects = ieffects sword ++ [Recharging $ DropItem COrgan "temporary conditions" True]
  , idesc    = "Cold, thin, ancient blade that pierces deeply and sends its victim into abrupt, sobering shock."
  }
halberd = ItemKind
  { isymbol  = symbolPolearm
  , iname    = "pole cleaver"
  , ifreq    = [("useful", 100), ("starting weapon", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(7, 1), (10, 10)]
  , iverbHit = "impale"
  , iweight  = 3000
  , iaspects = [AddArmorMelee $ 1 + dl 3 |*| 5]
  , ieffects = [Hurt (12 * d 1)]
  , ifeature = [ toVelocity 20  -- not balanced
               , Durable, Identified ]
  , idesc    = "An improvised but deadly weapon made of a long, sharp kitchen knife glued and bound to a long pole."
  , ikit     = []
  }
halberdPushActor = halberd
  { iname    = "Swiss Halberd"
  , ifreq    = [("treasure", 20)]
  , irarity  = [(7, 1), (10, 4)]
  , iaspects = [Unique, Timeout $ d 5 + 5 - dl 5 |*| 2]
  , ieffects = ieffects halberd ++ [Recharging (PushActor (ThrowMod 400 25))]
  , idesc    = "A perfect replica made for a reenactor troupe, missing only some sharpening. Versatile, with great reach and leverage. Foes are held at a distance."
  }

-- * Wands

wand = ItemKind
  { isymbol  = symbolWand
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

-- * Treasure

gem = ItemKind
  { isymbol  = symbolGem
  , iname    = "gem"
  , ifreq    = [("treasure", 100), ("gem", 1)]
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 1
  , irarity  = []
  , iverbHit = "tap"
  , iweight  = 50
  , iaspects = [AddLight 1, AddSpeed (-1)]  -- reflects strongly, distracts
  , ieffects = []
  , ifeature = [Precious]
  , idesc    = "Precious, though useless. Worth around 100 gold grains."
  , ikit     = []
  }
gem1 = gem
  { irarity  = [(3 * 10/12, 0), (10, 10)]
  }
gem2 = gem
  { irarity  = [(5 * 10/12, 0), (10, 15)]
  }
gem3 = gem
  { irarity  = [(7 * 10/12, 0), (10, 20)]
  }
gem4 = gem
  { iname    = "stimpack"
  , iflavour = zipPlain [BrYellow]
  , irarity  = [(1, 10), (3 * 10/12, 0), (5 * 10/12, 20), (10, 20)]
  , iaspects = []
  , ieffects = [NoEffect "of youth", OverfillCalm 5, OverfillHP 15]
  , ifeature = [Identified, Precious]  -- TODO: only for humans
  , idesc    = "Calms, heals, invigorates and rejuvenates at the same time. No side-effects. As valuable as precious gems, at 100 gold grains each."
  }
currency = ItemKind
  { isymbol  = symbolGold
  , iname    = "gold grain"
  , ifreq    = [("treasure", 100), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 + d 20 + dl 20
  , irarity  = [(1, 10), (3 * 10/12, 0), (5, 25), (10, 10)]
  , iverbHit = "tap"
  , iweight  = 1
  , iaspects = []
  , ieffects = []
  , ifeature = [Identified, Precious]
  , idesc    = "Reliably valuable in every civilized place."
  , ikit     = []
  }
