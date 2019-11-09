-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Blast definitions.
module Content.ItemKindBlast
  ( -- * Group name patterns
    pattern FIRECRACKER, pattern VIOLENT_FRAGMENTATION, pattern FRAGMENTATION, pattern FOCUSED_FRAGMENTATION, pattern VIOLENT_CONCUSSION, pattern CONCUSSION, pattern FOCUSED_CONCUSSION, pattern VIOLENT_FLASH, pattern SPARK, pattern FOCUSED_fLASH, pattern GLASS_HAIL, pattern FOCUSED_GLASS_HAIL, pattern PHEROMONE, pattern CALMING_MIST, pattern DISTRESSING_ODOR, pattern HEALING_MIST, pattern HEALING_MIST_2, pattern WOUNDING_MIST, pattern DISTORTION, pattern SMOKE, pattern BOILING_WATER, pattern GLUE, pattern WASTE, pattern ANTI_SLOW_MIST, pattern ANTIDOTE_MIST, pattern SLEEP_MIST, pattern DENSE_SHOWER, pattern SPARSE_SHOWER, pattern MELEE_PROTECTIVE_BALM, pattern RANGE_PROTECTIVE_BALM, pattern DEFENSELESSNESS_RUNOUT, pattern RESOLUTION_DUST, pattern HASTE_SPRAY, pattern SLOWNESS_MIST, pattern EYE_DROP, pattern IRON_FILING, pattern SMELLY_DROPLET, pattern EYE_SHINE, pattern WHISKEY_SPRAY, pattern YOUTH_SPRINKLE, pattern POISON_CLOUD, pattern ARMOR_MISC, pattern BURNING_OIL_2, pattern BURNING_OIL_3, pattern BURNING_OIL_4
  , blastNoStatOf, blastBonusStatOf
  , pattern NITROGEN_MIST, pattern PAINT_DROPLET, pattern RHINO_HOLOGRAM, pattern ADVERTISEMENT, pattern STORY_TELLING
  , pattern FIRE_SOURCE, pattern OIL_SOURCE, pattern WATER_SOURCE, pattern COLD_SOURCE
  , -- * Content
    blasts
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import Content.ItemKindTemporary
import Game.LambdaHack.Content.ItemKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Ability
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.Flavour

-- * Group name patterns

pattern FIRECRACKER, VIOLENT_FRAGMENTATION, FRAGMENTATION, FOCUSED_FRAGMENTATION, VIOLENT_CONCUSSION, CONCUSSION, FOCUSED_CONCUSSION, VIOLENT_FLASH, SPARK, FOCUSED_fLASH, GLASS_HAIL, FOCUSED_GLASS_HAIL, PHEROMONE, CALMING_MIST, DISTRESSING_ODOR, HEALING_MIST, HEALING_MIST_2, WOUNDING_MIST, DISTORTION, SMOKE, BOILING_WATER, GLUE, WASTE, ANTI_SLOW_MIST, ANTIDOTE_MIST, SLEEP_MIST, DENSE_SHOWER, SPARSE_SHOWER, MELEE_PROTECTIVE_BALM, RANGE_PROTECTIVE_BALM, DEFENSELESSNESS_RUNOUT, RESOLUTION_DUST, HASTE_SPRAY, SLOWNESS_MIST, EYE_DROP, IRON_FILING, SMELLY_DROPLET, EYE_SHINE, WHISKEY_SPRAY, YOUTH_SPRINKLE, POISON_CLOUD, ARMOR_MISC, BURNING_OIL_2, BURNING_OIL_3, BURNING_OIL_4 :: GroupName ItemKind

pattern NITROGEN_MIST, PAINT_DROPLET, RHINO_HOLOGRAM, ADVERTISEMENT, STORY_TELLING :: GroupName ItemKind

pattern FIRE_SOURCE, OIL_SOURCE, WATER_SOURCE, COLD_SOURCE :: GroupName ItemKind

pattern FIRECRACKER = GroupName "firecracker"
pattern VIOLENT_FRAGMENTATION = GroupName "violent fragmentation"
pattern FRAGMENTATION = GroupName "fragmentation"
pattern FOCUSED_FRAGMENTATION = GroupName "focused fragmentation"
pattern VIOLENT_CONCUSSION = GroupName "violent concussion"
pattern CONCUSSION = GroupName "concussion"
pattern FOCUSED_CONCUSSION = GroupName "focused concussion"
pattern VIOLENT_FLASH = GroupName "violent flash"
pattern SPARK = GroupName "spark"
pattern FOCUSED_fLASH = GroupName "focused flash"
pattern GLASS_HAIL = GroupName "glass hail"
pattern FOCUSED_GLASS_HAIL = GroupName "focused glass hail"
pattern PHEROMONE = GroupName "pheromone"
pattern CALMING_MIST = GroupName "calming mist"
pattern DISTRESSING_ODOR = GroupName "distressing odor"
pattern HEALING_MIST = GroupName "healing mist"
pattern HEALING_MIST_2 = GroupName "healing mist 2"
pattern WOUNDING_MIST = GroupName "wounding mist"
pattern DISTORTION = GroupName "distortion"
pattern SMOKE = GroupName "smoke"
pattern BOILING_WATER = GroupName "boiling water"
pattern GLUE = GroupName "glue"
pattern WASTE = GroupName "waste"
pattern ANTI_SLOW_MIST = GroupName "anti-slow mist"
pattern ANTIDOTE_MIST = GroupName "antidote mist"
pattern SLEEP_MIST = GroupName "sleep mist"
pattern DENSE_SHOWER = GroupName "dense shower"
pattern SPARSE_SHOWER = GroupName "sparse shower"
pattern MELEE_PROTECTIVE_BALM = GroupName "melee protective balm"
pattern RANGE_PROTECTIVE_BALM = GroupName "ranged protective balm"
pattern DEFENSELESSNESS_RUNOUT = GroupName "acid spray"
pattern RESOLUTION_DUST = GroupName "resolution dust"
pattern HASTE_SPRAY = GroupName "haste spray"
pattern SLOWNESS_MIST = GroupName "slowness mist"
pattern EYE_DROP = GroupName "eye drop"
pattern IRON_FILING = GroupName "iron filing"
pattern SMELLY_DROPLET = GroupName "smelly droplet"
pattern EYE_SHINE = GroupName "eye shine"
pattern WHISKEY_SPRAY = GroupName "whiskey spray"
pattern YOUTH_SPRINKLE = GroupName "youth sprinkle"
pattern POISON_CLOUD = GroupName "poison cloud"
pattern ARMOR_MISC = GroupName "armor misc"
pattern BURNING_OIL_2 = GroupName "burning oil 2"
pattern BURNING_OIL_3 = GroupName "burning oil 3"
pattern BURNING_OIL_4 = GroupName "burning oil 4"

firecrackerAt :: Int -> GroupName ItemKind
firecrackerAt n = GroupName $ "firecracker" <+> tshow n

blastNoStatOf :: GroupName ItemKind -> GroupName ItemKind
blastNoStatOf grp = GroupName $ fromGroupName grp <+> "mist"

blastBonusStatOf :: GroupName ItemKind -> GroupName ItemKind
blastBonusStatOf grp = GroupName $ fromGroupName grp <+> "dew"

-- ** Allure-specific
pattern NITROGEN_MIST = GroupName "nitrogen mist"
pattern PAINT_DROPLET = GroupName "paint droplet"
pattern RHINO_HOLOGRAM = GroupName "rhino hologram"
pattern ADVERTISEMENT = GroupName "advertisement"
pattern STORY_TELLING = GroupName "story-telling"

pattern FIRE_SOURCE = GroupName "fire source"
pattern OIL_SOURCE = GroupName "oil source"
pattern WATER_SOURCE = GroupName "water source"
pattern COLD_SOURCE = GroupName "cold source"

-- * Content

blasts :: [ItemKind]
blasts =
  [burningOil2, burningOil3, burningOil4, firecracker1, firecracker2, firecracker3, firecracker4, firecracker5, spreadFragmentation, spreadFragmentation8, focusedFragmentation, spreadConcussion, spreadConcussion8, focusedConcussion, spreadFlash, spreadFlash8, focusedFlash, singleSpark, glassPiece, focusedGlass, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, smoke, boilingWater, glue, waste, mistAntiSlow, mistAntidote, mistSleep, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, defenselessnessRunout, resolutionDust, hasteSpray, slownessMist, eyeDrop, ironFiling, smellyDroplet, eyeShine, whiskeySpray, youthSprinkle, poisonCloud, blastNoSkMove, blastNoSkMelee, blastNoSkDisplace, blastNoSkAlter, blastNoSkWait, blastNoSkMoveItem, blastNoSkProject, blastNoSkApply, blastBonusSkMove, blastBonusSkMelee, blastBonusSkDisplace, blastBonusSkAlter, blastBonusSkWait, blastBonusSkMoveItem, blastBonusSkProject, blastBonusSkApply]
  -- Allure-specific
  ++ [cruiseAdHologram, outerAdHologram, victoriaClassHologram, allureIntroHologram, nitrogenMist, paintSpray]

burningOil2,    burningOil3, burningOil4, firecracker1, firecracker2, firecracker3, firecracker4, firecracker5, spreadFragmentation, spreadFragmentation8, focusedFragmentation, spreadConcussion, spreadConcussion8, focusedConcussion, spreadFlash, spreadFlash8, focusedFlash, singleSpark, glassPiece, focusedGlass, fragrance, pheromone, mistCalming, odorDistressing, mistHealing, mistHealing2, mistWounding, distortion, smoke, boilingWater, glue, waste, mistAntiSlow, mistAntidote, mistSleep, denseShower, sparseShower, protectingBalmMelee, protectingBalmRanged, defenselessnessRunout, resolutionDust, hasteSpray, slownessMist, eyeDrop, ironFiling, smellyDroplet, eyeShine, whiskeySpray, youthSprinkle, poisonCloud, blastNoSkMove, blastNoSkMelee, blastNoSkDisplace, blastNoSkAlter, blastNoSkWait, blastNoSkMoveItem, blastNoSkProject, blastNoSkApply, blastBonusSkMove, blastBonusSkMelee, blastBonusSkDisplace, blastBonusSkAlter, blastBonusSkWait, blastBonusSkMoveItem, blastBonusSkProject, blastBonusSkApply :: ItemKind
-- Allure-specific
cruiseAdHologram,       outerAdHologram, victoriaClassHologram, allureIntroHologram, nitrogenMist, paintSpray :: ItemKind

-- We take care (e.g., in burningOil below) that blasts are not faster
-- than 100% fastest natural speed, or some frames would be skipped,
-- which is a waste of perfectly good frames.

-- * Parameterized blasts

burningOil :: Int -> GroupName ItemKind -> ItemKind
burningOil n grp = ItemKind
  { isymbol  = '*'
  , iname    = "burning oil"
  , ifreq    = [(grp, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = intToDice (4 + n * 4)
  , irarity  = [(1, 1)]
  , iverbHit = "sear"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity (min 100 $ n `div` 2 * 10)
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 2 ]
  , ieffects = [ Burn 1
               , toOrganBad PACIFIED (1 `d` 2) ]
                   -- slips and frantically puts out fire
  , idesc    = "Sticky oil, burning brightly."
  , ikit     = []
  }
burningOil2 = burningOil 2 BURNING_OIL_2  -- 2 steps, 2 turns
burningOil3 = burningOil 3 BURNING_OIL_3  -- 3 steps, 2 turns
burningOil4 = burningOil 4 BURNING_OIL_4  -- 4 steps, 2 turns
firecracker :: Int -> ItemKind
firecracker n = ItemKind
  { isymbol  = '*'
  , iname    = "firecracker"
  , ifreq    = [(if n == 5
                 then FIRECRACKER
                 else firecrackerAt n, 1)]
  , iflavour = zipPlain [brightCol !! ((n + 2) `mod` length brightCol)]
  , icount   = if n <= 3 then 1 `d` min 2 n else 2 + 1 `d` 2
  , irarity  = [(1, 1)]
  , iverbHit = if n >= 4 then "singe" else "crack"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine $ intToDice $ 1 + n `div` 2 ]
  , ieffects = [if n >= 4 then Burn 1 else RefillCalm (-2)]
               ++ [DropBestWeapon | n >= 4]
               ++ [OnSmash $ Explode $ firecrackerAt (n - 1) | n >= 2]
  , idesc    = "Scraps of burnt paper, covering little pockets of black powder, buffeted by colorful explosions."
  , ikit     = []
  }
firecracker5 = firecracker 5
firecracker4 = firecracker 4
firecracker3 = firecracker 3
firecracker2 = firecracker 2
firecracker1 = firecracker 1

-- * Focused blasts

spreadFragmentation = ItemKind
  { isymbol  = '*'
  , iname    = "fragmentation burst"
  , ifreq    = [(VIOLENT_FRAGMENTATION, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 16  -- strong but few, so not always hits target
  , irarity  = [(1, 1)]
  , iverbHit = "tear apart"
  , iweight  = 1
  , idamage  = 3 `d` 1  -- deadly and adjacent actor hit by 2 on average;
                        -- however, moderate armour blocks completely
  , iaspects = [ ToThrow $ ThrowMod 100 20 4  -- 4 steps, 1 turn
               , SetFlag Lobable, SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 3, AddSkill SkHurtMelee $ -12 * 5 ]
  , ieffects = [DropItem 1 1 COrgan CONDITION]
  , idesc    = "Flying shards, flame and smoke."
  , ikit     = []
  }
spreadFragmentation8 = spreadFragmentation
  { iname    = "fragmentation burst"
  , ifreq    = [(FRAGMENTATION, 1)]
  , icount   = 8
  , iaspects = [ ToThrow $ ThrowMod 100 10 2  -- 2 steps, 1 turn
               , SetFlag Lobable, SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 3, AddSkill SkHurtMelee $ -12 * 5 ]
      -- smaller radius, so worse for area effect, but twice the direct damage
  }
focusedFragmentation = ItemKind
  { isymbol  = '`'
  , iname    = "deflagration ignition"  -- improvised fertilizer, etc.
  , ifreq    = [(FOCUSED_FRAGMENTATION, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 4  -- 32 in total vs 16; on average 4 hits
  , irarity  = [(1, 1)]
  , iverbHit = "ignite"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 0  -- 0 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
      -- when the target position is occupied, the explosion starts one step
      -- away, hence we set range to 0 steps, to limit dispersal
  , ieffects = [OnSmash $ Explode FRAGMENTATION]
  , idesc    = idesc spreadFragmentation
  , ikit     = []
  }
spreadConcussion = ItemKind
  { isymbol  = '*'
  , iname    = "concussion blast"
  , ifreq    = [(VIOLENT_CONCUSSION, 1)]
  , iflavour = zipPlain [Magenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "shock"
  , iweight  = 1
  , idamage  = 1 `d` 1  -- only air pressure, so not as deadly as fragmentation,
                        -- but armour can't block completely that easily
  , iaspects = [ ToThrow $ ThrowMod 100 20 4  -- 4 steps, 1 turn
               , SetFlag Lobable, SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 3, AddSkill SkHurtMelee $ -8 * 5 ]
      -- outdoors it has short range, but we only model indoors in the game;
      -- it's much faster than black powder shock wave, but we are beyond
      -- human-noticeable speed differences on short distances anyway
  , ieffects = [ DropItem maxBound 1 CEqp ARMOR_MISC
               , PushActor (ThrowMod 400 25 1)  -- 1 step, fast; after DropItem
                   -- this produces spam for braced actors; too bad
               , toOrganBad IMMOBILE 3  -- no balance
               , toOrganBad DEAFENED 23 ]
  , idesc    = "Shock wave, hot gases, some fire and smoke."
  , ikit     = []
  }
spreadConcussion8 = spreadConcussion
  { iname    = "concussion blast"
  , ifreq    = [(CONCUSSION, 1)]
  , icount   = 8
  , iaspects = [ ToThrow $ ThrowMod 100 10 2  -- 2 steps, 1 turn
               , SetFlag Lobable, SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 3, AddSkill SkHurtMelee $ -8 * 5 ]
  }
focusedConcussion = ItemKind
  { isymbol  = '`'
  , iname    = "detonation ignition"  -- stabilized high explosive liquid
  , ifreq    = [(FOCUSED_CONCUSSION, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 4
  , irarity  = [(1, 1)]
  , iverbHit = "ignite"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 0  -- 0 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [OnSmash $ Explode CONCUSSION]
  , idesc    = idesc spreadConcussion
  , ikit     = []
  }
spreadFlash = ItemKind
  { isymbol  = '`'
  , iname    = "magnesium flash"  -- or aluminum, but let's stick to one
  , ifreq    = [(VIOLENT_FLASH, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "dazzle"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ ToThrow $ ThrowMod 100 20 4  -- 4 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 5 ]
  , ieffects = [toOrganBad BLIND 5, toOrganBad WEAKENED 20]
                 -- Wikipedia says: blind for five seconds and afterimage
                 -- for much longer, harming aim
  , idesc    = "A very bright flash of fire."
  , ikit     = []
  }
spreadFlash8 = spreadFlash
  { iname    = "spark"
  , ifreq    = [(SPARK, 1)]
  , icount   = 8
  , iverbHit = "singe"
  , iaspects = [ ToThrow $ ThrowMod 100 10 2  -- 2 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 5 ]
  }
focusedFlash = ItemKind
  { isymbol  = '`'
  , iname    = "magnesium ignition"
  , ifreq    = [(FOCUSED_fLASH, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 4
  , irarity  = [(1, 1)]
  , iverbHit = "ignite"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 0  -- 0 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [OnSmash $ Explode SPARK]
  , idesc    = idesc spreadFlash
  , ikit     = []
  }
singleSpark = spreadFlash
  { iname    = "single spark"
  , ifreq    = [(SINGLE_SPARK, 1)]
  , icount   = 1
  , iverbHit = "spark"
  , iaspects = [ toLinger 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 3 ]
  , ieffects = []
  , idesc    = "A glowing ember."
  , ikit     = []
  }
glassPiece = ItemKind
  { isymbol  = '*'
  , iname    = "glass piece"
  , ifreq    = [(GLASS_HAIL, 1)]
  , iflavour = zipPlain [Blue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "cut"
  , iweight  = 1
  , idamage  = 2 `d` 1
  , iaspects = [ ToThrow $ ThrowMod 100 20 4  -- 4 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkHurtMelee $ -15 * 5 ]
                 -- brittle, not too dense; armor blocks
  , ieffects = []
  , idesc    = "Swift, sharp edges."
  , ikit     = []
  }
focusedGlass = glassPiece  -- when blowing up windows
  { ifreq    = [(FOCUSED_GLASS_HAIL, 1)]
  , icount   = 4
  , iaspects = [ toLinger 0  -- 0 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkHurtMelee $ -15 * 5 ]
  , ieffects = [OnSmash $ Explode GLASS_HAIL]
  }

-- * Assorted blasts don't induce conditions or not mainly so

fragrance = ItemKind
  { isymbol  = '`'
  , iname    = "fragrance"  -- instant, fast fragrance
  , ifreq    = [(FRAGRANCE, 1)]
  , iflavour = zipPlain [Magenta]
  , icount   = 12
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 10  -- 2 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [Impress, toOrganGood ROSE_SMELLING 45]
  -- Linger 10, because sometimes it takes 2 turns due to starting just
  -- before actor turn's end (e.g., via a necklace).
  , idesc    = "A pleasant scent."
  , ikit     = []
  }
pheromone = ItemKind
  { isymbol  = '`'
  , iname    = "musky whiff"  -- a kind of mist rather than fragrance
  , ifreq    = [(PHEROMONE, 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "tempt"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 10  -- 2 steps, 2 turns
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [Dominate]
  , idesc    = "A sharp, strong scent."
  , ikit     = []
  }
mistCalming = ItemKind  -- unused
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [(CALMING_MIST, 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [RefillCalm 2]
  , idesc    = "A soothing, gentle cloud."
  , ikit     = []
  }
odorDistressing = ItemKind
  { isymbol  = '`'
  , iname    = "distressing whiff"
  , ifreq    = [(DISTRESSING_ODOR, 1)]
  , iflavour = zipFancy [BrRed]  -- salmon
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "distress"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 10  -- 2 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [ RefillCalm (-10)
               , toOrganBad FOUL_SMELLING (20 + 1 `d` 5)
               , toOrganBad IMPATIENT (2 + 1 `d` 2) ]
  , idesc    = "It turns the stomach."  -- and so can't stand still
  , ikit     = []
  }
mistHealing = ItemKind
  { isymbol  = '`'
  , iname    = "mist"  -- powerful, so slow and narrow
  , ifreq    = [(HEALING_MIST, 1)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 1 ]
  , ieffects = [RefillHP 2]
  , idesc    = "It fills the air with light and life. And lots of organic chemicals."
  , ikit     = []
  }
mistHealing2 = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [(HEALING_MIST_2, 1)]
  , iflavour = zipPlain [Green]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "revitalize"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast
               , AddSkill SkShine 2 ]
  , ieffects = [RefillHP 4]
  , idesc    = "At its touch, wounds close and bruises fade. Not the most frugal way to apply nanobots, though."
  , ikit     = []
  }
mistWounding = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [(WOUNDING_MIST, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "devitalize"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [RefillHP (-2)]
  , idesc    = "The air itself stings and itches."
  , ikit     = []
  }
distortion = ItemKind
  { isymbol  = 'v'
  , iname    = "vortex"
  , ifreq    = [(DISTORTION, 1)]
  , iflavour = zipPlain [White]
  , icount   = 8  -- braced are immune to Teleport; avoid failure messages
  , irarity  = [(1, 1)]
  , iverbHit = "engulf"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 10  -- 2 steps, 1 turn
               , SetFlag Lobable, SetFlag Fragile, SetFlag Blast ]
  , ieffects = [Teleport $ 15 + 1 `d` 10]
  , idesc    = "The air shifts oddly, as though light is being warped."
  , ikit     = []
  }
smoke = ItemKind  -- when stuff burns out  -- unused
  { isymbol  = '`'
  , iname    = "smoke fume"  -- pluralizes better than 'smokes'
  , ifreq    = [(SMOKE, 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "choke"  -- or "obscure"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 20  -- 4 steps, 2 turns
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [toOrganBad WITHHOLDING (5 + 1 `d` 3)]
                  -- choking and tears, can roughly see, but not aim
  , idesc    = "Twirling clouds of grey smoke."
  , ikit     = []
  }
boilingWater = ItemKind
  { isymbol  = '*'
  , iname    = "boiling water"
  , ifreq    = [(BOILING_WATER, 1)]
  , iflavour = zipPlain [White]
  , icount   = 18
  , irarity  = [(1, 1)]
  , iverbHit = "boil"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 30  -- 6 steps, 2 turns
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [Burn 1]
  , idesc    = "It bubbles and hisses."
  , ikit     = []
  }
glue = ItemKind
  { isymbol  = '*'
  , iname    = "glue droplet"
  , ifreq    = [(GLUE, 1)]
  , iflavour = zipPlain [Cyan]
  , icount   = 8  -- Paralyze doesn't stack; avoid failure messages
  , irarity  = [(1, 1)]
  , iverbHit = "glue"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 20  -- 4 steps, 2 turns
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [Paralyze 10]
  , idesc    = "Thick and clinging."
  , ikit     = []
  }
waste = ItemKind
  { isymbol  = '*'
  , iname    = "waste piece"
  , ifreq    = [(WASTE, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "splosh"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [ toOrganBad FOUL_SMELLING (30 + 1 `d` 10)
               , toOrganBad DISPOSSESSED (10 + 1 `d` 5) ]
  , idesc    = "Sodden and foul-smelling."
  , ikit     = []
  }
mistAntiSlow = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [(ANTI_SLOW_MIST, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "propel"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [DropItem 1 1 COrgan SLOWED]
  , idesc    = "A cleansing rain."
  , ikit     = []
  }
mistAntidote = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [(ANTIDOTE_MIST, 1)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "cure"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [DropItem 1 maxBound COrgan POISONED]
  , idesc    = "Washes away death's dew."
  , ikit     = []
  }
mistSleep = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [(SLEEP_MIST, 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "put to sleep"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [PutToSleep]
  , idesc    = "Lulls weary warriors."
  , ikit     = []
  }

-- * Condition-inducing blasts

-- Almost all have @toLinger 10@, that travels 2 steps in 1 turn.
-- These are very fast projectiles, not getting into the way of big
-- actors and not burdening the engine for long.
-- A few are slower 'mists'.

denseShower = ItemKind
  { isymbol  = '`'
  , iname    = "dense shower"
  , ifreq    = [(DENSE_SHOWER, 1)]
  , iflavour = zipFancy [Green]
  , icount   = 12
  , irarity  = [(1, 1)]
  , iverbHit = "strengthen"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood STRENGTHENED 5]
  , idesc    = "A thick rain of droplets."
  , ikit     = []
  }
sparseShower = ItemKind
  { isymbol  = '`'
  , iname    = "sparse shower"
  , ifreq    = [(SPARSE_SHOWER, 1)]
  , iflavour = zipFancy [Red]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "weaken"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganBad WEAKENED 7]
  , idesc    = "Light droplets that cling to clothing."
  , ikit     = []
  }
protectingBalmMelee = ItemKind
  { isymbol  = '`'
  , iname    = "balm droplet"
  , ifreq    = [(MELEE_PROTECTIVE_BALM, 1)]
  , iflavour = zipFancy [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood PROTECTED_FROM_MELEE (3 + 1 `d` 3)]
  , idesc    = "A thick ointment that hardens the skin."
  , ikit     = []
  }
protectingBalmRanged = ItemKind
  { isymbol  = '`'
  , iname    = "balm droplet"
  , ifreq    = [(RANGE_PROTECTIVE_BALM, 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "balm"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood PROTECTED_FROM_RANGED (3 + 1 `d` 3)]
  , idesc    = "Grease that protects from flying death."
  , ikit     = []
  }
defenselessnessRunout = ItemKind
  { isymbol  = '?'
  , iname    = "acid spray"
  , ifreq    = [(DEFENSELESSNESS_RUNOUT, 1)]
  , iflavour = zipFancy [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "dissolve"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganBad DEFENSELESS (3 + 1 `d` 3)]
  , idesc    = "A searing fluid that sticks to the skin."
  , ikit     = []
  }
resolutionDust = ItemKind
  { isymbol  = '`'
  , iname    = "resolution dust"
  , ifreq    = [(RESOLUTION_DUST, 1)]
  , iflavour = zipPlain [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "calm"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood RESOLUTE (3 + 1 `d` 3)]
                 -- short enough duration that @calmEnough@ not a big problem
  , idesc    = "A handful of honest earth, to strengthen the soul."
  , ikit     = []
  }
hasteSpray = ItemKind
  { isymbol  = '`'
  , iname    = "haste spray"
  , ifreq    = [(HASTE_SPRAY, 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "haste"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood HASTED (3 + 1 `d` 3)]
  , idesc    = "A quick spurt."
  , ikit     = []
  }
slownessMist = ItemKind
  { isymbol  = '`'
  , iname    = "slowness mist"
  , ifreq    = [(SLOWNESS_MIST, 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "slow"
  , iweight  = 0
  , idamage  = 0
  , iaspects = [toVelocity 5, SetFlag Fragile, SetFlag Blast]
                 -- 1 step, 1 turn, mist, slow
  , ieffects = [toOrganBad SLOWED (3 + 1 `d` 3)]
  , idesc    = "Clammy fog, making each movement an effort."
  , ikit     = []
  }
eyeDrop = ItemKind
  { isymbol  = '`'
  , iname    = "eye drop"
  , ifreq    = [(EYE_DROP, 1)]
  , iflavour = zipFancy [BrCyan]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "cleanse"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood FAR_SIGHTED (3 + 1 `d` 3)]
  , idesc    = "Not to be taken orally."
  , ikit     = []
  }
ironFiling = ItemKind
  { isymbol  = '`'
  , iname    = "iron filing"
  , ifreq    = [(IRON_FILING, 1)]
  , iflavour = zipPlain [Red]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "blind"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganBad BLIND (10 + 1 `d` 10)]
  , idesc    = "A shaving of bright metal."
  , ikit     = []
  }
smellyDroplet = ItemKind
  { isymbol  = '`'
  , iname    = "smelly droplet"
  , ifreq    = [(SMELLY_DROPLET, 1)]
  , iflavour = zipFancy [Blue]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sensitize"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood KEEN_SMELLING (5 + 1 `d` 3)]
  , idesc    = "A viscous lump that stains the skin."
  , ikit     = []
  }
eyeShine = ItemKind
  { isymbol  = '`'
  , iname    = "eye shine"
  , ifreq    = [(EYE_SHINE, 1)]
  , iflavour = zipFancy [Cyan]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "smear"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood SHINY_EYED (3 + 1 `d` 3)]
  , idesc    = "They almost glow in the dark."
  , ikit     = []
  }
whiskeySpray = ItemKind
  { isymbol  = '`'
  , iname    = "whiskey spray"
  , ifreq    = [(WHISKEY_SPRAY, 1)]
  , iflavour = zipFancy [Brown]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "inebriate"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganGood DRUNK (3 + 1 `d` 3)]
  , idesc    = "It burns in the best way."
  , ikit     = []
  }
youthSprinkle = ItemKind
  { isymbol  = '`'
  , iname    = "youth sprinkle"
  , ifreq    = [(YOUTH_SPRINKLE, 1)]
  , iflavour = zipFancy [BrGreen]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "sprinkle"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [ toOrganGood ROSE_SMELLING (40 + 1 `d` 20)
               , toOrganNoTimer REGENERATING ]
  , idesc    = "Bright and smelling of the Spring."
  , ikit     = []
  }
poisonCloud = ItemKind
  { isymbol  = '`'
  , iname    = "poison cloud"
  , ifreq    = [(POISON_CLOUD, 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "poison"
  , iweight  = 0
  , idamage  = 0
  , iaspects = [ ToThrow $ ThrowMod 10 100 2  -- 2 steps, 2 turns
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [toOrganNoTimer POISONED]
  , idesc    = "Choking gas that stings the eyes."
  , ikit     = []
  }
blastNoStat :: GroupName ItemKind -> ItemKind
blastNoStat grp = ItemKind
  { isymbol  = '`'
  , iname    = "mist"
  , ifreq    = [(blastNoStatOf grp, 1)]
  , iflavour = zipFancy [White]
  , icount   = 12
  , irarity  = [(1, 1)]
  , iverbHit = "drain"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 10  -- 2 steps, 2 turns
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [toOrganBad grp (3 + 1 `d` 3)]
  , idesc    = "Completely disables one personal faculty."
  , ikit     = []
  }
blastNoSkMove = blastNoStat IMMOBILE
blastNoSkMelee = blastNoStat PACIFIED
blastNoSkDisplace = blastNoStat IRREPLACEABLE
blastNoSkAlter = blastNoStat RETAINING
blastNoSkWait = blastNoStat IMPATIENT
blastNoSkMoveItem = blastNoStat DISPOSSESSED
blastNoSkProject = blastNoStat WITHHOLDING
blastNoSkApply = blastNoStat PARSIMONIOUS
blastBonusStat :: GroupName ItemKind -> ItemKind
blastBonusStat grp = ItemKind
  { isymbol  = '`'
  , iname    = "dew"
  , ifreq    = [(blastBonusStatOf grp, 1)]
  , iflavour = zipFancy [White]
  , icount   = 12
  , irarity  = [(1, 1)]
  , iverbHit = "elevate"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toVelocity 10  -- 2 steps, 2 turns
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [toOrganGood grp (20 + 1 `d` 5)]
  , idesc    = "Temporarily enhances the given personal faculty."
  , ikit     = []
  }
blastBonusSkMove = blastBonusStat MORE_MOBILE
blastBonusSkMelee = blastBonusStat MORE_COMBATIVE
blastBonusSkDisplace = blastBonusStat MORE_DISPLACING
blastBonusSkAlter = blastBonusStat MORE_MODIFYING
blastBonusSkWait = blastBonusStat MORE_PATIENT
blastBonusSkMoveItem = blastBonusStat MORE_TIDY
blastBonusSkProject = blastBonusStat MORE_PROJECTING
blastBonusSkApply = blastBonusStat MORE_PRACTICAL

-- * Allure-specific

-- ** Lore blasts

-- They exist for a short time only, but the lore can be read
-- from the lore menu. Only optional story bits should go there,
-- because some players may not even notice them (at first, at least).
-- This is designed not to spam gameplay with story. Gameplay first.
-- Generally, 3 to 5 blasts of each kind should suffice for variety.
-- More would induce excessive repetition of some to see all
-- (they are shown at random). With mild exceptions, they should have
-- no effects.

cruiseAdHologram = ItemKind
  { isymbol  = '`'
  , iname    = "cruise ad hologram"
  , ifreq    = [(RHINO_HOLOGRAM, 1), (ADVERTISEMENT, 10)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "excite"
  , iweight  = 0  -- delay of 1 turn at the start, to easily read the text
  , idamage  = 0
  , iaspects = [ toVelocity 5  -- 1 step, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [toOrganGood RESOLUTE (5 + 1 `d` 2), DropBestWeapon]
  , idesc    = "The fitful holographic clip shows a couple that laughs, watches in silence Saturn's rings through a huge window, throws treats to a little rhino frolicking in reduced gravity, runs through corridors wearing alien masks in a mock chase. An exited female voice proclaims: \"...safety, security and comfort...for each of your senses...personalized life support zones...robot servants...guessing your every wish...\""
  , ikit     = []
  }
outerAdHologram = cruiseAdHologram
  { iname    = "cruise ad hologram"
  , ifreq    = [(ADVERTISEMENT, 20)]
  , icount   = 4
  , ieffects = []  -- weak, 4 particles, no effect
  , idesc    = "A composed young man in a hat looks straight into your eyes with unwavering stare and extols the opportunities, freedom and excitement of the outer Solar System frontier life with unshakable conviction. Names of Neptune-area realtors scroll at the bottom in small font with oversize serifs."
  }
victoriaClassHologram = outerAdHologram
  { iname    = "space fleet hologram"
  , ifreq    = [(STORY_TELLING, 20)]
  , iflavour = zipFancy [BrBlue]
  , icount   = 1
  , iverbHit = "bore"
  , idesc    = "A series of huge spaceships zoom in and out of view in a solemn procession. Male voice drones over crackling static: Victoria-class cruise liners are the largest passenger ships ever serially manufactured and the third largest in general, including transport vessel series. Bigger ships are sometimes cobbled ad-hoc, by wiring together cheap modules and primitive cargo hulls welded in space, but they are rarely certified for public commercial operation. Victoria-class passenger cruisers are produced for over three decades now, in slowly evolving configurations, one every two years on average. The design is as conservative, as possible. A disc large enough for comfortable artificial gravity through constant spinning. Fusion reactor in the middle of the axle powering engines protruding far back from the rear plane. Meteor shield at the front. Numerous redundant rechargeable power sources and autonomous life support areas within several independently pressurized slices of the disc, eliminating the \"all locked in a single can, breathing the same air\" space travel grievance. Actually, everything is redundant twice over, due to strict regulations. To sum it up, these are the most boring spaceships in the galaxy."
  }
allureIntroHologram = victoriaClassHologram
  { iname    = "spaceship hologram"
  , ifreq    = [(STORY_TELLING, 10)]
  , idesc    = "A wavy 3D wireframe of a spaceship rotates ponderously. Male voice drones: Allure of the Stars belongs to a long line of luxurious orbit-to-orbit cruise liners, the Victoria-class. The ship is named after the largest passenger sea vessel of the early 21st century, with which it shares the grandeur and extravagance. This particular Victoria-class specimen has been designed for long cruises to gas giants, their moons and the moon cities (with their notorious saloons and night life). It has a meteor shield in the form of a flat, multi-layer. unpressurized cargo bay covering the front plane. Such extra cargo capacity enables long space journeys with no limits on resource usage. On shorter legs of the journeys it also enables opportunistic mass cargo transport (in accordance to strictest regulations and completely isolated from the airflow on passenger decks), which is always in demand at the profusely productive, but scarcely populated Solar System frontier. It also makes the unit much thicker than usual: the length from the tip of the cargo bay to the engines' exhausts is almost two thirds of the diameter of the disk. All in all, it is a particularly sturdy and self-sufficient member of a class famed for exceptional resilience and safety."
  }

-- ** Misc

nitrogenMist = ItemKind
  { isymbol  = '`'
  , iname    = "nitrogen mist"
  , ifreq    = [(NITROGEN_MIST, 1), (COLD_SOURCE, 1)]
  , iflavour = zipFancy [BrBlack]
  , icount   = 8
  , irarity  = [(1, 1)]
  , iverbHit = "freeze"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [ toLinger 0  -- 0 steps, 1 turn
               , SetFlag Fragile, SetFlag Blast ]
  , ieffects = [toOrganBad SLOWED (2 + 1 `d` 3)]
  , idesc    = "Colourless and colder than ice."
  , ikit     = []
  }
paintSpray = ItemKind
  { isymbol  = '`'
  , iname    = "fluorescent paint"
  , ifreq    = [(PAINT_DROPLET, 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 16
  , irarity  = [(1, 1)]
  , iverbHit = "paint"
  , iweight  = 1
  , idamage  = 0
  , iaspects = [toLinger 10, SetFlag Fragile, SetFlag Blast]
  , ieffects = [toOrganBad PAINTED (3 + 1 `d` 3)]
  , idesc    = "Softly glowing red paint that marks a target."
  , ikit     = []
  }
