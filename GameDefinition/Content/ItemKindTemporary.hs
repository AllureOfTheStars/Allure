-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Temporary aspect pseudo-item definitions.
module Content.ItemKindTemporary
  ( temporaries
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

temporaries :: [ItemKind]
temporaries =
  [tmpStrengthened, tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpNoctovision, tmpDrunk, tmpRegenerating, tmpWeaklyRegenerating, tmpPoisoned, tmpWeaklyPoisoned, tmpSlow10Resistant, tmpPoisonResistant, tmpImpressed]

tmpStrengthened,    tmpWeakened, tmpProtectedMelee, tmpProtectedRanged, tmpVulnerable, tmpResolute, tmpFast20, tmpSlow10, tmpFarSighted, tmpBlind, tmpKeenSmelling, tmpNoctovision, tmpDrunk, tmpRegenerating, tmpWeaklyRegenerating, tmpPoisoned, tmpWeaklyPoisoned, tmpSlow10Resistant, tmpPoisonResistant, tmpImpressed :: ItemKind

tmpNoLonger :: Text -> Effect
tmpNoLonger name = Temporary $ "be no longer" <+> name

-- The @name@ is be used in item description, so it should be an adjective
-- describing the temporary set of aspects.
tmpAs :: Text -> [Aspect] -> ItemKind
tmpAs name aspects = ItemKind
  { isymbol  = '+'
  , iname    = name
  , ifreq    = [(toGroupName name, 1), ("temporary condition", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "affect"
  , iweight  = 0
  , idamage  = toDmg 0
  , iaspects = -- timeout is 0; activates and vanishes soon,
               -- depending on initial timer setting
               aspects
  , ieffects = [ Periodic
               , Recharging $ tmpNoLonger name
               , OnSmash $ tmpNoLonger name ]
  , ifeature = [Identified, Fragile, Durable]  -- hack: destroy on drop
  , idesc    = ""
  , ikit     = []
  }

tmpStrengthened = tmpAs "strengthened" [AddHurtMelee 20]
tmpWeakened = tmpAs "weakened" [AddHurtMelee (-20)]
tmpProtectedMelee = tmpAs "protected from melee" [AddArmorMelee 50]
tmpProtectedRanged = tmpAs "protected from ranged" [AddArmorRanged 25]
tmpVulnerable = tmpAs "painted red" [ AddArmorMelee (-50)
                                    , AddArmorRanged (-25) ]
tmpResolute = tmpAs "resolute" [AddMaxCalm 60]
tmpFast20 = tmpAs "hasted" [AddSpeed 20]
tmpSlow10 = tmpAs "slowed" [AddSpeed (-10)]
tmpFarSighted = tmpAs "far-sighted" [AddSight 5]
tmpBlind = tmpAs "blind" [AddSight (-99)]
tmpKeenSmelling = tmpAs "keen-smelling" [AddSmell 2]
tmpNoctovision = tmpAs "shiny-eyed" [AddNocto 2]
tmpDrunk = tmpAs "drunk" [ AddHurtMelee 30  -- fury
                         , AddArmorMelee (-20)
                         , AddArmorRanged (-20)
                         , AddSight (-8)
                         ]
tmpRegenerating =
  let tmp = tmpAs "regenerating" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (RefillHP 1) : ieffects tmp
         }
tmpWeaklyRegenerating =
  let tmp = tmpAs "regenerating" []
  in tmp { ifreq = [("weakly regenerating", 1)]
         , icount = 3 + d 3
         , ieffects = Recharging (RefillHP 1) : ieffects tmp
         }
tmpPoisoned =
  let tmp = tmpAs "poisoned" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (RefillHP (-1)) : ieffects tmp
         }
tmpWeaklyPoisoned =
  let tmp = tmpAs "poisoned" []
  in tmp { ifreq = [("weakly poisoned", 1)]
         , icount = 3 + d 3
         , ieffects = Recharging (RefillHP (-1)) : ieffects tmp
         }
tmpSlow10Resistant =
  let tmp = tmpAs "slow resistant" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (DropItem 1 1 COrgan "slowed") : ieffects tmp
         }
tmpPoisonResistant =
  let tmp = tmpAs "poison resistant" []
  in tmp { icount = 7 + d 5
         , ieffects = Recharging (DropItem 1 maxBound COrgan "poisoned")
                      : ieffects tmp
         }
tmpImpressed =
  let tmp = tmpAs "impressed" []
  in tmp { isymbol = '!'
         , ifreq = [("impressed", 1)]  -- no "temporary condition"
         , ieffects = [OnSmash $ tmpNoLonger "impressed"]  -- not @Periodic@
         }
