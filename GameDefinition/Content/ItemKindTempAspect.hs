-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Temporary aspect pseudo-item definitions.
module Content.ItemKindTempAspect ( tempAspects ) where

import Data.Text (Text)

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.ItemKind

tempAspects :: [ItemKind]
tempAspects =
  [tmpFast20, tmpDrunk]

tmpFast20,    tmpDrunk :: ItemKind

-- The @name@ is be used in item description, so it should be an adjetive
-- describing the temporary set of aspects.
tmpAs :: Text -> [Aspect Dice] -> ItemKind
tmpAs name aspects = ItemKind
  { isymbol  = '.'
  , iname    = name
  , ifreq    = [(toGroupName name, 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "affect"
  , iweight  = 1
  , iaspects = [Periodic, Timeout 0]  -- activates and vanishes soon,
                                      -- depending on initial timer setting
               ++ aspects
  , ieffects = [Recharging (Temporary $ "be no longer" <+> name)]
  , ifeature = [Identified]
  , idesc    = ""
  , ikit     = []
  }

tmpFast20 = tmpAs "fast 20" [AddSpeed 30]
tmpDrunk = tmpAs "drunk" [ AddHurtMelee 30  -- fury
                         , AddArmorMelee (-20)
                         , AddArmorRanged (-20)
                         , AddSight (-7)
                         ]
