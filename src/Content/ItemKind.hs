{-# LANGUAGE OverloadedStrings #-}
-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Weapons and treasure for Allure of the Stars.
module Content.ItemKind ( cdefs ) where

import qualified Data.List as L

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = ivalidate
  , content =
      [necklace, dart, gem1, gem2, gem3, currency, javelin, kitchenKnife, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand1, wand2, fist, foot, tentacle]
  }
necklace,        dart, gem1, gem2, gem3, currency, javelin, kitchenKnife, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand1, wand2, fist, foot, tentacle :: ItemKind

gem, potion, scroll, wand :: ItemKind  -- generic templates

-- castDeep (aDb, xDy) = castDice aDb + lvl * castDice xDy / depth

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

necklace = ItemKind
  { isymbol  = '"'
  , iname    = "necklace"
  , ifreq    = [("dng", 6)]
  , iflavour = zipFancy [BrGreen]
  , ieffect  = Regeneration (rollDice 2 3, rollDice 1 10)
  , icount   = intToDeep 1
  , iverbApply   = "tear down"
  , iverbProject = "cast"
  , iweight  = 30
  , itoThrow = -50  -- not dense enough
  }
dart = ItemKind
  { isymbol  = '{'
  , iname    = "billiard ball"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [BrWhite]
  , ieffect  = Hurt (rollDice 1 1) (rollDice 1 2, rollDice 1 2)
  , icount   = (rollDice 3 3, rollDice 0 0)
  , iverbApply   = "splinter"
  , iverbProject = "hurl"
  , iweight  = 50
  , itoThrow = 0  -- a cheap dart
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "precious gem"
  , ifreq    = [("dng", 20)]       -- x3, but rare on shallow levels
  , iflavour = zipPlain $ L.delete BrYellow brightCol  -- natural, so not fancy
  , ieffect  = NoEffect
  , icount   = intToDeep 0
  , iverbApply   = "crush"
  , iverbProject = "toss"
  , iweight  = 50
  , itoThrow = 0
  }
gem1 = gem
  { icount   = (rollDice 0 0, rollDice 1 1)  -- appears on max depth
  }
gem2 = gem
  { icount   = (rollDice 0 0, rollDice 1 2)  -- appears halfway, doubled on max
  }
gem3 = gem
  { icount   = (rollDice 0 0, rollDice 1 3)
  }
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold grain"
  , ifreq    = [("dng", 50), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , ieffect  = NoEffect
  , icount   = (rollDice 0 0, rollDice 10 10)
  , iverbApply   = "smear"
  , iverbProject = "blow away"
  , iweight  = 1
  , itoThrow = 0
  }
javelin = ItemKind
  { isymbol  = '{'
  , iname    = "javelin"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [Brown]
  , ieffect  = Hurt (rollDice 1 2) (rollDice 1 2, rollDice 2 2)
  , icount   = (rollDice 0 0, rollDice 1 1)
  , iverbApply   = "break up"
  , iverbProject = "hurl"
  , iweight  = 2000
  , itoThrow = 0  -- cheap but deadly
  }
kitchenKnife = ItemKind
  { isymbol  = '{'
  , iname    = "kitchen knife"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [BrCyan]
  , ieffect  = Hurt (rollDice 1 1) (rollDice 0 0, rollDice 1 2)
  , icount   = (rollDice 0 0, rollDice 1 2)
  , iverbApply   = "bend"
  , iverbProject = "throw"
  , iweight  = 200
  , itoThrow = -50  -- too flexible and the handle causes air resistance
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "vial"
  , ifreq    = [("dng", 15)]
  , iflavour = zipFancy stdCol
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  , iweight  = 200
  , itoThrow = -50  -- oily, bad grip
  }
potion1 = potion
  { ifreq    = [("dng", 5)]
  , ieffect  = ApplyPerfume
  }
potion2 = potion
  { ieffect  = Heal 5
  }
potion3 = potion
  { ifreq    = [("dng", 5)]
  , ieffect  = Heal (-5)
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = []  -- [("dng", 10)]  -- TODO: make it useful
  , iflavour = zipPlain [White]
  , ieffect  = Searching (rollDice 1 6, rollDice 3 2)
  , icount   = intToDeep 1
  , iverbApply   = "squeeze down"
  , iverbProject = "toss"
  , iweight  = 15
  , itoThrow = 0
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "tablet"
  , ifreq    = [("dng", 4)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , iverbApply   = "dial"
  , iverbProject = "lob"
  , iweight  = 700
  , itoThrow = -25  -- bad grip
  }
scroll1 = scroll
  { ieffect  = CallFriend 1
  }
scroll2 = scroll
  { ieffect  = Summon 1
  }
scroll3 = scroll
  { ieffect  = Descend 1
  }
sword = ItemKind
  { isymbol  = '/'
  , iname    = "sharpened pipe"
  , ifreq    = [("dng", 40)]
  , iflavour = zipPlain [Cyan]
  , ieffect  = Hurt (rollDice 3 1) (rollDice 1 2, rollDice 4 2)
  , icount   = intToDeep 1
  , iverbApply   = "hit"
  , iverbProject = "heave"
  , iweight  = 3000
  , itoThrow = -25  -- bad grip
  }
wand = ItemKind
  { isymbol  = '-'
  , iname    = "injector"
  , ifreq    = [("dng", 15)]
  , iflavour = zipFancy brightCol
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , iverbApply   = "snap"
  , iverbProject = "zap"
  , iweight  = 300
  , itoThrow = 25  -- jet
  }
wand1 = wand
  { ieffect  = Dominate
  }
wand2 = wand
  { ifreq    = [("dng", 3)]
  , ieffect  = Heal (-25)
  }
fist = sword
  { isymbol  = '@'
  , iname    = "fist"
  , ifreq    = [("hth", 1), ("unarmed", 100)]
  , ieffect  = Hurt (rollDice 3 1) (intToDeep 0)
  , iverbApply   = "punch"
  , iverbProject = "ERROR, please report: iverbProject fist"
  }
foot = sword
  { isymbol  = '@'
  , iname    = "foot"
  , ifreq    = [("hth", 1), ("unarmed", 50)]
  , ieffect  = Hurt (rollDice 3 1) (intToDeep 0)
  , iverbApply   = "kick"
  , iverbProject = "ERROR, please report: iverbProject foot"
  }
tentacle = sword
  { isymbol  = 'S'
  , iname    = "tentacle"
  , ifreq    = [("hth", 1), ("monstrous", 100)]
  , ieffect  = Hurt (rollDice 3 1) (intToDeep 0)
  , iverbApply   = "hit"
  , iverbProject = "ERROR, please report: iverbProject tentacle"
  }
