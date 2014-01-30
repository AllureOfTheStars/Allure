-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
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
import Game.LambdaHack.Common.ItemFeature
import Game.LambdaHack.Common.Random
import Game.LambdaHack.Content.ItemKind

cdefs :: ContentDef ItemKind
cdefs = ContentDef
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = validateItemKind
  , content =
      [necklace, dart, gem1, gem2, gem3, currency, javelin, kitchenKnife, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand1, wand2, fist, foot, tentacle, fragrance, mist_healing, mist_wounding, glass_piece, smoke]
  }
necklace,        dart, gem1, gem2, gem3, currency, javelin, kitchenKnife, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand1, wand2, fist, foot, tentacle, fragrance, mist_healing, mist_wounding, glass_piece, smoke :: ItemKind

gem, potion, scroll, wand :: ItemKind  -- generic templates

-- castDeep (aDb, xDy) = castDice aDb + (lvl - 1) * castDice xDy / (depth - 1)

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
  , ifreq    = [("useful", 6)]
  , iflavour = zipFancy [BrGreen]
  , icount   = intToDeep 1
  , iverbApply   = "tear down"
  , iverbProject = "cast"
  , iweight  = 30
  , itoThrow = -50  -- not dense enough
  , ifeature = [Cause $ Regeneration (rollDeep (2, 3) (1, 10))]
  }
dart = ItemKind
  { isymbol  = '{'
  , iname    = "billiard ball"
  , ifreq    = [("useful", 20)]
  , iflavour = zipPlain [BrWhite]
  , icount   = rollDeep (3, 3) (0, 0)
  , iverbApply   = "splinter"
  , iverbProject = "hurl"
  , iweight  = 50
  , itoThrow = 0  -- a cheap dart
  , ifeature = [Cause $ Hurt (rollDice 1 1) (rollDeep (1, 2) (1, 2))]
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "precious gem"
  , ifreq    = [("treasure", 20)]  -- x3, but rare on shallow levels
  , iflavour = zipPlain $ L.delete BrYellow brightCol  -- natural, so not fancy
  , icount   = intToDeep 0
  , iverbApply   = "crush"
  , iverbProject = "toss"
  , iweight  = 50
  , itoThrow = 0
  , ifeature = []
  }
gem1 = gem
  { icount   = rollDeep (0, 0) (1, 1)  -- appears on max depth
  }
gem2 = gem
  { icount   = rollDeep (0, 0) (1, 2)  -- appears halfway
  }
gem3 = gem
  { icount   = rollDeep (0, 0) (1, 3)  -- appears early
  }
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold grain"
  , ifreq    = [("treasure", 20), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = rollDeep (0, 0) (10, 10)  -- appears on lvl 2
  , iverbApply   = "smear"
  , iverbProject = "blow away"
  , iweight  = 1
  , itoThrow = 0
  , ifeature = []
  }
javelin = ItemKind
  { isymbol  = '{'
  , iname    = "javelin"
  , ifreq    = [("useful", 30)]
  , iflavour = zipPlain [Brown]
  , icount   = rollDeep (0, 0) (1, 1)
  , iverbApply   = "break up"
  , iverbProject = "hurl"
  , iweight  = 2000
  , itoThrow = 0  -- cheap but deadly
  , ifeature = [Cause $ Hurt (rollDice 1 2) (rollDeep (1, 2) (2, 2))]
  }
kitchenKnife = ItemKind
  { isymbol  = '{'
  , iname    = "kitchen knife"
  , ifreq    = [("useful", 25)]
  , iflavour = zipPlain [BrCyan]
  , icount   = rollDeep (0, 0) (1, 2)
  , iverbApply   = "bend"
  , iverbProject = "throw"
  , iweight  = 200
  , itoThrow = -50  -- too flexible and the handle causes air resistance
  , ifeature = [Cause $ Hurt (rollDice 1 1) (rollDeep (0, 0) (1, 2))]
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "vial"
  , ifreq    = [("useful", 15)]
  , iflavour = zipFancy stdCol
  , icount   = intToDeep 1
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  , iweight  = 200
  , itoThrow = -50  -- oily, bad grip
  , ifeature = []
  }
potion1 = potion
  { ifreq    = [("useful", 5)]
  , ifeature = [Cause ApplyPerfume, Explode "fragrance"]
  }
potion2 = potion
  { ifeature = [Cause $ Heal 5, Explode "mist healing"]
  }
potion3 = potion
  { ifreq    = [("useful", 5)]
  , ifeature = [Cause $ Heal (-5), Explode "mist wounding"]
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = []  -- [("useful", 10)]  -- TODO: make it useful
  , iflavour = zipPlain [White]
  , icount   = intToDeep 1
  , iverbApply   = "squeeze down"
  , iverbProject = "toss"
  , iweight  = 15
  , itoThrow = 0
  , ifeature = [Cause $ Searching (rollDeep (1, 6) (3, 2))]
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "tablet"
  , ifreq    = [("useful", 4)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , icount   = intToDeep 1
  , iverbApply   = "dial"
  , iverbProject = "lob"
  , iweight  = 700
  , itoThrow = -25  -- bad grip
  , ifeature = []
  }
scroll1 = scroll
  { ifreq    = [("useful", 2)]
  , ifeature = [Cause $ CallFriend 1]
  }
scroll2 = scroll
  { ifeature = [Cause $ Summon 1]
  }
scroll3 = scroll
  { ifeature = [Cause $ Ascend (-1)]
  }
sword = ItemKind
  { isymbol  = '/'
  , iname    = "sharpened pipe"
  , ifreq    = [("useful", 40)]
  , iflavour = zipPlain [Cyan]
  , icount   = intToDeep 1
  , iverbApply   = "hit"
  , iverbProject = "heave"
  , iweight  = 3000
  , itoThrow = -25  -- bad grip
  , ifeature = [Cause $ Hurt (rollDice 3 1) (rollDeep (1, 2) (4, 2))]
  }
wand = ItemKind
  { isymbol  = '-'
  , iname    = "injector"
  , ifreq    = [("useful", 15)]
  , iflavour = zipFancy brightCol
  , icount   = intToDeep 1
  , iverbApply   = "snap"
  , iverbProject = "zap"
  , iweight  = 300
  , itoThrow = 25  -- jet
  , ifeature = [Fragile]
  }
wand1 = wand
  { ifeature = ifeature wand ++ [Cause Dominate]
  }
wand2 = wand
  { ifreq    = [("useful", 3)]
  , ifeature = ifeature wand ++ [Cause $ Heal (-25)]
  }
fist = sword
  { isymbol  = '@'
  , iname    = "fist"
  , ifreq    = [("hth", 1), ("unarmed", 100)]
  , iverbApply   = "punch"
  , iverbProject = "ERROR, please report: iverbProject fist"
  , ifeature = [Cause $ Hurt (rollDice 3 1) (intToDeep 0)]
  }
foot = sword
  { isymbol  = '@'
  , iname    = "foot"
  , ifreq    = [("hth", 1), ("unarmed", 50)]
  , iverbApply   = "kick"
  , iverbProject = "ERROR, please report: iverbProject foot"
  , ifeature = [Cause $ Hurt (rollDice 3 1) (intToDeep 0)]
  }
tentacle = sword
  { isymbol  = 'S'
  , iname    = "tentacle"
  , ifreq    = [("hth", 1), ("monstrous", 100)]
  , iverbApply   = "hit"
  , iverbProject = "ERROR, please report: iverbProject tentacle"
  , ifeature = [Cause $ Hurt (rollDice 3 1) (intToDeep 0)]
  }
fragrance = ItemKind
  { isymbol  = '\''
  , iname    = "fragrance"
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = rollDeep (5, 2) (0, 0)
  , iverbApply   = "smell"
  , iverbProject = "exude"
  , iweight  = 1
  , itoThrow = -93  -- the slowest that gets anywhere (1 step only)
  , ifeature = [Fragile]
  }
mist_healing = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("mist healing", 1)]
  , iflavour = zipFancy [White]
  , icount   = rollDeep (12, 2) (0, 0)
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , itoThrow = -87  -- the slowest that travels at least 2 steps
  , ifeature = [Cause $ Heal 1, Fragile]
  }
mist_wounding = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("mist wounding", 1)]
  , iflavour = zipFancy [White]
  , icount   = rollDeep (12, 2) (0, 0)
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , itoThrow = -87
  , ifeature = [Cause $ Heal (-1), Fragile]
  }
glass_piece = ItemKind
  { isymbol  = '\''
  , iname    = "glass piece"
  , ifreq    = [("glass piece", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = rollDeep (10, 2) (0, 0)
  , iverbApply   = "grate"
  , iverbProject = "toss"
  , iweight  = 10
  , itoThrow = 0
  , ifeature = [Cause $ Hurt (rollDice 1 1) (intToDeep 0), Fragile, Linger 20]
  }
smoke = ItemKind
  { isymbol  = '\''
  , iname    = "smoke"
  , ifreq    = [("smoke", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = rollDeep (12, 2) (0, 0)
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , itoThrow = -70
  , ifeature = [Fragile]
  }
