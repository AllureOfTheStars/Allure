-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Weapons and treasure for Allure of the Stars.
module Content.ItemKind ( cdefs ) where

import Data.List

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Effect
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.ItemFeature
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
  , icount   = 1
  , iverbApply   = "tear down"
  , iverbProject = "cast"
  , iweight  = 30
  , iaspects = [Regeneration (2 * d 3 + dl 10)]
  , ieffects = []  -- TODO: DropAllEqp? change text if so
  , ifeature = [ToThrow (-50)]  -- not dense enough
  , idesc    = "A necklace of dried herbs and healing berries."
  }
dart = ItemKind
  { isymbol  = '{'
  , iname    = "billiard ball"
  , ifreq    = [("useful", 20), ("fallback item", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 3 * d 3
  , iverbApply   = "splinter"
  , iverbProject = "hurl"
  , iweight  = 50
  , iaspects = []
  , ieffects = [Hurt (d 2) (d 2 + dl 2)]
  , ifeature = []
  , idesc    = "Little, but sharp and sturdy."
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "precious gem"
  , ifreq    = [("treasure", 20)]  -- x3, but rare on shallow levels
  , iflavour = zipPlain $ delete BrYellow brightCol  -- natural, so not fancy
  , icount   = 0
  , iverbApply   = "crush"
  , iverbProject = "toss"
  , iweight  = 50
  , iaspects = []
  , ieffects = []
  , ifeature = [Light 0]  -- just reflects strongly
  , idesc    = "Precious, though useless. Worth around 100 gold."
  }
gem1 = gem
  { icount   = dl 1  -- appears on max depth
  }
gem2 = gem
  { icount   = dl 2  -- appears halfway
  }
gem3 = gem
  { icount   = dl 3  -- appears early
  }
currency = ItemKind
  { isymbol  = '$'
  , iname    = "gold grain"
  , ifreq    = [("treasure", 20), ("currency", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 10 * dl 10  -- appears on lvl 2
  , iverbApply   = "smear"
  , iverbProject = "blow away"
  , iweight  = 1
  , iaspects = []
  , ieffects = []
  , ifeature = []
  , idesc    = "Reliably valuable in every civilized place."
  }
javelin = ItemKind  -- rename/replace by hook/hack/harpoon
  { isymbol  = '{'
  , iname    = "javelin"
  , ifreq    = [("useful", 30)]
  , iflavour = zipPlain [Brown]
  , icount   = dl 1
  , iverbApply   = "break up"
  , iverbProject = "hurl"
  , iweight  = 2000
  , iaspects = []
  , ieffects = [Hurt (2 * d 2) (d 2 + 2 * dl 2), PullActor 100 50]
  , ifeature = []
  , idesc    = "The cruel, barbed head lodges in its victim so painfully that the weakest tug of the thin line sends the victim flying."
  }
kitchenKnife = ItemKind
  { isymbol  = '{'
  , iname    = "kitchen knife"
  , ifreq    = [("useful", 25)]
  , iflavour = zipPlain [BrCyan]
  , icount   = dl 2
  , iverbApply   = "bend"
  , iverbProject = "throw"
  , iweight  = 200
  , iaspects = []
  , ieffects = [Hurt (1 * d 1) (dl 2)]
  , ifeature = [ToThrow (-50)]  -- too flexible and the handle causes air drag
  , idesc    = ""
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "vial"
  , ifreq    = [("useful", 15)]
  , iflavour = zipFancy stdCol
  , icount   = 1
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  , iweight  = 200
  , iaspects = []
  , ieffects = []
  , ifeature = [ ToThrow (-50)  -- oily, bad grip
               , Consumable, Fragile ]
  , idesc    = "A flask of bubbly, slightly oily liquid of a suspect color."
  }
potion1 = potion
  { iaspects = [Explode "fragrance"]
  , ieffects = [ApplyPerfume, Impress]
  }
potion2 = potion
  { iaspects = [Explode "healing mist"]
  , ieffects = [Heal 5]
  }
potion3 = potion  -- TODO: a bit boring
  { ifreq    = [("useful", 5)]
  , iaspects = [Explode "wounding mist"]
  , ieffects = [Heal (-5)]
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("useful", 6)]
  , iflavour = zipPlain [White]
  , icount   = 1
  , iverbApply   = "squeeze down"
  , iverbProject = "toss"
  , iweight  = 15
  , iaspects = [Steadfastness (d 2 + 2 * dl 2)]
  , ieffects = []  -- TODO: add something
  , ifeature = []
  , idesc    = "Cold, solid to the touch, perfectly round, engraved with letters that meant a lot to somebody."
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "tablet"
  , ifreq    = [("useful", 4)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , icount   = 1
  , iverbApply   = "dial"
  , iverbProject = "lob"
  , iweight  = 700
  , iaspects = []
  , ieffects = []
  , ifeature = [ ToThrow (-25)  -- bad grip
               , Consumable ]
  , idesc    = "A haphazardly scribbled piece of parchment. May contain directions or a secret call sign."
  }
scroll1 = scroll
  { ifreq    = [("useful", 2)]
  , ieffects = [CallFriend 1]
  }
scroll2 = scroll
  { ieffects = [Summon 1]
  }
scroll3 = scroll
  { ieffects = [Ascend (-1)]
  }
sword = ItemKind
  { isymbol  = '/'
  , iname    = "sharpened pipe"
  , ifreq    = [("useful", 40)]
  , iflavour = zipPlain [Cyan]
  , icount   = 1
  , iverbApply   = "hit"
  , iverbProject = "heave"
  , iweight  = 3000
  , iaspects = []
  , ieffects = [Hurt (5 * d 1) (d 2 + 4 * dl 2)]
  , ifeature = [ToThrow (-25)]  -- bad grip
  , idesc    = "A standard heavy weapon. Does not penetrate very effectively, but hard to block."
  }
wand = ItemKind
  { isymbol  = '-'
  , iname    = "injector"
  , ifreq    = []  -- TODO: add charges, etc.  -- [("useful", 2)]
  , iflavour = zipFancy brightCol
  , icount   = 1
  , iverbApply   = "snap"
  , iverbProject = "zap"
  , iweight  = 300
  , iaspects = []
  , ieffects = []
  , ifeature = [ ToThrow 25  -- jet
               , Light 1
               , Fragile ]
  , idesc    = "Buzzing with dazzling light that shines even through appendages that handle it."
  }
wand1 = wand
  { ieffects = [NoEffect]  -- TODO: emit a cone of sound shrapnel that makes enemy cover his ears and so drop '|' and '{'
  }
wand2 = wand
  { ieffects = [NoEffect]
  }
fist = sword
  { isymbol  = '%'
  , iname    = "fist"
  , ifreq    = [("fist", 1), ("unarmed", 100)]
  , iverbApply   = "punch"
  , iverbProject = "ERROR, please report: iverbProject fist"
  , ieffects = [Hurt (5 * d 1) 0]
  }
foot = sword
  { isymbol  = '%'
  , iname    = "foot"
  , ifreq    = [("foot", 1), ("unarmed", 50)]
  , iverbApply   = "kick"
  , iverbProject = "ERROR, please report: iverbProject foot"
  , ieffects = [Hurt (5 * d 1) 0]
  }
tentacle = sword
  { isymbol  = 'S'
  , iname    = "tentacle"
  , ifreq    = [("hth", 1), ("monstrous", 100)]
  , iverbApply   = "hit"
  , iverbProject = "ERROR, please report: iverbProject tentacle"
  , ieffects = [Hurt (5 * d 1) 0]
  }
fragrance = ItemKind
  { isymbol  = '\''
  , iname    = "fragrance"
  , ifreq    = [("fragrance", 1)]
  , iflavour = zipFancy [BrMagenta]
  , icount   = 15
  , iverbApply   = "smell"
  , iverbProject = "exude"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Impress]
  , ifeature = [ ToThrow (-87)  -- the slowest that travels at least 2 steps
               , Fragile ]
  , idesc    = ""
  }
mist_healing = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("healing mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 11
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Heal 2]
  , ifeature = [ ToThrow (-93)  -- the slowest that gets anywhere (1 step only)
               , Light 0
               , Fragile ]
  , idesc    = ""
  }
mist_wounding = ItemKind
  { isymbol  = '\''
  , iname    = "mist"
  , ifreq    = [("wounding mist", 1)]
  , iflavour = zipFancy [White]
  , icount   = 13
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , iaspects = []
  , ieffects = [Heal (-2)]
  , ifeature = [ ToThrow (-93)  -- the slowest that gets anywhere (1 step only)
               , Fragile ]
  , idesc    = ""
  }
glass_piece = ItemKind
  { isymbol  = '\''
  , iname    = "glass piece"
  , ifreq    = [("glass piece", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 17
  , iverbApply   = "grate"
  , iverbProject = "toss"
  , iweight  = 10
  , iaspects = []
  , ieffects = [Hurt (d 1) 0]
  , ifeature = [Fragile, Linger 20]
  , idesc    = ""
  }
smoke = ItemKind
  { isymbol  = '\''
  , iname    = "smoke"
  , ifreq    = [("smoke", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 19
  , iverbApply   = "inhale"
  , iverbProject = "blow"
  , iweight  = 1
  , iaspects = []
  , ieffects = []
  , ifeature = [ ToThrow (-70)
               , Fragile ]
  , idesc    = ""
  }
