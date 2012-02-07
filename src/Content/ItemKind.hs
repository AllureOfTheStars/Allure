-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Weapons and treasure for Allure of the Stars.
module Content.ItemKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content as Content
import Game.LambdaHack.Effect
import Game.LambdaHack.Flavour
import Game.LambdaHack.Random
import Game.LambdaHack.Content.ItemKind

cdefs :: Content.CDefs ItemKind
cdefs = Content.CDefs
  { getSymbol = isymbol
  , getName = iname
  , getFreq = ifreq
  , validate = ivalidate
  , content =
      [necklace, dart, gem1, gem2, gem3, gold, javelin, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand, fist, foot, tentacle, weight]
  }
necklace,        dart, gem1, gem2, gem3, gold, javelin, potion1, potion2, potion3, ring, scroll1, scroll2, scroll3, sword, wand, fist, foot, tentacle, weight :: ItemKind

gem, potion, scroll :: ItemKind  -- generic templates

-- rollDeep (aDb, xDy) = rollDice aDb + lvl * rollDice xDy / depth

necklace = ItemKind
  { isymbol  = '"'
  , iname    = "necklace"
  , ifreq    = [("dng", 6)]
  , iflavour = zipFancy [BrGreen]
  , ieffect  = Regeneration
  , icount   = intToDeep 1
  , ipower   = (RollDice 2 3, RollDice 1 10)
  , iverbApply   = "tear down"
  , iverbProject = "throw"
  }
dart = ItemKind
  { isymbol  = '|'
  , iname    = "billiard ball"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [Cyan]
  , ieffect  = Wound (RollDice 1 1)
  , icount   = (RollDice 3 3, RollDice 0 0)
  , ipower   = intToDeep 0
  , iverbApply   = "snap"
  , iverbProject = "throw"
  }
gem = ItemKind
  { isymbol  = '*'
  , iname    = "precious gem"
  , ifreq    = [("dng", 20)]       -- x3, but rare on shallow levels
  , iflavour = zipPlain brightCol  -- natural, so not fancy
  , ieffect  = NoEffect
  , icount   = intToDeep 0
  , ipower   = intToDeep 0
  , iverbApply   = "crush"
  , iverbProject = "throw"
  }
gem1 = gem
  { icount   = (RollDice 0 0, RollDice 1 1)  -- appears on max depth
  }
gem2 = gem
  { icount   = (RollDice 0 0, RollDice 1 2)  -- appears halfway, doubled on max
  }
gem3 = gem
  { icount   = (RollDice 0 0, RollDice 1 3)
  }
gold = ItemKind
  { isymbol  = '$'
  , iname    = "gold coin"
  , ifreq    = [("dng", 80)]
  , iflavour = zipPlain [BrYellow]
  , ieffect  = NoEffect
  , icount   = (RollDice 0 0, RollDice 10 10)
  , ipower   = intToDeep 0
  , iverbApply   = "grind"
  , iverbProject = "throw"
  }
javelin = ItemKind
  { isymbol  = '|'
  , iname    = "kitchen knife"
  , ifreq    = [("dng", 30)]
  , iflavour = zipPlain [Brown]
  , ieffect  = Wound (RollDice 1 2)
  , icount   = (RollDice 0 0, RollDice 2 2)
  , ipower   = (RollDice 1 1, RollDice 2 2)
  , iverbApply   = "break up"
  , iverbProject = "throw"
  }
potion = ItemKind
  { isymbol  = '!'
  , iname    = "concoction"
  , ifreq    = [("dng", 15)]
  , iflavour = zipFancy stdCol
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "gulp down"
  , iverbProject = "lob"
  }
potion1 = potion
  { ifreq    = [("dng", 5)]
  , ieffect  = ApplyPerfume
  }
potion2 = potion
  { ieffect  = Heal
  , ipower   = (RollDice 5 1, RollDice 0 0)
  }
potion3 = potion
  { ifreq    = [("dng", 5)]
  , ieffect  = Wound (RollDice 0 0)
  , ipower   = (RollDice 5 1, RollDice 0 0)
  }
ring = ItemKind
  { isymbol  = '='
  , iname    = "ring"
  , ifreq    = [("dng", 10)]
  , iflavour = zipPlain [White]
  , ieffect  = Searching
  , icount   = intToDeep 1
  , ipower   = (RollDice 1 6, RollDice 3 2)
  , iverbApply   = "squeeze down"
  , iverbProject = "throw"
  }
scroll = ItemKind
  { isymbol  = '?'
  , iname    = "comm tablet"
  , ifreq    = [("dng", 6)]
  , iflavour = zipFancy darkCol  -- arcane and old
  , ieffect  = NoEffect
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "dial"
  , iverbProject = "throw"
  }
scroll1 = scroll
  { ieffect  = SummonFriend
  }
scroll2 = scroll
  { ifreq    = [("dng", 3)]
  , ieffect  = SummonEnemy
  }
scroll3 = scroll
  { ieffect  = Descend
  }
sword = ItemKind
  { isymbol  = ')'
  , iname    = "sharpened pipe"
  , ifreq    = [("dng", 60)]
  , iflavour = zipPlain [BrCyan]
  , ieffect  = Wound (RollDice 3 1)
  , icount   = intToDeep 1
  , ipower   = (RollDice 1 2, RollDice 4 2)
  , iverbApply   = "hit"
  , iverbProject = "heave"
  }
wand = ItemKind
  { isymbol  = '/'
  , iname    = "transmitter"
  , ifreq    = [("dng", 15)]
  , iflavour = zipFancy [BrRed]
  , ieffect  = Dominate
  , icount   = intToDeep 1
  , ipower   = intToDeep 0
  , iverbApply   = "snap"
  , iverbProject = "zap"
  }
fist = sword
  { isymbol  = '@'
  , iname    = "fist"
  , ifreq    = [("unarmed", 100)]
  , iverbApply   = "punch"
  , iverbProject = "ERROR, please report: iverbProject fist"
  }
foot = sword
  { isymbol  = '@'
  , iname    = "foot"
  , ifreq    = [("unarmed", 50)]
  , iverbApply   = "kick"
  , iverbProject = "ERROR, please report: iverbProject foot"
  }
tentacle = sword
  { isymbol  = 'S'
  , iname    = "tentacle"
  , ifreq    = [("monstrous", 100)]
  , iverbApply   = "hit"
  , iverbProject = "ERROR, please report: iverbProject tentacle"
  }
weight = sword
  { isymbol  = '@'
  , iname    = "power jump"
  , ifreq    = [("weight", 100)]
  , ieffect  = Wound (RollDice 99 99)
  , ipower   = (RollDice 1 99, RollDice 0 0)
  , iverbApply   = "squash"
  , iverbProject = "ERROR, please report: iverbProject weight"
  }
