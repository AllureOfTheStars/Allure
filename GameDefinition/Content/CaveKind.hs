-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Cave layouts for Allure of the Stars.
module Content.CaveKind ( cdefs ) where

import Data.Ratio

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.CaveKind

cdefs :: ContentDef CaveKind
cdefs = ContentDef
  { getSymbol = csymbol
  , getName = cname
  , getFreq = cfreq
  , validate = validateCaveKind
  , content =
      [rogue, arena, empty, noise, combat, battle]
  }
rogue,        arena, empty, noise, combat, battle :: CaveKind

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "Storage area"
  , cfreq         = [("dng", 100), ("caveRogue", 1)]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = DiceXY (3 * d 2) (d 2 + 2)
  , cminPlaceSize = DiceXY (2 * d 2 + 2) 4
  , cmaxPlaceSize = DiceXY 15 10
  , cdarkChance   = d 54 + dl 20
  , cnightChance  = 51
  , cauxConnects  = 1%3
  , cmaxVoid      = 1%6
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chidden       = 8
  , citemNum      = 8 * d 2
  , citemFreq     = [(70, "useful"), (30, "treasure")]
  , cplaceFreq    = [(100, "rogue")]
  , cpassable     = False
  , cdefTile        = "fillerWall"
  , cdarkCorTile    = "floorCorridorDark"
  , clitCorTile     = "floorCorridorLit"
  , cfillerTile     = "fillerWall"
  , couterFenceTile = "basic outer fence"
  , clegendDarkTile = "legendDark"
  , clegendLitTile  = "legendLit"
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Recreational deck"
  , cfreq         = [("dng", 100), ("caveArena", 1)]
  , cgrid         = DiceXY (2 * d 2) (2 * d 2)
  , cminPlaceSize = DiceXY (2 * d 2 + 3) 4
  , cdarkChance   = d 80 + dl 60
  , cnightChance  = 0
  , cmaxVoid      = 1%3
  , chidden       = 1000
  , citemNum      = 6 * d 2  -- few rooms
  , cpassable     = True
  , cdefTile      = "arenaSet"
  , cdarkCorTile  = "trailLit"  -- let paths around rooms be lit
  , clitCorTile   = "trailLit"
--  , couterFenceTile = "oriels fence"
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Construction site"
  , cfreq         = [("dng", 100), ("caveEmpty", 1)]
  , cgrid         = DiceXY (d 2 + 1) 1
  , cminPlaceSize = DiceXY 10 10
  , cmaxPlaceSize = DiceXY 24 12
  , cdarkChance   = d 80 + dl 80
  , cnightChance  = 0
  , cauxConnects  = 1
  , cmaxVoid      = 1%2
  , cminStairDist = 50
  , chidden       = 1000
  , citemNum      = 4 * d 2  -- few rooms
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "trailLit"  -- let paths around rooms be lit
  , clitCorTile   = "floorArenaLit"
  , couterFenceTile = "oriels fence"
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Machine rooms"
  , cfreq         = [("dng", 100), ("caveNoise", 1)]
  , cgrid         = DiceXY 3 3
  , cminPlaceSize = DiceXY 8 4
  , cmaxPlaceSize = DiceXY 24 12
  , cnightChance  = d 100
  , cauxConnects  = 0
  , cmaxVoid      = 0
  , chidden       = 1000
  , citemNum      = 10 * d 2  -- an incentive to explore the labyrinth
  , cpassable     = True
--  , cplaceFreq    = [(50, "noise"), (50, "rogue")]
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
combat = rogue
  { csymbol       = 'C'
  , cname         = "Combat arena"
  , cfreq         = [("caveCombat", 1)]
  , cgrid         = DiceXY (2 * d 2 + 2) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkChance   = 100
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 1000
  , citemNum      = 12 * d 2
  , citemFreq     = [(100, "useful")]
--  , cplaceFreq    = [(60, "skirmish"), (40, "rogue")]
  , cpassable     = True
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  }
battle = combat  -- TODO: actors can get stuck forever among trees
  { csymbol       = 'B'
  , cname         = "Battle arena"
  , cfreq         = [("caveBattle", 1)]
  , cdefTile      = "noiseSet"
  }
