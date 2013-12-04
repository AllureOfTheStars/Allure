-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Cave layouts for Allure of the Stars.
module Content.CaveKind ( cdefs ) where

import Data.Ratio

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Random as Random
import Game.LambdaHack.Content.CaveKind

cdefs :: ContentDef CaveKind
cdefs = ContentDef
  { getSymbol = csymbol
  , getName = cname
  , getFreq = cfreq
  , validate = cvalidate
  , content =
      [rogue, arena, empty, noise, combat]
  }
rogue,        arena, empty, noise, combat :: CaveKind

rogue = CaveKind
  { csymbol       = '$'
  , cname         = "Storage area"
  , cfreq         = [("dng", 100), ("caveRogue", 1)]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = rollDiceXY [(3, 2)] [(1, 2), (2, 1)]
  , cminPlaceSize = rollDiceXY [(2, 2), (2, 1)] [(4, 1)]
  , cmaxPlaceSize = rollDiceXY [(fst normalLevelBound, 1)]
                               [(snd normalLevelBound, 1)]
  , cdarkChance   = rollDeep (1, 54) (0, 0)
  , cauxConnects  = 1%3
  , cmaxVoid      = 1%6
  , cminStairDist = 30
  , cdoorChance   = 1%2
  , copenChance   = 1%10
  , chidden       = 8
  , citemNum      = rollDice 7 2
  , cdefTile        = "fillerWall"
  , ccorridorTile   = "darkCorridor"
  , cfillerTile     = "fillerWall"
  , cdarkLegendTile = "darkLegend"
  , clitLegendTile  = "litLegend"
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Recreational deck"
  , cfreq         = [("dng", 100), ("caveArena", 1)]
  , cgrid         = rollDiceXY [(2, 2)] [(2, 2)]
  , cminPlaceSize = rollDiceXY [(2, 2), (3, 1)] [(4, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 60)
  , cmaxVoid      = 1%3
  , chidden       = 9
  , citemNum      = rollDice 5 2  -- few rooms
  , cdefTile      = "arenaSet"
  , ccorridorTile = "path"
  }
empty = rogue
  { csymbol       = '.'
  , cname         = "Construction site"
  , cfreq         = [("dng", 100), ("caveEmpty", 1)]
  , cgrid         = rollDiceXY [(1, 2)] [(1, 1)]
  , cminPlaceSize = rollDiceXY [(10, 1)] [(10, 1)]
  , cmaxPlaceSize = rollDiceXY [(fst normalLevelBound * 3 `div` 5, 1)]
                               [(snd normalLevelBound * 3 `div` 5, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 80)
  , cauxConnects  = 1
  , cmaxVoid      = 0
  , cminStairDist = 50
  , chidden       = 100
  , citemNum      = rollDice 8 2  -- whole floor strewn with treasure
  , cdefTile      = "emptySet"
  , ccorridorTile = "emptySet"
  }
noise = rogue
  { csymbol       = '!'
  , cname         = "Machine rooms"
  , cfreq         = [("dng", 100), ("caveNoise", 1)]
  , cgrid         = rollDiceXY [(2, 2)] [(1, 2), (1, 1)]
  , cminPlaceSize = rollDiceXY [(3, 2), (2, 1)] [(5, 1)]
  , cdarkChance   = rollDeep (1, 80) (1, 40)
  , cmaxVoid      = 0
  , chidden       = 6
  , citemNum      = rollDice 4 2  -- fewer rooms
  , cdefTile      = "noiseSet"
  , ccorridorTile = "path"
  }
combat = rogue
  { csymbol       = 'C'
  , cname         = "Combat arena"
  , cfreq         = [("caveCombat", 1)]
  , cgrid         = rollDiceXY [(2, 2), (3, 1)] [(1, 2), (2, 1)]
  , cminPlaceSize = rollDiceXY [(3, 1)] [(3, 1)]
  , cmaxPlaceSize = rollDiceXY [(5, 1)] [(5, 1)]
  , cdarkChance   = intToDeep 100
  , chidden       = 2
  , cauxConnects  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , citemNum      = rollDice 12 2
  , cdefTile      = "combatSet"
  , ccorridorTile = "arenaSet"
  }
