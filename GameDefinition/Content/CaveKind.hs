-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Cave layouts.
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
  , validateSingle = validateSingleCaveKind
  , validateAll = validateAllCaveKind
  , content =
      [rogue, arena, empty, noise, battle, skirmish, ambush, safari1, safari2, safari3, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1arena]
  }
rogue,        arena, empty, noise, battle, skirmish, ambush, safari1, safari2, safari3, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1arena :: CaveKind

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "Storage area"
  , cfreq         = [("campaign random", 100), ("caveRogue", 1)]
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
  , cactorCoeff   = 15  -- the maze requires time to explore
  , cactorFreq    = [("alien", 50), ("animal", 25), ("robot", 25)]
  , citemNum      = 13 * d 2
  , citemFreq     = [("useful", 50), ("treasure", 50)]
  , cplaceFreq    = [("rogue", 100)]
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
  , cfreq         = [("campaign random", 100), ("caveArena", 1)]
  , cgrid         = DiceXY (2 * d 2) (2 * d 2)
  , cminPlaceSize = DiceXY (2 * d 2 + 3) 4
  , cdarkChance   = d 80 + dl 60
  , cnightChance  = 0
  , cmaxVoid      = 1%4
  , chidden       = 1000
  , cactorCoeff   = 10
  , cactorFreq    = [("alien", 25), ("animal", 70), ("robot", 5)]
  , citemNum      = 11 * d 2  -- few rooms
  , cpassable     = True
  , cdefTile      = "arenaSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Construction site"
  , cfreq         = [("campaign random", 100), ("caveEmpty", 1)]
  , cgrid         = DiceXY (d 2 + 1) 1
  , cminPlaceSize = DiceXY 10 10
  , cmaxPlaceSize = DiceXY 24 15
  , cdarkChance   = d 80 + dl 80
  , cnightChance  = 0
  , cauxConnects  = 1
  , cmaxVoid      = 1%2
  , cminStairDist = 50
  , chidden       = 1000
  , cactorCoeff   = 8
  , cactorFreq    = [("alien", 25), ("animal", 5), ("robot", 70)]
  , citemNum      = 9 * d 2  -- few rooms
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Machine rooms"
  , cfreq         = [("caveNoise", 1)]
  , cgrid         = DiceXY (2 + d 2) 3
  , cminPlaceSize = DiceXY 12 5
  , cmaxPlaceSize = DiceXY 24 15
  , cnightChance  = d 80 + dl 80
  , cauxConnects  = 0
  , cmaxVoid      = 0
  , chidden       = 1000
  , cactorCoeff   = 20  -- the maze requires time to explore
  , cactorFreq    = [("alien", 70), ("animal", 15), ("robot", 15)]
  , citemNum      = 15 * d 2  -- an incentive to explore the labyrinth
  , cpassable     = True
  , cplaceFreq    = [("noise", 100)]
  , cdefTile      = "noiseSet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
battle = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'B'
  , cname         = "Ravaged spaceport"
  , cfreq         = [("caveBattle", 1)]
  , cgrid         = DiceXY (2 * d 2 + 1) 3
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 9 7
  , cdarkChance   = 0
  , cnightChance  = 100
  , cmaxVoid      = 0
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , chidden       = 1000
  , cactorFreq    = []
  , citemNum      = 20 * d 2
  , citemFreq     = [("useful", 100), ("light source", 200)]
  , cplaceFreq    = [("battle", 50), ("rogue", 50)]
  , cpassable     = True
  , cdefTile      = "battleSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
skirmish = rogue  -- many random solid tiles, to break LOS, since it's a day
  { csymbol       = 'S'
  , cname         = "Woodland biosphere"
  , cfreq         = [("caveSkirmish", 1)]
  , cgrid         = DiceXY (2 * d 2 + 2) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkChance   = 100
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 1000
  , cactorFreq    = []
  , citemNum      = 20 * d 2
  , citemFreq     = [("useful", 100)]
  , cplaceFreq    = [("skirmish", 60), ("rogue", 40)]
  , cpassable     = True
  , cdefTile      = "skirmishSet"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  }
ambush = rogue  -- lots of lights, to give a chance to snipe
  { csymbol       = 'M'
  , cname         = "Public garden"
  , cfreq         = [("caveAmbush", 1)]
  , cgrid         = DiceXY (2 * d 2 + 3) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkChance   = 0
  , cnightChance  = 100
  , cauxConnects  = 1
  , cdoorChance   = 1%10
  , copenChance   = 9%10
  , chidden       = 1000
  , cactorFreq    = []
  , citemNum      = 22 * d 2
  , citemFreq     = [("useful", 100)]
  , cplaceFreq    = [("ambush", 100)]
  , cpassable     = True
  , cdefTile      = "ambushSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
safari1 = ambush {cfreq = [("caveSafari1", 1)]}
safari2 = battle {cfreq = [("caveSafari2", 1)]}
safari3 = skirmish {cfreq = [("caveSafari3", 1)]}
bridge = rogue
  { csymbol       = 'B'
  , cname         = "Captain's bridge"
  , cfreq         = [("caveBridge", 1)]
  , cgrid         = DiceXY (d 2 + 1) 2
  , cminPlaceSize = DiceXY 10 7
  , cmaxPlaceSize = DiceXY 40 14
  , cdarkChance   = 0
  , cmaxVoid      = 1%10
  , cactorCoeff   = 0  -- safe, nothing spawns
  , citemNum      = 9 * d 2  -- few rooms
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq arena
  , cdarkCorTile  = "emergency walkway"
  , clitCorTile   = "emergency walkway"
  }
shallow2rogue = rogue
  { cfreq         = [("shallow random 2", 100)]
  , cactorFreq    = filter ((/= "alien") . fst) $ cactorFreq rogue
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  }
shallow2arena = arena
  { cfreq         = [("shallow random 2", 100)]
  , cactorFreq    = filter ((/= "alien") . fst) $ cactorFreq empty
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  }
shallow2empty = empty
  { cfreq         = [("shallow random 2", 100)]
  , cactorFreq    = filter ((/= "alien") . fst) $ cactorFreq empty
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  }
shallow1arena = shallow2arena  -- TODO: replace some rooms with oriels?
  { cname         = "Outermost deck"
  , cfreq         = [("shallow random 1", 100)]
  , cminPlaceSize = DiceXY (2 * d 2 + 3) 3
  , couterFenceTile = "oriels fence"
  }
