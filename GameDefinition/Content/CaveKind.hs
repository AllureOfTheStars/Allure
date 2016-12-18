-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Cave layouts.
module Content.CaveKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

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
  , content = contentFromList
      [rogue, arena, laboratory, empty, noise, battle, brawl, ambush, safari1, safari2, safari3, rogueLit, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1arena, emptyExit]
  }
rogue,        arena, laboratory, empty, noise, battle, brawl, ambush, safari1, safari2, safari3, rogueLit, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1arena, emptyExit :: CaveKind

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "Storage area"
  , cfreq         = [("default random", 100), ("caveRogue", 1)]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = DiceXY (3 * d 2) 4
  , cminPlaceSize = DiceXY (2 * d 2 + 4) 5
  , cmaxPlaceSize = DiceXY 15 10
  , cdarkChance   = d 54 + dl 20
  , cnightChance  = 51  -- always night
  , cauxConnects  = 3%4
  , cmaxVoid      = 1%6
  , cminStairDist = 15
  , cextraStairs  = 1 + d 2
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 130  -- the maze requires time to explore
  , cactorFreq    = [("alien", 50), ("animal", 25), ("robot", 25)]
  , citemNum      = 10 * d 2
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
  , cescapeGroup    = Nothing
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Recreational deck"
  , cfreq         = [("default random", 50), ("caveArena", 1)]
  , cgrid         = DiceXY (2 * d 2) (d 3)
  , cminPlaceSize = DiceXY (2 * d 2 + 4) 6
  , cdarkChance   = d 100 - dl 50
  -- Trails provide enough light for fun stealth. Light is not too deadly,
  -- because not many obstructions, so foes visible from far away.
  , cnightChance  = d 50 + dl 50
  , cmaxVoid      = 1%5
  , cextraStairs  = d 3
  , chidden       = 0
  , cactorCoeff   = 100
  , cactorFreq    = [("alien", 25), ("animal", 70), ("robot", 5)]
  , citemNum      = 9 * d 2  -- few rooms
  , citemFreq     = [("useful", 20), ("treasure", 30), ("any scroll", 50)]
  , cplaceFreq    = [("arena", 100)]
  , cpassable     = True
  , cdefTile      = "arenaSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
laboratory = arena
  { csymbol       = 'L'
  , cname         = "Burnt laboratory"
  , cfreq         = [("default random", 20), ("caveLaboratory", 1)]
  , cgrid         = DiceXY 10 3
  , cminPlaceSize = DiceXY 11 4
  , cdarkChance   = 51  -- always dark, burnt
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%10
  , cextraStairs  = d 2
  , cdoorChance   = 1
  , copenChance   = 1%2
  , chidden       = 7
  , cactorCoeff   = 160  -- deadly enough due to unclear corridors
  , citemNum      = 11 * d 2  -- reward difficulty
  , citemFreq     = [("useful", 20), ("treasure", 30), ("any vial", 50)]
  , cpassable     = False
  , cdefTile      = "fillerWall"
  , cdarkCorTile  = "labTrail"
  , clitCorTile   = "labTrail"
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Construction site"
  , cfreq         = [("default random", 20), ("caveEmpty", 1)]
  , cgrid         = DiceXY (d 2) 1
  , cminPlaceSize = DiceXY 12 12
  , cmaxPlaceSize = DiceXY 24 16
  , cdarkChance   = d 80 + dl 80
  , cnightChance  = 0  -- always day
  , cauxConnects  = 3%2
  , cmaxVoid      = 3%4
  , cminStairDist = 30
  , cextraStairs  = d 2
  , cdoorChance   = 0
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 80
  , cactorFreq    = [("alien", 25), ("animal", 5), ("robot", 70)]
  , citemNum      = 7 * d 2  -- few rooms
  , cplaceFreq    = [("empty", 100)]
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Machine rooms"
  , cfreq         = [("default random", 10), ("caveNoise", 1)]
  , cgrid         = DiceXY (2 + d 2) 3
  , cminPlaceSize = DiceXY 10 6
  , cmaxPlaceSize = DiceXY 20 10
  , cdarkChance   = 0  -- few rooms, so all lit
  -- Light is deadly, because nowhere to hide and pillars enable spawning
  -- very close to heroes, so deep down light should be rare.
  , cnightChance  = dl 300
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%100
  , cextraStairs  = d 4
  , chidden       = 0
  , cactorCoeff   = 160  -- the maze requires time to explore
  , cactorFreq    = [("alien", 70), ("animal", 15), ("robot", 15)]
  , citemNum      = 12 * d 2  -- an incentive to explore the labyrinth
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
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 20 * d 2
  , citemFreq     = [("useful", 100), ("light source", 200)]
  , cplaceFreq    = [("battle", 50), ("rogue", 50)]
  , cpassable     = True
  , cdefTile      = "battleSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
  { csymbol       = 'S'
  , cname         = "Woodland biosphere"
  , cfreq         = [("caveBrawl", 1)]
  , cgrid         = DiceXY (2 * d 2 + 2) (d 2 + 2)
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkChance   = 100
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 20 * d 2
  , citemFreq     = [("useful", 100)]
  , cplaceFreq    = [("brawl", 60), ("rogue", 40)]
  , cpassable     = True
  , cdefTile      = "brawlSet"
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
  , cnightChance  = 51  -- always night
  , cauxConnects  = 3%2
  , cdoorChance   = 1%10
  , copenChance   = 9%10
  , cextraStairs  = 1
  , chidden       = 0
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
safari3 = brawl
  { cfreq = [("caveSafari3", 1)]
  , cescapeGroup  = Just "escape outdoor down"
  }
rogueLit = rogue
  { csymbol       = 'S'
  , cname         = "Triton City Sewers"
  , cfreq         = [("caveRogueLit", 1)]
  , cdarkChance   = 0
  , cmaxVoid      = 1%10
  , cactorCoeff   = 1000  -- deep level with no eqp, so slow spawning
  , cactorFreq    = [("animal", 50), ("robot", 50)]
  , citemNum      = 30 * d 2  -- just one level, hard enemies, treasure
  , citemFreq     = [("useful", 33), ("gem", 33), ("currency", 33)]
  , cdarkCorTile  = "emergency walkway"
  , clitCorTile   = "emergency walkway"
  , cescapeGroup  = Just "escape up"
  }
bridge = rogueLit
  { csymbol       = 'B'
  , cname         = "Captain's bridge"
  , cfreq         = [("caveBridge", 1)]
  , cgrid         = DiceXY (d 2 + 1) 2
  , cminPlaceSize = DiceXY 10 7
  , cmaxPlaceSize = DiceXY 40 14
  , cactorFreq    = []  -- safe, nothing spawns
  , citemNum      = 15 * d 2  -- lure them in with loot
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  , cextraStairs  = 1
  , cescapeGroup    = Nothing
  }
shallow2rogue = rogue
  { cfreq         = [("shallow random 2", 50)]
  , cactorCoeff   = cactorCoeff rogue `div` 2
  , cactorFreq    = filter ((/= "alien") . fst) $ cactorFreq rogue
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  }
shallow2arena = arena
  { cfreq         = [("shallow random 2", 100)]
  , cnightChance  = 0  -- safe and easy
  , cactorCoeff   = cactorCoeff arena `div` 2
  , cactorFreq    = filter ((/= "alien") . fst) $ cactorFreq empty
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  }
shallow2empty = empty
  { cfreq         = [("shallow random 2", 20)]
  , cactorFreq    = filter ((/= "alien") . fst) $ cactorFreq empty
  , cactorCoeff   = cactorCoeff empty `div` 2
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  }
shallow1arena = shallow2empty  -- TODO: replace some rooms with oriels?
  { cname         = "Outermost deck"
  , cfreq         = [("shallow random 1", 100)]
  , cminPlaceSize = DiceXY (2 * d 2 + 3) 3
  , cactorCoeff   = 3  -- mostly immobile actors anyway
  , cactorFreq    = [("animal", 8), ("robot", 2), ("immobile robot", 90)]
      -- The medbot faucets on lvl 1 act like HP resets. They are needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. The items are not reset, even if the are bad, which provides
      -- enough of a continuity. The faucets on lvl 1 are not OP and can't be
      -- abused, because they spawn less and less often and they don't heal over
      -- max HP.
  , couterFenceTile = "oriels fence"
  }
emptyExit = empty
  { cfreq = [("caveEmptyExit", 1)]
  , cescapeGroup = Just "escape spaceship down"
  }
