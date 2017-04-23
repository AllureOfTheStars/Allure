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
      [rogue, arena, laboratory, empty, noise, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1arena, emptyExit, raid, brawl, shootout, escape, zoo, ambush, battle, safari1, safari2, safari3]
  }
rogue,        arena, laboratory, empty, noise, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1arena, emptyExit, raid, brawl, shootout, escape, zoo, ambush, battle, safari1, safari2, safari3 :: CaveKind

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
  , cauxConnects  = 1%2
  , cmaxVoid      = 1%6
  , cminStairDist = 15
  , cextraStairs  = 1 + d 2
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 130  -- the maze requires time to explore
  , cactorFreq    = [("monster", 50), ("animal", 25), ("robot", 25)]
  , citemNum      = 4 * d 4
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
  , cstairFreq      = [("staircase", 100)]
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Recreational deck"
  , cfreq         = [("default random", 40), ("caveArena", 1)]
  , cgrid         = DiceXY (2 * d 2) (d 3)
  , cminPlaceSize = DiceXY (2 * d 2 + 4) 6
  , cmaxPlaceSize = DiceXY 12 8
  , cdarkChance   = d 100 - dl 50
  -- Trails provide enough light for fun stealth. Light is not too deadly,
  -- because not many obstructions, so foes visible from far away.
  , cnightChance  = d 50 + dl 50
  , cextraStairs  = d 3
  , chidden       = 0
  , cactorCoeff   = 100
  , cactorFreq    = [("monster", 25), ("animal", 70), ("robot", 5)]
  , citemNum      = 3 * d 4  -- few rooms
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
  , cfreq         = [("default random", 40), ("caveLaboratory", 1)]
  , cgrid         = DiceXY (2 * d 2 + 7) 3
  , cminPlaceSize = DiceXY (3 * d 2 + 4) 5
  , cdarkChance   = 51  -- always dark, burnt
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%10
  , cextraStairs  = d 2
  , cdoorChance   = 1
  , copenChance   = 1%2
  , chidden       = 7
  , cactorCoeff   = 160  -- deadly enough due to unclear corridors
  , citemNum      = 5 * d 4  -- reward difficulty
  , citemFreq     = [("useful", 20), ("treasure", 30), ("any vial", 50)]
  , cplaceFreq    = [("laboratory", 100)]
  , cpassable     = False
  , cdefTile      = "fillerWall"
  , cdarkCorTile  = "labTrailLit"
  , clitCorTile   = "labTrailLit"
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Construction site"
  , cfreq         = [("default random", 20), ("caveEmpty", 1)]
  , cgrid         = DiceXY (d 2) 1
  , cminPlaceSize = DiceXY 12 12
  , cmaxPlaceSize = DiceXY 48 32  -- favour large rooms
  , cdarkChance   = d 80 + dl 80
  , cnightChance  = 0  -- always day
  , cauxConnects  = 3%2
  , cminStairDist = 30
  , cextraStairs  = d 2
  , cdoorChance   = 0
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 80
  , cactorFreq    = [("monster", 25), ("animal", 5), ("robot", 70)]
  , citemNum      = 3 * d 4  -- few rooms
  , cplaceFreq    = [("empty", 100)]
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Machine rooms"
  , cfreq         = [("caveNoise", 1)]
  , cgrid         = DiceXY (2 + d 3) 3
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
  , cactorFreq    = [("monster", 70), ("animal", 15), ("robot", 15)]
  , citemNum      = 5 * d 4  -- an incentive to explore the labyrinth
  , cpassable     = True
  , cplaceFreq    = [("noise", 100)]
  , cdefTile      = "noiseSet"
  , couterFenceTile = "noise fence"  -- ensures no cut-off parts from collapsed
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  , cstairFreq    = [("gated staircase", 100)]
  }
bridge = rogue
  { csymbol       = 'B'
  , cname         = "Captain's bridge"
  , cfreq         = [("caveBridge", 1)]
  , cdarkChance   = 0  -- all rooms lit, for a gentle start
  , cextraStairs  = 1
  , cactorFreq    = []  -- safe, nothing spawns
  , citemNum      = 7 * d 4  -- lure them in with loot
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  , cescapeGroup  = Nothing
  }
shallow2rogue = rogue
  { cfreq         = [("shallow random 2", 50)]
  , cactorCoeff   = cactorCoeff rogue `div` 2
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq rogue
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  }
shallow2arena = arena
  { cfreq         = [("shallow random 2", 100)]
  , cnightChance  = 0  -- safe and easy
  , cactorCoeff   = cactorCoeff arena `div` 2
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq empty
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  }
shallow2empty = empty
  { cfreq         = [("shallow random 2", 20)]
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq empty
  , cactorCoeff   = cactorCoeff empty `div` 2
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  }
shallow1arena = shallow2empty  -- TODO: replace some rooms with oriels?
  { cname         = "Outermost deck"
  , cfreq         = [("shallow random 1", 100)]
  , cactorCoeff   = 20
  , cactorFreq    = [("animal", 8), ("robot", 2), ("immobile robot", 90)]
      -- The medbot faucets on lvl 1 act like HP resets. Needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. Items are not reset, even if they are bad, which provides
      -- enough of a continuity. The faucets on lvl 1 are not OP and can't be
      -- abused, because they spawn less and less often and also HP doesn't
      -- effectively accumulate over max.
  , couterFenceTile = "oriels fence"
  }
emptyExit = empty
  { cfreq = [("caveEmptyExit", 1)]
  , cescapeGroup = Just "escape spaceship down"
  }
raid = rogue
  { csymbol       = 'S'
  , cname         = "Triton City Sewers"
  , cfreq         = [("caveRaid", 1)]
  , cdarkChance   = 0
  , cmaxVoid      = 1%10
  , cactorCoeff   = 1000  -- deep level with no kit, so slow spawning
  , cactorFreq    = [("animal", 50), ("robot", 50)]
  , citemNum      = 5 * d 8  -- just one level, hard enemies, treasure
  , citemFreq     = [("useful", 33), ("gem", 33), ("currency", 33)]
  , cescapeGroup  = Just "escape up"
  }
brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
               -- and this scenario is not focused on ranged combat;
               -- also, sanctuaries against missiles in shadow under trees
  { csymbol       = 'S'
  , cname         = "Woodland biosphere"
  , cfreq         = [("caveBrawl", 1)]
  , cgrid         = DiceXY (2 * d 2 + 2) 3
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkChance   = 100
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 * d 8
  , citemFreq     = [("useful", 100)]
  , cplaceFreq    = [("brawl", 60), ("rogue", 40)]
  , cpassable     = True
  , cdefTile      = "brawlSet"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  }
shootout = rogue  -- a scenario with strong missiles;
                  -- no solid tiles, but only translucent tiles or walkable
                  -- opaque tiles, to make scouting and sniping more interesting
                  -- and to avoid obstructing view too much, since this
                  -- scenario is about ranged combat at long range
  { csymbol       = 'S'
  , cname         = "Misty hydroponic farm"
  , cfreq         = [("caveShootout", 1)]
  , cgrid         = DiceXY (d 2 + 7) 3
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 3 4
  , cdarkChance   = 100
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 * d 16
                      -- less items in inventory, more to be picked up,
                      -- to reward explorer and aggressor and punish camper
  , citemFreq     = [ ("useful", 30)
                    , ("any arrow", 400), ("harpoon", 300)
                    , ("any vial", 60) ]
                      -- Many consumable buffs are needed in symmetric maps
                      -- so that aggresor prepares them in advance and camper
                      -- needs to waste initial turns to buff for the defence.
  , cplaceFreq    = [("shootout", 100)]
  , cpassable     = True
  , cdefTile      = "shootoutSet"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  }
escape = rogue  -- a scenario with weak missiles, because heroes don't depend
                -- on them; dark, so solid obstacles are to hide from missiles,
                -- not view; obstacles are not lit, to frustrate the AI;
                -- lots of small lights to cross, to have some risks
  { csymbol       = 'E'
  , cname         = "Public park at night"
  , cfreq         = [("caveEscape", 1)]
  , cgrid         = DiceXY -- (2 * d 2 + 3) 4  -- park, so lamps in lines
                           (2 * d 2 + 6) 3   -- for now, to fit larger places
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 7  -- bias towards larger lamp areas
  , cdarkChance   = 51  -- colonnade floors always dark
  , cnightChance  = 51  -- always night
  , cauxConnects  = 3%2
  , cmaxVoid      = 1%20
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 * d 8
  , citemFreq     = [ ("useful", 30), ("treasure", 30), ("gem", 100)
                    , ("weak arrow", 500), ("harpoon", 400) ]
  , cplaceFreq    = [("ambush", 100)]  -- the same rooms as ambush
  , cpassable     = True
  , cdefTile      = "escapeSet"  -- different tiles, not burning yet
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cescapeGroup  = Just "escape outdoor down"
  }
zoo = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'Z'
  , cname         = "Zoo in flames"
  , cfreq         = [("caveZoo", 1)]
  , cgrid         = DiceXY (2 * d 2 + 6) 3
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 7 7
  , cdarkChance   = 51  -- always dark rooms
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 7 * d 8
  , citemFreq     = [("useful", 100), ("light source", 1000)]
  , cplaceFreq    = [("zoo", 50)]
  , cpassable     = True
  , cdefTile      = "zooSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  }
ambush = rogue  -- a scenario with strong missiles;
                -- dark, so solid obstacles are to hide from missiles,
                -- not view, and they are all lit, because stopped missiles
                -- are frustrating, while a few LOS-only obstacles are not lit;
                -- lots of small lights to cross, to give a chance to snipe;
                -- a crucial difference wrt shootout is that trajectories
                -- of missiles are usually not seen, so enemy can't be guessed;
                -- camping doesn't pay off, because enemies can sneak and only
                -- active scouting, throwing flares and shooting discovers them
  { csymbol       = 'M'
  , cname         = "Burning public park"
  , cfreq         = [("caveAmbush", 1)]
  , cgrid         = DiceXY -- (2 * d 2 + 3) 4  -- park, so lamps in lines
                           (2 * d 2 + 6) 3   -- for now, to fit larger places
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 7  -- bias towards larger lamp areas
  , cdarkChance   = 51  -- colonnade floors always dark
  , cnightChance  = 51  -- always night
  , cauxConnects  = 3%2
  , cmaxVoid      = 1%20
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 * d 8
  , citemFreq     = [("useful", 30), ("any arrow", 400), ("harpoon", 300)]
  , cplaceFreq    = [("ambush", 100)]
  , cpassable     = True
  , cdefTile      = "ambushSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
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
  , citemNum      = 5 * d 8
  , citemFreq     = [("useful", 100), ("light source", 200)]
  , cplaceFreq    = [("battle", 50), ("rogue", 50)]
  , cpassable     = True
  , cdefTile      = "battleSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , couterFenceTile = "noise fence"  -- ensures no cut-off parts from collapsed
  }
safari1 = escape
  { cfreq = [("caveSafari1", 1)]
  , cescapeGroup = Nothing
  , cstairFreq = [("staircase outdoor", 1)]
  }
safari2 = shootout
  { cfreq = [("caveSafari2", 1)]
  , cstairFreq = [("staircase outdoor", 1)]
  }
safari3 = brawl
  { cfreq = [("caveSafari3", 1)]
  , cescapeGroup = Just "escape outdoor down"
  , cstairFreq = [("staircase outdoor", 1)]
  }
