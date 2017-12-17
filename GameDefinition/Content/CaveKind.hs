-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2017 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Cave properties.
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
      [rogue, rogue2, arena, arena2, laboratory, empty, noise, noise2, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1empty, emptyExit, raid, brawl, shootout, escape, zoo, ambush, battle, safari1, safari2, safari3]
  }
rogue,        rogue2, arena, arena2, laboratory, empty, noise, noise2, bridge, shallow2rogue, shallow2arena, shallow2empty, shallow1empty, emptyExit, raid, brawl, shootout, escape, zoo, ambush, battle, safari1, safari2, safari3 :: CaveKind

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "Insulated storage area"
  , cfreq         = [ ("default random", 100), ("deep random", 80)
                    , ("caveRogue", 1) ]
  , cxsize        = fst normalLevelBound + 1
  , cysize        = snd normalLevelBound + 1
  , cgrid         = DiceXY (3 `d` 2) 4
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 15 10
  , cdarkChance   = 1 `d` 54 + 1 `dl` 20
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%2
  , cmaxVoid      = 1%6
  , cminStairDist = 15
  , cextraStairs  = 1 + 1 `d` 2
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 130  -- the maze requires time to explore
  , cactorFreq    = [("monster", 50), ("animal", 25), ("robot", 25)]
  , citemNum      = 6 `d` 5
  , citemFreq     = [("useful", 40), ("treasure", 60)]
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
  , cstairFreq      = [("staircase lift", 100)]
  , cdesc         = ""
  }
rogue2 = rogue
  { cfreq         = [("deep random", 20)]
  , cname         = "Dark storage area"
  , cdarkChance   = 51  -- all rooms dark
  , cnightChance  = 0  -- always day
  , cdesc         = "The lights had gone out."
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Recreational deck"
  , cfreq         = [("default random", 40), ("caveArena", 1)]
  , cgrid         = DiceXY (2 + 1 `d` 2) (1 `d` 3)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 6
  , cmaxPlaceSize = DiceXY 16 12
  , cdarkChance   = 49 + 1 `d` 10  -- almost all rooms dark (1 in 10 lit)
  -- Light is not too deadly, because not many obstructions and so
  -- foes visible from far away and few foes have ranged combat
  -- at shallow depth.
  , cnightChance  = 0  -- always day
  , cauxConnects  = 1
  , cmaxVoid      = 1%8
  , cextraStairs  = 1 `d` 3
  , chidden       = 0
  , cactorCoeff   = 100
  , cactorFreq    = [("monster", 25), ("animal", 70), ("robot", 5)]
  , citemNum      = 5 `d` 5  -- few rooms
  , citemFreq     = [("useful", 20), ("treasure", 40), ("any scroll", 40)]
  , cplaceFreq    = [("arena", 100)]
  , cpassable     = True
  , cdefTile      = "arenaSetLit"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cdesc         = ""
  }
arena2 = arena
  { cname         = "Casino"
  , cfreq         = [("deep random", 30)]
  , cdarkChance   = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth.
  , cnightChance  = 51  -- always night
  , citemNum      = 7 `d` 5  -- rare, so make it exciting
  , citemFreq     = [("useful", 20), ("treasure", 80)] -- lives up to the name
  , cdefTile      = "arenaSetDark"
  , cdesc         = ""
  }
laboratory = arena2
  { csymbol       = 'L'
  , cname         = "Laboratory"
  , cfreq         = [("deep random", 20), ("caveLaboratory", 1)]
  , cgrid         = DiceXY (2 `d` 2 + 7) 3
  , cminPlaceSize = DiceXY (3 `d` 2 + 4) 5
  , cdarkChance   = 1 `d` 54 + 1 `dl` 20  -- most rooms lit, to compensate for corridors
  , cnightChance  = 0  -- always day
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%10
  , cextraStairs  = 1 `d` 2
  , cdoorChance   = 1
  , copenChance   = 1%2
  , chidden       = 7
  , citemNum      = 7 `d` 5  -- reward difficulty
  , citemFreq     = [("useful", 20), ("treasure", 40), ("any vial", 40)]
  , cplaceFreq    = [("laboratory", 100)]
  , cpassable     = False
  , cdefTile      = "fillerWall"
  , cdarkCorTile  = "labTrailLit"  -- let lab smoke give off light always
  , clitCorTile   = "labTrailLit"
  , cstairFreq    = [("staircase lift", 100)]
  , cdesc         = "An experiment (or was it manufacturing?) had gone wrong here."
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Construction site"
  , cfreq         = [("caveEmpty", 1)]
  , cgrid         = DiceXY 1 1
  , cminPlaceSize = DiceXY 12 12
  , cmaxPlaceSize = DiceXY 48 32  -- favour large rooms
  , cdarkChance   = 1 `d` 100 + 1 `dl` 100
  , cnightChance  = 0  -- always day
  , cauxConnects  = 3%2
  , cminStairDist = 30
  , cmaxVoid      = 0  -- too few rooms to have void and fog common anyway
  , cextraStairs  = 1 `d` 2
  , cdoorChance   = 0
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 80
  , cactorFreq    = [("monster", 25), ("animal", 5), ("robot", 70)]
  , citemNum      = 5 `d` 5  -- few rooms
  , cplaceFreq    = [("empty", 100)]
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  , cstairFreq    = [("staircase", 50), ("staircase lift", 50)]
  , cdesc         = "Not much to see here yet."
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Flight hardware hub"
  , cfreq         = [("caveNoise", 1)]
  , cgrid         = DiceXY (2 + 1 `d` 3) 3
  , cminPlaceSize = DiceXY 8 5
  , cmaxPlaceSize = DiceXY 20 10
  , cdarkChance   = 51
  -- Light is deadly, because nowhere to hide and pillars enable spawning
  -- very close to heroes.
  , cnightChance  = 0  -- harder variant, but looks cheerful
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%100
  , cextraStairs  = 1 `d` 4
  , cdoorChance   = 1  -- to enable the doorlessWall hack and have no lit tiles
  , chidden       = 0
  , cactorCoeff   = 160  -- the maze requires time to explore
  , cactorFreq    = [("monster", 70), ("animal", 15), ("robot", 15)]
  , citemNum      = 7 `d` 5  -- an incentive to explore the labyrinth
  , cpassable     = True
  , cplaceFreq    = [("noise", 100)]
  , cdefTile      = "noiseSet"
  , couterFenceTile = "noise fence"  -- ensures no cut-off parts from collapsed
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  , cdesc         = ""
  }
noise2 = noise
  { cname         = "Power distribution hub"
  , cfreq         = [("caveNoise2", 1)]
  , cnightChance  = 51  -- easier variant, but looks sinister
  , citemNum      = 13 `d` 5  -- an incentive to explore the final labyrinth
  , cplaceFreq    = [("noise", 1), ("mine", 99)]
  , cstairFreq    = [("gated staircase", 100)]
  , cdesc         = ""
  }
bridge = rogue
  { csymbol       = 'B'
  , cname         = "Captain's bridge"
  , cfreq         = [("caveBridge", 1)]
  , cdarkChance   = 0  -- all rooms lit, for a gentle start
  , cextraStairs  = 1
  , cactorCoeff   = 200  -- it's quite deep already, so spawn slowly
  , cactorFreq    = [("animal", 100)]
  , citemNum      = 9 `d` 5  -- lure them in with loot
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  , cdesc         = "The bridge is gutted out and deserted. There are animal cries down below and ominous silence up above."
  }
shallow2rogue = rogue
  { cfreq         = [("shallow random 2", 50)]
  , cactorCoeff   = cactorCoeff rogue `div` 2  -- more difficult
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq rogue
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  , cdesc         = ""
  }
shallow2arena = arena
  { cfreq         = [("shallow random 2", 100)]
  , cactorCoeff   = cactorCoeff arena `div` 2
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq empty
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  , cdesc         = ""
  }
shallow2empty = empty
  { cfreq         = [("shallow random 2", 10)]
  , cactorCoeff   = cactorCoeff empty `div` 2
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq empty
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  , cdesc         = ""
  }
shallow1empty = shallow2empty  -- TODO: replace some rooms with oriels?
  { cname         = "Outermost deck"
  , cfreq         = [("outermost", 100)]
  , cactorCoeff   = 4  -- shallower than LH, so fewer immediate actors, so boost
  , cactorFreq    = [("animal", 3), ("robot", 2), ("immobile robot", 95)]
      -- The medbot faucets on lvl 1 act like HP resets. Needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. Items are not reset, even if they are bad, which provides
      -- enough of a continuity. The faucets on lvl 1 are not OP and can't be
      -- abused, because they spawn less and less often and also HP doesn't
      -- effectively accumulate over max.
  , couterFenceTile = "oriels fence"
  , cdesc         = "The black sky outside can be seen through the oriels."
  }
emptyExit = empty
  { cname         = "Shuttle servicing area"
  , cfreq         = [("caveEmptyExit", 1)]
  , cdarkCorTile  = "trailLit"  -- flavour
  , couterFenceTile = "noise fence"  -- for flavour
  , cescapeGroup  = Just "escape spaceship down"
  , cdesc         = "Empty husks and strewn entrails of small craft litter the hangar among neglected cranes and airlocks."
  }
raid = rogue
  { csymbol       = 'S'
  , cname         = "Triton City sewers"
  , cfreq         = [("caveRaid", 1)]
  , cdarkChance   = 0  -- all rooms lit, for a gentle start
  , cmaxVoid      = 1%10
  , cactorCoeff   = 500  -- deep level with no kit, so slow spawning
  , cactorFreq    = [("animal", 50), ("robot", 50)]
  , citemNum      = 6 `d` 8  -- just one level, hard enemies, treasure
  , citemFreq     = [("useful", 33), ("gem", 33), ("currency", 33)]
  , cescapeGroup  = Just "escape up"
  , cdesc         = ""
  }
brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
               -- and this scenario is not focused on ranged combat;
               -- also, sanctuaries against missiles in shadow under trees
  { csymbol       = 'S'
  , cname         = "Woodland biosphere"
  , cfreq         = [("caveBrawl", 1)]
  , cgrid         = DiceXY (2 `d` 2 + 2) 3
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkChance   = 51
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [("useful", 100)]
  , cplaceFreq    = [("brawl", 60), ("rogue", 40)]
  , cpassable     = True
  , cdefTile      = "brawlSetLit"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  , cdesc         = ""
  }
shootout = rogue  -- a scenario with strong missiles;
                  -- few solid tiles, but only translucent tiles or walkable
                  -- opaque tiles, to make scouting and sniping more interesting
                  -- and to avoid obstructing view too much, since this
                  -- scenario is about ranged combat at long range
  { csymbol       = 'S'
  , cname         = "Hydroponic farm"  -- still a neutral, offcial wording
  , cfreq         = [("caveShootout", 1)]
  , cgrid         = DiceXY (1 `d` 2 + 7) 3
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 3 4
  , cdarkChance   = 51
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 16
                      -- less items in inventory, more to be picked up,
                      -- to reward explorer and aggressor and punish camper
  , citemFreq     = [ ("useful", 30)
                    , ("any arrow", 400), ("harpoon", 300)
                    , ("any vial", 60) ]
                      -- Many consumable buffs are needed in symmetric maps
                      -- so that aggressor prepares them in advance and camper
                      -- needs to waste initial turns to buff for the defence.
  , cplaceFreq    = [("shootout", 100)]
  , cpassable     = True
  , cdefTile      = "shootoutSetLit"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  }
escape = rogue  -- a scenario with weak missiles, because heroes don't depend
                -- on them; dark, so solid obstacles are to hide from missiles,
                -- not view; obstacles are not lit, to frustrate the AI;
                -- lots of small lights to cross, to have some risks
  { csymbol       = 'E'
  , cname         = "Red Collar Bros den"  -- tension rises; non-official name
  , cfreq         = [("caveEscape", 1)]
  , cgrid         = DiceXY -- (2 `d` 2 + 3) 4  -- park, so lamps in lines
                           (2 `d` 2 + 6) 3   -- for now, to fit larger places
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 9 9  -- bias towards larger lamp areas
  , cdarkChance   = 51  -- colonnade rooms should always be dark
  , cnightChance  = 51  -- always night
  , cauxConnects  = 3%2
  , cmaxVoid      = 1%20
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [ ("useful", 30), ("treasure", 30), ("gem", 100)
                    , ("weak arrow", 500), ("harpoon", 400) ]
  , cplaceFreq    = [("park", 100)]
  , cpassable     = True
  , cdefTile      = "escapeSetDark"  -- different tiles, not burning yet
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cescapeGroup  = Just "escape outdoor down"
  , cdesc         = ""
  }
zoo = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'Z'
  , cname         = "Municipal zoo in flames"  -- non-official adjective
  , cfreq         = [("caveZoo", 1)]
  , cgrid         = DiceXY (2 `d` 2 + 6) 3
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 12 12
  , cdarkChance   = 51  -- always dark rooms
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 7 `d` 8
  , citemFreq     = [("useful", 100), ("light source", 1000)]
  , cplaceFreq    = [("zoo", 50)]
  , cpassable     = True
  , cdefTile      = "zooSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cdesc         = ""
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
  , cname         = "Ravaged spaceport"  -- non-official adjective
  , cfreq         = [("caveAmbush", 1)]
  , cgrid         = DiceXY -- (2 `d` 2 + 3) 4  -- spaceport, so lamps in lines
                           (2 `d` 2 + 5) 3   -- for now, to fit larger places
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 15 15  -- allow hangars
  , cdarkChance   = 51
  , cnightChance  = 51  -- always night
  , cauxConnects  = 3%2
  , cmaxVoid      = 1%20
  , cextraStairs  = 1
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [("useful", 30), ("any arrow", 400), ("harpoon", 300)]
  , cplaceFreq    = [("ambush", 100)]
  , cpassable     = True
  , cdefTile      = "ambushSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cdesc         = ""
  }
battle = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'B'
  , cname         = "Burning public park"
  , cfreq         = [("caveBattle", 1)]
  , cgrid         = DiceXY (2 `d` 2 + 1) 3
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
  , citemNum      = 5 `d` 8
  , citemFreq     = [("useful", 100), ("light source", 200)]
  , cplaceFreq    = [("battle", 50), ("rogue", 50)]
  , cpassable     = True
  , cdefTile      = "battleSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , couterFenceTile = "noise fence"  -- ensures no cut-off parts from collapsed
  , cdesc         = ""
  }
safari1 = brawl
  { cname         = "Hunam habitat"
  , cfreq         = [("caveSafari1", 1)]
  , cescapeGroup  = Nothing
  , cstairFreq    = [("staircase outdoor", 1)]
  , cdesc         = "\"DLC 1. Hunams scavenge in a forest in their usual disgusting way.\""
  }
safari2 = ambush  -- lamps instead of trees, but ok, it's only a simulation
  { cname         = "Deep into the jungle"
  , cfreq         = [("caveSafari2", 1)]
  , cstairFreq    = [("staircase outdoor", 1)]
  , cdesc         = "\"DLC 2. In the dark pure heart of the jungle noble animals roam freely.\""
  }
safari3 = zoo  -- glass rooms, but ok, it's only a simulation
  { cname         = "Jungle in flames"
  , cfreq         = [("caveSafari3", 1)]
  , cescapeGroup  = Just "escape outdoor down"
  , cstairFreq    = [("staircase outdoor", 1)]
  , cdesc         = "\"DLC 3. Jealous hunams set jungle on fire and flee.\""
  }
