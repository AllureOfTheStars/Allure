-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Cave properties.
module Content.CaveKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Data.Ratio

import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Content.CaveKind

content :: [CaveKind]
content =
  [rogue, rogue2, arena, arena2, laboratory, empty, noise, noise2, bridge, shallow2rogue, shallow2arena, shallow1empty, emptyExit, raid, brawl, shootout, escape, zoo, ambush, battle, safari1, safari2, safari3]

rogue,        rogue2, arena, arena2, laboratory, empty, noise, noise2, bridge, shallow2rogue, shallow2arena, shallow1empty, emptyExit, raid, brawl, shootout, escape, zoo, ambush, battle, safari1, safari2, safari3 :: CaveKind

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "Maintenance and storage"
  , cfreq         = [ ("default random", 100), ("deep random", 40)
                    , ("caveRogue", 1) ]
  , cXminSize     = 80
  , cYminSize     = 42
  , ccellSize     = DiceXY (2 `d` 4 + 10) (1 `d` 3 + 6)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) (1 `d` 2 + 5)
  , cmaxPlaceSize = DiceXY 16 40
  , cdarkChance   = 1 `d` 54 + 1 `dL` 20
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%2
  , cmaxVoid      = 1%8
  , cminStairDist = 30
  , cextraStairs  = 1 + 1 `d` 2
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 65  -- the maze requires time to explore
  , cactorFreq    = [("monster", 50), ("animal", 25), ("robot", 25)]
  , citemNum      = 8 `d` 4 + 10 - 10 `dL` 1  -- deep down quality over quantity
  , citemFreq     = [ ("common item", 40), ("treasure", 60)
                    , ("curious item", 10) ]
  , cplaceFreq    = [("rogue", 100)]
  , cpassable     = False
  , cdefTile        = "fillerWall"
  , cdarkCorTile    = "floorCorridorDark"
  , clitCorTile     = "floorCorridorLit"
  , cfillerTile     = "fillerWall"
  , cfenceTileN     = "basic outer fence"
  , cfenceTileE     = "habitat containment wall"
  , cfenceTileS     = "basic outer fence"
  , cfenceTileW     = "habitat containment wall"
  , cfenceApart     = False
  , clegendDarkTile = "legendDark"
  , clegendLitTile  = "legendLit"
  , cescapeFreq   = []
  , cstairFreq    = [ ("walled lift", 50), ("open lift", 50)
                    , ("tiny lift", 1) ]
  , cdesc         = "Winding tunnels stretch into the dark. Most of the area is crammed with tanks and cells of raw materials and machinery."
  }
rogue2 = rogue
  { cfreq         = [("deep random", 60)]
  , cname         = "Residential area"
  , cmaxPlaceSize = DiceXY 10 20  -- fewer big rooms
  , cdarkChance   = 51  -- all rooms dark
  , cnightChance  = 0  -- always day
  , cmaxVoid      = 1%4
  , cstairFreq    = [ ("walled staircase", 50), ("open staircase", 50)
                    , ("tiny staircase", 1) ]
  , cdesc         = "It's been powered down, except for emergency corridors, and many suites are depressurized and sealed."
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Recreational deck"
  , cfreq         = [("default random", 40), ("caveArena", 1)]
  , cXminSize     = 80
  , cYminSize     = 25
  , ccellSize     = DiceXY (3 `d` 3 + 17) (1 `d` 6 + 4)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 6
  , cmaxPlaceSize = DiceXY 16 16
  , cdarkChance   = 49 + 1 `d` 10  -- almost all rooms dark (1 in 10 lit)
  -- Light is not too deadly, because not many obstructions and so
  -- foes visible from far away and few foes have ranged combat
  -- at shallow depth.
  , cnightChance  = 0  -- always day
  , cauxConnects  = 1
  , cmaxVoid      = 1%8
  , cminStairDist = 20
  , cextraStairs  = 1 `d` 2
  , chidden       = 0
  , cactorCoeff   = 50  -- smallish level, but easy to view and plan
  , cactorFreq    = [("monster", 25), ("animal", 70), ("robot", 5)]
  , citemNum      = 7 `d` 3  -- few rooms
  , citemFreq     = [ ("common item", 20), ("treasure", 40), ("any scroll", 40)
                    , ("curious item", 10) ]
  , cplaceFreq    = [("arena", 100)]
  , cpassable     = True
  , cdefTile      = "arenaSetLit"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cstairFreq    = [ ("walled staircase", 20), ("closed staircase", 80)
                    , ("tiny staircase", 1) ]
  , cdesc         = ""
  }
arena2 = arena
  { cname         = "Casino"
  , cfreq         = [("deep random", 30)]
  , cXminSize     = 21
  , cYminSize     = 21
  , ccellSize     = DiceXY (3 `d` 3 + 17) (1 `d` 3 + 4)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 6
  , cmaxPlaceSize = DiceXY 16 12
  , cdarkChance   = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth.
  , cnightChance  = 51  -- always night
  , cminStairDist = 10
  , cactorCoeff   = 80  -- cramped, don't overcrowd
  , citemNum      = 7 `d` 3  -- rare, so make it exciting, by keeping items
  , citemFreq     = [ ("common item", 20)
                    , ("treasure", 80)  -- lives up to the name
                    , ("curious item", 20) ]
  , cdefTile      = "arenaSetDark"
  , cfenceTileN   = "habitat containment wall"  -- small cave
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "habitat containment wall"
  , cfenceTileW   = "habitat containment wall"
  , cdesc         = ""
  }
laboratory = arena2
  { csymbol       = 'L'
  , cname         = "Laboratory"
  , cfreq         = [("deep random", 30), ("caveLaboratory", 1)]
  , cXminSize     = 60
  , cYminSize     = 42
  , ccellSize     = DiceXY (1 `d` 2 + 5) (1 `d` 2 + 7)
  , cminPlaceSize = DiceXY 6 8
  , cmaxPlaceSize = DiceXY 10 40
  , cdarkChance   = 1 `d` 54 + 1 `dL` 20
      -- most rooms lit, to compensate for corridors
  , cnightChance  = 0  -- always day
  , cauxConnects  = 1%5
  , cmaxVoid      = 1%10
  , cminStairDist = 25
  , cdoorChance   = 1
  , copenChance   = 1%2
  , chidden       = 7
  , cactorCoeff   = 65  -- the maze requires time to explore
  , citemNum      = 7 `d` 4  -- reward difficulty
  , citemFreq     = [ ("common item", 20), ("treasure", 40), ("explosive", 40)
                    , ("curious item", 20) ]
  , cplaceFreq    = [("laboratory", 100)]
  , cpassable     = False
  , cdefTile      = "fillerWall"
  , cdarkCorTile  = "labTrailLit"  -- let lab smoke give off light always
  , clitCorTile   = "labTrailLit"
  , cstairFreq    = [ ("walled staircase", 50), ("open staircase", 50)
                    , ("tiny staircase", 1) ]
  , cdesc         = "Shattered glassware and the sharp scent of spilt chemicals show that something terrible happened here."
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Construction site"
  , cfreq         = []
  , ccellSize     = DiceXY (2 `d` 10 + 30) (3 `d` 2 + 13)
  , cminPlaceSize = DiceXY (2 `d` 2 + 10) (1 `d` 2 + 10)
  , cmaxPlaceSize = DiceXY 48 32  -- favour large rooms
  , cdarkChance   = 1 `d` 100 + 1 `dL` 100
  , cnightChance  = 0  -- always day
  , cauxConnects  = 3%2
  , cmaxVoid      = 0  -- too few rooms to have void and fog common anyway
  , cminStairDist = 40
  , cextraStairs  = 1
  , cdoorChance   = 0
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 40  -- easy to view and plan
  , cactorFreq    = [("monster", 25), ("animal", 5), ("robot", 70)]
  , citemNum      = 7 `d` 4  -- few rooms
  , cplaceFreq    = [("empty", 100)]
  , cpassable     = True
  , cdefTile      = "emptySet"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  , cfenceApart   = True  -- ensures no cut-off border airlocks and collapsed
  , cstairFreq    = [ ("walled lift", 20), ("closed lift", 80)
                    , ("tiny lift", 1) ]
  , cdesc         = "Not much to see here yet."
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Flight hardware hub"
  , cfreq         = [("caveNoise", 1)]
  , cXminSize     = 21
  , cYminSize     = 42
  , ccellSize     = DiceXY (3 `d` 5 + 12) 8
  , cminPlaceSize = DiceXY 8 7
  , cmaxPlaceSize = DiceXY 20 20
  , cdarkChance   = 51
  -- Light is deadly, because nowhere to hide and pillars enable spawning
  -- very close to heroes.
  , cnightChance  = 0  -- harder variant, but looks cheerful
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%100
  , cminStairDist = 15
  , cdoorChance   = 1  -- to enable the doorlessWall hack and have no lit tiles
  , chidden       = 0
  , cactorCoeff   = 80  -- the maze requires time to explore; also, small
  , cactorFreq    = [("monster", 70), ("animal", 15), ("robot", 15)]
  , citemNum      = 6 `d` 4  -- an incentive to explore the labyrinth
  , citemFreq     = [ ("common item", 20), ("treasure", 60), ("explosive", 20)
                    , ("curious item", 20) ]
  , cpassable     = True
  , cplaceFreq    = [("noise", 100)]
  , cdefTile      = "noiseSet"
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  , cstairFreq    = [ ("closed lift", 50), ("open lift", 50)
                    , ("tiny lift", 1) ]
  , cdesc         = ""
  }
noise2 = noise
  { cname         = "Power distribution hub"
  , cfreq         = [("caveNoise2", 1)]
  , cXminSize     = 32
  , cYminSize     = 42
  , cnightChance  = 51  -- easier variant, but looks sinister
  , citemNum      = 10 `d` 4  -- an incentive to explore the final labyrinth
  , citemFreq     = [ ("common item", 40), ("treasure", 60)
                    , ("curious item", 20) ]
  , cplaceFreq    = [("noise", 1), ("mine", 99)]
  , cstairFreq    = [("gated staircase", 100)]
  , cdesc         = ""
  }
bridge = rogue
  { csymbol       = 'B'
  , cname         = "Captain's bridge"
  , cfreq         = [("caveBridge", 1)]
  , cXminSize     = 30
  , cYminSize     = 30
  , ccellSize     = DiceXY (2 `d` 4 + 5) (1 `d` 2 + 5)
  , cminPlaceSize = DiceXY (2 `d` 2 + 3) (1 `d` 2 + 4)
  , cmaxPlaceSize = DiceXY 16 20
  , cminStairDist = 10
  , cdarkChance   = 0  -- all rooms lit, for a gentle start
  , cextraStairs  = 1
  , cactorCoeff   = 200  -- it's quite deep already, so spawn slowly
  , cactorFreq    = [("animal", 100)]
  , citemNum      = 8 `d` 3  -- lure them in with loot
  , citemFreq     = filter ((`notElem` ["treasure", "curious item"]) . fst)
                    $ citemFreq rogue
  , cfenceTileN   = "habitat containment wall"  -- cave isolated for safety
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "habitat containment wall"
  , cfenceTileW   = "habitat containment wall"
  , cdesc         = "The bridge is gutted out and nonoperational. There are animal cries down below and ominous silence up above."
  }
shallow2blurb :: Text
shallow2blurb = "This close to the outer level, residence is not permitted and doors are sturdier, to contain a possible micro-meteorite breach. The area is not closed, though, because some passengers can't live without regularly looking at the void and the light of distant suns and planets through a glass window, as opposed to electronic display and glass. Animals appear to share the fascination, or perhaps they crave the increased gravity, nearly Earth-like, unlike elsewhere on the ship."
shallow2rogue = rogue
  { cfreq         = [("shallow random 2", 50)]
  , cXminSize     = 60
  , cYminSize     = 37
  , cactorCoeff   = cactorCoeff rogue `div` 2  -- more difficult
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq rogue
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq rogue
  , cdesc         = shallow2blurb
  }
shallow2arena = arena
  { cfreq         = [("shallow random 2", 100)]
  , cXminSize     = 80
  , cYminSize     = 30
  , cactorCoeff   = cactorCoeff arena `div` 2  -- more difficult
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq arena
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq arena
  , cdesc         = shallow2blurb
  }
shallow1empty = empty
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
  , citemFreq     = filter ((/= "treasure") . fst) $ citemFreq empty
  , cfenceTileN   = "oriels fence"
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "empty airlock fence"
  , cfenceTileW   = "habitat containment wall"
  , cdesc         = "The black sky outside sucks light through the oriels and airlock glass. This is the main pressurized cargo bay and storage, with the only other docking hub for small craft somewhere among the giant spaceship's uppermost levels. You can't see from afar the shuttle you left engaged to one of the few free airlocks covered in guano. Water treatment basins and series of hanging and stacked tanks double as radiation shields. Hoses writhe on the floor and dangle in thick knots from the ceiling."
      -- E and W sides are borders with other level sections, so no oriels.
      -- TODO: water-filled wall-less rooms, as soon as there are water tiles
      -- TODO: exclusively water-liking animals, when there is enough; plants
  }
emptyExit = empty
  { cname         = "Shuttle servicing level"
  , cfreq         = [("caveEmptyExit", 1)]
  , cdefTile      = "emptyExitSet"
  , cdarkCorTile  = "transport route"
  , clitCorTile   = "transport route"
  , cfenceTileN   = "basic outer fence"
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "airlock fence"
  , cfenceTileW   = "habitat containment wall"
  , cescapeFreq   = [("escape spaceship down", 1)]
  , cstairFreq    = [("gated staircase", 100)]
  , cdesc         = "Empty husks and strewn entrails of small craft litter the hangar among cranes and welding machines. Distant engines can be seen to the rear of the spaceship through oriels and airlocks of all sizes."
      -- E and W sides are borders with other level sections, so no oriels.
      -- The meteor shield towards N is not punctured here, because
      -- the cargo bay is too thick here, near the axis of the ship.
  }
raid = rogue
  { csymbol       = 'S'
  , cname         = "Triton City sewers"
  , cfreq         = [("caveRaid", 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 4 + 6) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkChance   = 0  -- all rooms lit, for a gentle start
  , cmaxVoid      = 1%10
  , cextraStairs  = 0
  , cactorCoeff   = 250  -- deep level with no kit, so slow spawning
  , cactorFreq    = [("animal", 50), ("robot", 50)]
  , citemNum      = 6 `d` 6  -- just one level, hard enemies, treasure
  , citemFreq     = [("common item", 100), ("currency", 500)]
  , cescapeFreq   = [("escape up", 1)]
  , cstairFreq    = []
  , cdesc         = ""
  }
brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
               -- and this scenario is not focused on ranged combat;
               -- also, sanctuaries against missiles in shadow under trees
  { csymbol       = 'S'
  , cname         = "Woodland biosphere"
  , cfreq         = [("caveBrawl", 1)]
  , cXminSize     = 60
  , cYminSize     = 30
  , ccellSize     = DiceXY (2 `d` 5 + 8) 7
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkChance   = 51
  , cnightChance  = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 6
  , citemFreq     = [("common item", 100)]
  , cplaceFreq    = [("brawl", 50), ("laboratory", 50)]
  , cpassable     = True
  , cdefTile      = "brawlSetLit"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  , cstairFreq    = []
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
  , ccellSize     = DiceXY (1 `d` 2 + 8) 7
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 3 4
  , cdarkChance   = 51
  , cnightChance  = 0
  , cauxConnects  = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 16
                      -- less items in inventory, more to be picked up,
                      -- to reward explorer and aggressor and punish camper
  , citemFreq     = [ ("common item", 30)
                    , ("any arrow", 400), ("harpoon", 300), ("explosive", 50) ]
                      -- Many consumable buffs are needed in symmetric maps
                      -- so that aggressor prepares them in advance and camper
                      -- needs to waste initial turns to buff for the defence.
  , cplaceFreq    = [("shootout", 100)]
  , cpassable     = True
  , cdefTile      = "shootoutSetLit"
  , cdarkCorTile  = "floorArenaLit"
  , clitCorTile   = "floorArenaLit"
  , cstairFreq    = []
  , cdesc         = ""
  }
escape = rogue  -- a scenario with weak missiles, because heroes don't depend
                -- on them; dark, so solid obstacles are to hide from missiles,
                -- not view; obstacles are not lit, to frustrate the AI;
                -- lots of small lights to cross, to have some risks
  { csymbol       = 'E'
  , cname         = "Red Collar Bros den"  -- tension rises; non-official name
  , cfreq         = [("caveEscape", 1)]
  , ccellSize     = DiceXY (1 `d` 3 + 7) 7
  , cminPlaceSize = DiceXY 5 3
  , cmaxPlaceSize = DiceXY 9 9  -- bias towards larger lamp areas
  , cdarkChance   = 51  -- colonnade rooms should always be dark
  , cnightChance  = 51  -- always night
  , cauxConnects  = 2
  , cmaxVoid      = 1%100
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 6 `d` 8
  , citemFreq     = [ ("common item", 30), ("gem", 150)
                    , ("weak arrow", 500), ("harpoon", 400)
                    , ("explosive", 100) ]
  , cplaceFreq    = [("escape", 100)]
  , cpassable     = True
  , cdefTile      = "escapeSetDark"  -- different tiles, not burning yet
  , cdarkCorTile  = "alarmingTrailLit"  -- let trails give off light
  , clitCorTile   = "alarmingTrailLit"
  , cescapeFreq   = [("escape outdoor down", 1)]
  , cstairFreq    = []
  , cdesc         = ""
  }
zoo = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'Z'
  , cname         = "Municipal zoo in flames"  -- non-official adjective
  , cfreq         = [("caveZoo", 1)]
  , ccellSize     = DiceXY (1 `d` 3 + 7) 6
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 12 5
  , cdarkChance   = 51  -- always dark rooms
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 7%10
  , copenChance   = 9%10
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 7 `d` 8
  , citemFreq     = [("common item", 100), ("light source", 1000)]
  , cplaceFreq    = [("zoo", 50)]
  , cpassable     = True
  , cdefTile      = "zooSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cstairFreq    = []
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
  , ccellSize     = DiceXY (1 `d` 4 + 7) 7
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 15 15  -- allow hangars
  , cdarkChance   = 51  -- colonnade rooms should always be dark
  , cnightChance  = 51  -- always night
  , cauxConnects  = 3%2
  , cmaxVoid      = 1%20
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [ ("common item", 30)
                    , ("any arrow", 400), ("harpoon", 300), ("explosive", 50) ]
  , cplaceFreq    = [("ambush", 100)]
  , cpassable     = True
  , cdefTile      = "ambushSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cstairFreq    = []
  , cdesc         = ""
  }
battle = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'B'
  , cname         = "Burning public park"
  , cfreq         = [("caveBattle", 1)]
  , ccellSize     = DiceXY (5 `d` 3 + 11) 7
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 9 7
  , cdarkChance   = 0
  , cnightChance  = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [("common item", 100), ("light source", 200)]
  , cplaceFreq    = [("battle", 50), ("rogue", 50)]
  , cpassable     = True
  , cdefTile      = "battleSet"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cstairFreq    = []
  , cdesc         = ""
  }
safari1 = brawl
  { cname         = "Hunam habitat"
  , cfreq         = [("caveSafari1", 1)]
  , cminPlaceSize = DiceXY 5 3
  , cextraStairs  = 1
  , cstairFreq    = [("staircase outdoor", 1)]
  , cdesc         = "\"DLC 1. Hunams scavenge in a forest in their usual disgusting way.\""
  }
safari2 = ambush  -- lamps instead of trees, but ok, it's only a simulation
  { cname         = "Deep into the jungle"
  , cfreq         = [("caveSafari2", 1)]
  , cminPlaceSize = DiceXY 5 3
  , cextraStairs  = 1
  , cstairFreq    = [("staircase outdoor", 1)]
  , cdesc         = "\"DLC 2. In the dark pure heart of the jungle noble animals roam freely.\""
  }
safari3 = zoo  -- glass rooms, but ok, it's only a simulation
  { cname         = "Jungle in flames"
  , cfreq         = [("caveSafari3", 1)]
  , cminPlaceSize = DiceXY 5 4
  , cescapeFreq   = [("escape outdoor down", 1)]
  , cextraStairs  = 1
  , cstairFreq    = [("staircase outdoor", 1)]
  , cdesc         = "\"DLC 3. Jealous hunams set jungle on fire and flee.\""
  }
