-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
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
  [rogue, residential, arena, casino, museum, laboratory, noise, power, empty, exit, outermost, bridge, shallowRogue, raid, brawl, shootout, hunt, escape, zoo, ambush, battle, safari1, safari2, safari3]

rogue,    residential, arena, casino, museum, laboratory, noise, power, empty, exit, outermost, bridge, shallowRogue, raid, brawl, shootout, hunt, escape, zoo, ambush, battle, safari1, safari2, safari3 :: CaveKind

-- * On-ship "caves", that is, decks, most of mediocre height and size

rogue = CaveKind
  { csymbol       = 'R'
  , cname         = "Maintenance and storage"
  , cfreq         = [("default random", 100), ("caveRogue", 1)]
  , cXminSize     = 80
  , cYminSize     = 42
  , ccellSize     = DiceXY (2 `d` 4 + 10) (1 `d` 3 + 6)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) (1 `d` 2 + 5)  -- sometimes merge vert.
  , cmaxPlaceSize = DiceXY 16 40  -- often maximize vertically
  , cdarkOdds     = 1 `d` 54 + 1 `dL` 20
      -- most rooms lit, to compensate for dark corridors
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%2
  , cmaxVoid      = 1%8
  , cminStairDist = 30
  , cextraStairs  = 1 `d` 2
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 50  -- the maze requires time to explore
  , cactorFreq    = [("monster", 50), ("animal", 20), ("robot", 30)]
  , citemNum      = 8 `d` 4 + 20 - 20 `dL` 1  -- deep down quality over quantity
  , citemFreq     = [ ("common item", 40), ("treasure", 60)
                    , ("curious item", 10) ]
  , cplaceFreq    = [("rogue", 1)]
  , cpassable     = False
  , labyrinth     = False
  , cdefTile      = "rogueSet"
  , cdarkCorTile  = "floorCorridorDark"
  , clitCorTile   = "floorCorridorLit"
  , cwallTile     = "trappableWall"
  , ccornerTile   = "fillerWall"
  , cfenceTileN   = "basic outer fence"
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "basic outer fence"
  , cfenceTileW   = "habitat containment wall"
  , cfenceApart   = False
  , clegendDarkTile = "legendDark"
  , clegendLitTile  = "legendLit"
  , cescapeFreq   = []
  , cstairFreq    = [ ("walled lift", 50), ("open lift", 50)
                    , ("tiny lift", 1) ]
  , cstairAllowed = [ ("walled staircase", 50), ("open staircase", 50)
                    , ("tiny staircase", 1) ]
  , cdesc         = "Winding tunnels stretch into the dark. A few areas are passable but the remainder is packed with tanks and cells of raw materials and machinery."
  }
residential = rogue
  { cfreq         = [("default random", 50), ("caveResidential", 1)]
  , cname         = "Residential area"
  , cmaxPlaceSize = DiceXY 14 20  -- fewer big rooms
  , cdarkOdds     = 51  -- all rooms dark
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 1%10  -- differentiate; bare skeleton feel; long span paths
  , cmaxVoid      = 1%4
  , cextraStairs  = 1 + 1 `d` 2
  , cdefTile      = "fillerWall"
  , cstairFreq    = [ ("walled staircase", 50), ("open staircase", 50)
                    , ("tiny staircase", 1) ]
  , cstairAllowed = [ ("walled lift", 50), ("open lift", 50)
                    , ("tiny lift", 1) ]
  , cdesc         = "The area has been powered down, except for emergency corridors. Many suites are depressurized and sealed."
  }
arena = rogue
  { csymbol       = 'A'
  , cname         = "Recreational deck"
  , cfreq         = [("caveArena", 1)]
  , cXminSize     = 80
  , cYminSize     = 25
  , ccellSize     = DiceXY (3 `d` 3 + 17) (1 `d` 3 + 5)
  , cminPlaceSize = DiceXY 10 15  -- merge vertically
  , cmaxPlaceSize = DiceXY 25 40  -- often maximize vertically
  , cdarkOdds     = 49 + 1 `d` 10  -- almost all rooms dark (1 in 10 lit)
  -- Light is not too deadly, because not many obstructions and so
  -- foes visible from far away and few foes have ranged combat
  -- at shallow depth.
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 1
  , cmaxVoid      = 1%20
  , cminStairDist = 20
  , cextraStairs  = 1 `d` 2
  , chidden       = 0
  , cactorCoeff   = 40  -- smallish level, but easy to view and plan
  , cactorFreq    = [ ("monster", 50), ("animal", 70), ("robot", 5)
                    , ("aquatic", 10) ]
  , citemNum      = 7 `d` 4  -- few rooms
  , citemFreq     = [ ("common item", 20), ("treasure", 40), ("any scroll", 40)
                    , ("explosive", 40), ("curious item", 20) ]
  , cplaceFreq    = [("arena", 1)]
  , cpassable     = True
  , cdefTile      = "arenaSetLit"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"  -- may be rolled different than the above
  , cwallTile     = "openableWall"
  , cstairFreq    = [ ("walled lift", 20), ("closed lift", 80)
                    , ("tiny lift", 1) ]
  , cstairAllowed = [ ("walled staircase", 20), ("closed staircase", 80)
                    , ("tiny staircase", 1) ]
  , cdesc         = "Debris litters the wide streets and all the stalls have their shutters down."
  }
casino = arena
  { cname         = "Casino"
  , cfreq         = [("caveCasino", 1)]
  , cXminSize     = 21
  , cYminSize     = 21
  , cdarkOdds     = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth, though level too small.
  , cnightOdds    = 51  -- always night
  , cminStairDist = 10
  , cactorCoeff   = 70  -- cramped, don't overcrowd
  , cactorFreq    = [("monster", 50), ("animal", 25), ("robot", 50)]
  , citemNum      = 7 `d` 3  -- rare, so make it exciting, by keeping many items
  , citemFreq     = [ ("common item", 20)
                    , ("treasure", 80)  -- lives up to the name
                    , ("curious item", 40) ]
  , cdefTile      = "arenaSetDark"
  , cfenceTileN   = "habitat containment wall"  -- small cave
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "habitat containment wall"
  , cfenceTileW   = "habitat containment wall"
  , cdesc         = "No longer filled with hollow-eyed gamblers; more dangerous things now lurk in the dark."
  }
museum = arena
  { cname         = "Museum"
  , cfreq         = [("caveMuseum", 1)]
  , cXminSize     = 25
  , cYminSize     = 25
  , cdarkOdds     = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth, though level too small.
  , cnightOdds    = 51  -- always night
  , cminStairDist = 10
  , cactorCoeff   = 70  -- cramped, don't overcrowd
  , cactorFreq    = [("monster", 100), ("animal", 25), ("robot", 25)]
  , citemNum      = 7 `d` 4  -- rare, so make it exciting
  , citemFreq     = [ ("common item", 20)
                    , ("treasure", 40)
                    , ("curious item", 40)
                    , ("museum", 100) ]  -- lives up to the name
  , cplaceFreq    = [("museum", 1)]
  , cdefTile      = "museumSetDark"
  , cfenceTileN   = "habitat containment wall"  -- small cave
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "habitat containment wall"
  , cfenceTileW   = "habitat containment wall"
  , cdesc         = "History has shown that museal treasures are safer in space than anywhere on Earth. Cruise passengers are also more likely to visit exhibitions, even if that's captive audience effect to some extent. That rarely applies to spaceship crew and yet museum security has a particularly keen eye for the working men. Quite often a museum is the only place within millions of kilometers to house a desperately needed tool, old but sturdy beyond anything a 3D printer could produce."
  }
laboratory = rogue
  { csymbol       = 'L'
  , cname         = "Laboratory"
  , cfreq         = [("caveLaboratory", 1)]
  , cXminSize     = 60
  , cYminSize     = 42
  , ccellSize     = DiceXY (1 `d` 2 + 5) (1 `d` 2 + 7)
  , cminPlaceSize = DiceXY 6 8  -- merge, usually vertically
  , cmaxPlaceSize = DiceXY 12 40  -- often maximize vertically
  , cnightOdds    = 0  -- always day so that the corridor smoke is lit
  , cauxConnects  = 1%5
  , cmaxVoid      = 1%10
  , cminStairDist = 25
  , cextraStairs  = 1 `d` 2
  , cdoorChance   = 1
  , copenChance   = 1%2
  , cactorFreq    = [ ("monster", 50), ("animal", 70), ("robot", 5)
                    , ("aquatic", 10) ]
  , citemNum      = 7 `d` 5  -- reward difficulty
  , citemFreq     = [ ("common item", 20), ("treasure", 40), ("potion", 40)
                    , ("curious item", 40) ]
  , cplaceFreq    = [("laboratory", 1)]
  , cdefTile      = "fillerWall"
  , cdarkCorTile  = "labTrailLit"  -- let lab smoke give off light always
  , clitCorTile   = "labTrailLit"
  , cstairFreq    = [ ("decontaminating walled staircase", 50)
                    , ("decontaminating open staircase", 50)
                    , ("decontaminating tiny staircase", 1) ]
      -- In lone wolf challenge, the player better summoned or dominated
      -- any helpers by this point. If not, good luck fighting bare-handed.
  , cstairAllowed = [ ("decontaminating walled lift", 50)
                    , ("decontaminating open lift", 50)
                    , ("decontaminating tiny lift", 1) ]
  , cdesc         = "Shattered glassware and the sharp scent of spilt chemicals show that something terrible happened here."
  }
noise = rogue
  { csymbol       = 'N'
  , cname         = "Computing hardware hub"
  , cfreq         = [("caveNoise", 1)]
  , cXminSize     = 21
  , cYminSize     = 42
  , ccellSize     = DiceXY (3 `d` 5 + 12) 8
  , cminPlaceSize = DiceXY 8 7  -- often merge vertically
  , cmaxPlaceSize = DiceXY 20 20
  , cdarkOdds     = 51
  -- Light is deadly, because nowhere to hide and pillars enable spawning
  -- very close to heroes.
  , cnightOdds    = 0  -- harder variant, but looks cheerful
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%100
  , cminStairDist = 15
  , cextraStairs  = 1
  , cdoorChance   = 1  -- to enable the doorlessWall hack
  , chidden       = 0
  , cactorCoeff   = 70  -- the maze requires time to explore; also, small
  , cactorFreq    = [("monster", 100), ("animal", 5), ("robot", 25)]
  , citemNum      = 8 `d` 5  -- an incentive to explore the labyrinth
  , citemFreq     = [ ("common item", 20), ("treasure", 60), ("explosive", 20)
                    , ("curious item", 30) ]
  , cpassable     = True
  , labyrinth     = True
  , cplaceFreq    = [("noise", 1)]
  , cdefTile      = "noiseSetLit"
  , cwallTile     = "openableWall"
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cdarkCorTile  = "damp floor Dark"
  , clitCorTile   = "damp floor Lit"
  , cstairFreq    = [ ("closed staircase", 50), ("open staircase", 50)
                    , ("tiny staircase", 1) ]
  , cstairAllowed = [ ("closed lift", 50), ("open lift", 50)
                    , ("tiny lift", 1) ]
  , cdesc         = "Several machines still function, processors whirring through routines scheduled by dead men."
  }
power = noise
  { cname         = "Power distribution hub"
  , cfreq         = [("cavePower", 1)]
  , cXminSize     = 32
  , cYminSize     = 42
  , cnightOdds    = 51  -- easier variant, but looks sinister
  , cextraStairs  = 2
  , citemNum      = 8 `d` 5  -- an incentive to explore the final labyrinth
  , citemFreq     = [ ("common item", 20), ("gem", 80), ("curious item", 30) ]
                      -- can't be "valuable" or template items generated
  , cdefTile      = "powerSetDark"
  , cdarkCorTile  = "oily floor Dark"
  , clitCorTile   = "oily floor Lit"
  , cstairFreq    = [ ("gated closed staircase", 50)
                    , ("gated open staircase", 50)
                    , ("gated tiny staircase", 1) ]
  , cstairAllowed = [ ("gated closed lift", 50)
                    , ("gated open lift", 50)
                    , ("gated tiny lift", 1) ]
  , cdesc         = "A trickle of energy flows through a hub that could power a city. The air is warm and carries organic stench. Once in a while a young animal scurries across a lit patch of ground, pouncing in low gravity."
  }
empty = rogue
  { csymbol       = 'E'
  , cname         = "Construction site"
  , cfreq         = []
  , ccellSize     = DiceXY (2 `d` 8 + 14) 16
  , cminPlaceSize = DiceXY 9 9  -- normally don't merge
  , cmaxPlaceSize = DiceXY 50 20  -- often maximize horizontally
  , cdarkOdds     = 1 `d` 100 + 1 `dL` 100
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 3%2
  , cmaxVoid      = 0  -- too few rooms to have void and fog common anyway
  , cminStairDist = 40
  , cextraStairs  = 1
  , cdoorChance   = 1  -- to enable the doorlessWall hack
  , chidden       = 0
  , cactorCoeff   = 40  -- easy to view and plan
  , cactorFreq    = [("monster", 10), ("animal", 5), ("robot", 85)]
  , citemNum      = 7 `d` 4  -- few rooms
  , cplaceFreq    = [("empty", 1)]
  , cpassable     = True
  , cdefTile      = "emptySetLit"
  , cdarkCorTile  = "floorArenaDark"
  , clitCorTile   = "floorArenaLit"
  , cwallTile     = "openableWall"
  , cfenceApart   = True  -- ensures no cut-off border airlocks and tanks
  , cstairFreq    = [ ("walled lift", 20), ("closed lift", 80)
                    , ("tiny lift", 1) ]
  , cstairAllowed = [ ("walled staircase", 20), ("closed staircase", 80)
                    , ("tiny staircase", 1) ]
  , cdesc         = "Not much to see here yet."
  }
exit = empty
  { cname         = "Shuttle servicing level"
  , cfreq         = [("caveExit", 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 20) 16
  , cmaxPlaceSize = DiceXY 25 20
  , cdarkOdds     = 51  -- all dark to compensate for the always lit shuttles
  , cextraStairs  = 2
  , cplaceFreq    = [("exit", 1)]
  , cdefTile      = "exitSetLit"
  , cdarkCorTile  = "transport route"
  , clitCorTile   = "transport route"
  , cfenceTileN   = "basic outer fence"
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "airlock fence"
  , cfenceTileW   = "habitat containment wall"
  , cescapeFreq   = [("escape spaceship down", 1)]
  , cstairFreq    = [("walled lift", 20), ("tiny lift", 1)]  -- for miniboss
  , cstairAllowed = [ ("decontaminating walled staircase", 20)
                    , ("decontaminating tiny staircase", 1) ]  -- for miniboss
  , cdesc         = "Empty husks and strewn entrails of small craft litter the hangar among cranes and welding machines. Distant main fusion thruster array can be seen to the rear of the spaceship through oriels and airlocks of all sizes."
      -- E and W sides are borders with other level sections, so no oriels.
      -- The meteor shield towards N is not punctured here, because
      -- the cargo bay is too thick here, near the axis of the ship.
  }
outermost = empty
  { cname         = "Outermost deck"
  , cfreq         = [("caveOutermost", 100)]
  , cactorCoeff   = 5  -- shallower than LH, so fewer immediate actors, so boost
  , cactorFreq    = [ ("animal", 3), ("robot", 1)
                    , ("immobile robot", 90), ("immobile animal", 2)
                    , ("aquatic animal", 2) ]  -- ("aquatic robot", 2)
      -- The medbot faucets on lvl 1 act like HP resets. Needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. Items are not reset, even if they are bad, which provides
      -- enough of a continuity. The faucets on lvl 1 are not OP and can't be
      -- abused, because they spawn less and less often and also HP doesn't
      -- effectively accumulate over max.
  , citemFreq     = ("starting weapon", 20)
                    : filter ((/= "treasure") . fst) (citemFreq empty)
  , cfenceTileN   = "oriels fence"
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "empty airlock fence"
  , cfenceTileW   = "habitat containment wall"
  , cdesc         = "The black sky outside sucks light through the oriel and airlock glass in the walls and floor of this outermost level. The mucky floor itself looks misleading straight, its curvature noticeable only across the whole extent of the hull section. Overflowing water treatment basins and series of hanging and stacked tanks double as radiation shields. Hoses writhe on the ground and dangle in thick knots from the ceiling. This deck is the main pressurized cargo bay and storage, with the only other docking hub for small craft somewhere among the giant spaceship's upper levels. You can't see the shuttle you left engaged to airlock clamps outside. You remember with disgust the airlock's inner doors smeared with guano."
      -- E and W sides are borders with other level sections, so no oriels.
  }
bridge = rogue
  { csymbol       = 'B'
  , cname         = "Captain's bridge"
  , cfreq         = [("caveBridge", 1)]
  , cXminSize     = 30
  , cYminSize     = 30
  , ccellSize     = DiceXY (2 `d` 4 + 5) (1 `d` 2 + 5)
  , cminPlaceSize = DiceXY (2 `d` 2 + 3) (1 `d` 2 + 4)  -- sometimes merge all
  , cmaxPlaceSize = DiceXY 16 20
  , cminStairDist = 10
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cauxConnects  = 2  -- few rooms, so many corridors
  , cextraStairs  = 1
  , cactorCoeff   = 300  -- it's quite deep already, so spawn slowly;
                         -- initially best for sleeping, then all catch up
  , cactorFreq    = [("animal", 100)]
  , citemNum      = 8 `d` 3  -- lure them in with loot
  , citemFreq     = filter ((`notElem` ["treasure", "curious item"]) . fst)
                    $ citemFreq rogue
  , cdefTile      = "fillerWall"
  , cfenceTileN   = "habitat containment wall"  -- cave isolated for safety
  , cfenceTileE   = "habitat containment wall"
  , cfenceTileS   = "habitat containment wall"
  , cfenceTileW   = "habitat containment wall"
  , cstairFreq    = [ ("welded walled lift", 50)
                    , ("welded open lift", 50)
                    , ("welded tiny lift", 1) ]
  , cstairAllowed = [ ("welded walled staircase", 50)
                    , ("welded open staircase", 50)
                    , ("welded tiny staircase", 1) ]
  , cdesc         = "The bridge is gutted out and nonoperational. There are animal cries down below and ominous silence up above."
  }
shallowRogue = rogue
  { cfreq         = [("caveShallowRogue", 100)]
  , cXminSize     = 60
  , cYminSize     = 37
  , cextraStairs  = 2
  , cactorCoeff   = 40  -- more difficult
  , cactorFreq    = filter ((/= "monster") . fst) $ cactorFreq rogue
  , citemNum      = 8 `d` 4
  , citemFreq     = ("starting weapon", 20)
                    : filter ((/= "treasure") . fst) (citemFreq rogue)
  , cdesc         = "This close to the outermost deck, residence is not permitted and doors are sturdier, to contain a theoretically possible micro-meteorite breach. The entry is not closed off, though, because some passengers can't live without regularly looking at the void and the sharp points of stars and planets through the reinforced glass of oriels, as opposed to the glass of electronic displays. Each minute a dusky melancholic light of the distant Sun tries to squeeze in, but is pushed out by artificial lighting. Animals appear to share the fascination, perhaps rather fond of the increased gravity, nearly Earth-like, unlike elsewhere on the ship."
  }

-- * "Caves" on various celestial bodies (including, but not limited to, moons,
--   with virtually no story-wise limits wrt height and size

raid = rogue
  { csymbol       = 'S'
  , cname         = "Triton City sewers"
  , cfreq         = [("caveRaid", 1)]
  , cXminSize     = 50
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 2 + 7) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5  -- sometimes merge all
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cmaxVoid      = 1%10
  , cdefTile      = "fillerWall"
  , cextraStairs  = 0
  , cactorCoeff   = 250  -- deep level with no kit, so slow spawning
  , cactorFreq    = [("animal", 50), ("robot", 50)]
  , citemNum      = 6 `d` 6  -- just one level, hard enemies, treasure
  , citemFreq     = [("common item", 100), ("currency", 500)]
  , cescapeFreq   = [("escape up", 1)]
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Mold spreads across the walls and scuttling sounds can be heard in the distance."
  }
brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
               -- and this scenario is not focused on ranged combat;
               -- also, sanctuaries against missiles in shadow under trees
  { csymbol       = 'S'
  , cname         = "Woodland biosphere"
  , cfreq         = [("caveBrawl", 1)]
  , cXminSize     = 60
  , cYminSize     = 30
  , ccellSize     = DiceXY (2 `d` 5 + 5) 7
  , cminPlaceSize = DiceXY 3 3  -- rarely merge vertically
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 6
  , citemFreq     = [("common item", 100)]
  , cplaceFreq    = [("brawl", 1)]
  , cpassable     = True
  , cdefTile      = "brawlSetLit"
  , cdarkCorTile  = "dirt Lit"
  , clitCorTile   = "dirt Lit"
  , cwallTile     = "openableWall"
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Shadows pool under the trees and leaves crunch underfoot."
  }
shootout = rogue  -- a scenario with strong missiles;
                  -- few solid tiles, but only translucent tiles or walkable
                  -- opaque tiles, to make scouting and sniping more interesting
                  -- and to avoid obstructing view too much, since this
                  -- scenario is about ranged combat at long range
  { csymbol       = 'S'
  , cname         = "Hydroponic farm"  -- still a neutral, official wording
  , cfreq         = [("caveShootout", 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 5) 6
  , cminPlaceSize = DiceXY 3 3  -- rarely merge vertically
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkOdds     = 51
  , cnightOdds    = 0
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
  , cplaceFreq    = [("shootout", 1)]
  , cpassable     = True
  , cdefTile      = "shootoutSetLit"
  , cdarkCorTile  = "dirt Lit"
  , clitCorTile   = "dirt Lit"
  , cwallTile     = "openableWall"
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Once so carefully curated, the planting beds are now overgrown and chocked with weeds."  -- when we know what fits the plot, suggest it here; e.g., there are better way to earn money, shortage of manpower, dangerous area of the city, mismanagement by the city council
  }
hunt = rogue  -- a scenario with strong missiles for ranged and shade for melee
  { csymbol       = 'H'
  , cname         = "Swamp biosphere"
  , cfreq         = [("caveHunt", 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 5) 6
  , cminPlaceSize = DiceXY 3 3  -- rarely merge vertically
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cauxConnects  = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 10
  , citemFreq     = [ ("common item", 30)
                    , ("any arrow", 400), ("harpoon", 300), ("explosive", 50) ]
  , cplaceFreq    = [("brawl", 50), ("shootout", 100)]
  , cpassable     = True
  , cdefTile      = "huntSetLit"  -- much more water than in shootoutSetLit
  , cdarkCorTile  = "dirt Lit"
  , clitCorTile   = "dirt Lit"
  , cwallTile     = "openableWall"
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Mangrove trees and murky water, inspired by a habitat now eradicated from Earth."
  }
escape = rogue  -- a scenario with weak missiles, because heroes don't depend
                -- on them; dark, so solid obstacles are to hide from missiles,
                -- not view; obstacles are not lit, to frustrate the AI;
                -- lots of small lights to cross, to have some risks
  { csymbol       = 'E'
  , cname         = "Red Collar Bros den"  -- tension rises; non-official name
  , cfreq         = [("caveEscape", 1)]
  , ccellSize     = DiceXY (1 `d` 3 + 6) 7
  , cminPlaceSize = DiceXY 5 4  -- rarely merge
  , cmaxPlaceSize = DiceXY 9 9  -- bias towards larger lamp areas
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 2  -- many lit trails, so easy to aim
  , cmaxVoid      = 1%100
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 6 `d` 8
  , citemFreq     = [ ("common item", 30), ("gem", 150)
                    , ("weak arrow", 500), ("harpoon", 400)
                    , ("explosive", 100) ]
  , cplaceFreq    = [("escape", 1)]
  , cpassable     = True
  , cdefTile      = "escapeSetDark"
  , cdarkCorTile  = "alarmingTrailLit"  -- let trails give off light
  , clitCorTile   = "alarmingTrailLit"
  , cwallTile     = "openableWall"
  , cescapeFreq   = [("escape outdoor down", 1)]
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Graffiti scrawls across the walls and the heavy scents of stimulants hang in the air."
  }
zoo = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'Z'
  , cname         = "Municipal zoo in flames"  -- non-official adjective
  , cfreq         = [("caveZoo", 1)]
  , ccellSize     = DiceXY (1 `d` 4 + 7) 8
  , cminPlaceSize = DiceXY 4 4  -- don't merge
  , cmaxPlaceSize = DiceXY 14 7
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 7%10
  , copenChance   = 9%10
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 7 `d` 8
  , citemFreq     = [("common item", 100), ("light source", 1000)]
  , cplaceFreq    = [("zoo", 1)]
  , cpassable     = True
  , cdefTile      = "zooSetDark"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cwallTile     = "openableWall"
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Leaping flames illuminate the cages; not all are empty."
  }
ambush = rogue  -- a scenario with strong missiles;
                -- dark, so solid obstacles are to hide from missiles,
                -- not view, and they are all lit, because stopped missiles
                -- are frustrating, while a few LOS-only obstacles are not lit;
                -- few small lights to cross, giving a chance to snipe;
                -- crucial difference wrt shootout and hunt is that trajectories
                -- of missiles are usually not seen, so enemy can't be guessed;
                -- camping doesn't pay off, because enemies can sneak and only
                -- active scouting, throwing flares and shooting discovers them
                -- and the level is big enough for all that
  { csymbol       = 'M'
  , cname         = "Ravaged spaceport"  -- non-official adjective
  , cfreq         = [("caveAmbush", 1)]
  , ccellSize     = DiceXY 11 6
  , cminPlaceSize = DiceXY 9 10  -- merge vertically
  , cmaxPlaceSize = DiceXY 40 30  -- allow hangars and shuttles
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%10  -- few lit trails, so hard to aim
  , cextraStairs  = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 5 `d` 8
  , citemFreq     = [ ("common item", 30)
                    , ("any arrow", 400), ("harpoon", 300), ("explosive", 50) ]
  , cplaceFreq    = [("ambush", 1)]
  , cpassable     = True
  , cdefTile      = "ambushSetDark"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cwallTile     = "openableWall"
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Scarred walls and ransacked lockers show the total breakdown of order."  -- seems related to the abandoned farm; perhaps distantly to the existence of the gangs; more closely to the mystery of the lost and found space cruiser and various parties interested in it
  }

-- * Other caves; testing, Easter egg, future work

battle = rogue  -- few lights and many solids, to help the less numerous heroes
  { csymbol       = 'B'
  , cname         = "Old industrial plant"
  , cfreq         = [("caveBattle", 1)]
  , ccellSize     = DiceXY (5 `d` 3 + 11) 7
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 9 7
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
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
  , cdefTile      = "battleSetDark"
  , cdarkCorTile  = "trailLit"  -- let trails give off light
  , clitCorTile   = "trailLit"
  , cwallTile     = "openableWall"
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Huge machines stand silent and powerless in the dark."
  }
safari1 = brawl
  { cname         = "Hunam habitat"
  , cfreq         = [("caveSafari1", 1)]
  , cminPlaceSize = DiceXY 5 3
  , cextraStairs  = 1
  , cstairFreq    = [ ("outdoor walled staircase", 20)
                    , ("outdoor closed staircase", 80)
                    , ("outdoor tiny staircase", 1) ]
  , cstairAllowed = []
  , cdesc         = "\"DLC 1. Hunams scavenge in a forest in their usual disgusting way.\""
  }
safari2 = escape  -- lamps instead of trees, but ok, it's only a simulation
  { cname         = "Deep into the jungle"
  , cfreq         = [("caveSafari2", 1)]
  , cextraStairs  = 1
  , cescapeFreq   = []
  , cstairFreq    = [ ("outdoor walled staircase", 20)
                    , ("outdoor closed staircase", 80)
                    , ("outdoor tiny staircase", 1) ]
  , cstairAllowed = []
  , cdesc         = "\"DLC 2. In the dark pure heart of the jungle noble animals roam freely.\""
  }
safari3 = zoo  -- glass rooms, but ok, it's only a simulation
  { cname         = "Jungle in flames"
  , cfreq         = [("caveSafari3", 1)]
  , cminPlaceSize = DiceXY 5 4
  , cescapeFreq   = [("escape outdoor down", 1)]
  , cextraStairs  = 1
  , cstairFreq    = [ ("outdoor walled staircase", 20)
                    , ("outdoor closed staircase", 80)
                    , ("outdoor tiny staircase", 1) ]
  , cstairAllowed = []
  , cdesc         = "\"DLC 3. Jealous hunams set jungle on fire and flee.\""
  }
