-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Definitions of of cave kinds. Every level in the game is an instantiated
-- cave kind.
module Content.CaveKind
  ( -- * Group name patterns
    pattern CAVE_ROGUE, pattern CAVE_ARENA, pattern CAVE_LABORATORY, pattern CAVE_NOISE, pattern CAVE_SHALLOW_ROGUE, pattern CAVE_OUTERMOST, pattern CAVE_RAID, pattern CAVE_BRAWL, pattern CAVE_BRAWL_ALT, pattern CAVE_SHOOTOUT, pattern CAVE_HUNT, pattern CAVE_FLIGHT, pattern CAVE_ZOO, pattern CAVE_AMBUSH, pattern CAVE_BATTLE, pattern CAVE_SAFARI_1, pattern CAVE_SAFARI_2, pattern CAVE_SAFARI_3
  , pattern CAVE_BRIDGE, pattern CAVE_VIRUS, pattern CAVE_RESIDENTIAL, pattern CAVE_MUSEUM, pattern CAVE_EGRESS, pattern CAVE_CASINO, pattern CAVE_POWER, pattern CAVE_GAUNTLET
  , groupNamesSingleton, groupNames
  , -- * Content
    content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Data.Ratio
import qualified Data.Text as T

import           Game.LambdaHack.Content.CaveKind
import qualified Game.LambdaHack.Content.ItemKind as IK
import           Game.LambdaHack.Content.TileKind
import           Game.LambdaHack.Core.Dice
import           Game.LambdaHack.Definition.Defs
import           Game.LambdaHack.Definition.DefsInternal

import Content.ItemKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor
import Content.ItemKindEmbed
import Content.PlaceKind hiding (content, groupNames, groupNamesSingleton)
import Content.TileKind hiding (content, groupNames, groupNamesSingleton)

-- * Group name patterns

groupNamesSingleton :: [GroupName CaveKind]
groupNamesSingleton = []

groupNames :: [GroupName CaveKind]
groupNames =
       [CAVE_ROGUE, CAVE_ARENA, CAVE_LABORATORY, CAVE_NOISE, CAVE_SHALLOW_ROGUE, CAVE_OUTERMOST, CAVE_RAID, CAVE_BRAWL, CAVE_BRAWL_ALT, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_FLIGHT, CAVE_ZOO, CAVE_AMBUSH, CAVE_BATTLE, CAVE_SAFARI_1, CAVE_SAFARI_2, CAVE_SAFARI_3]
    ++ [CAVE_BRIDGE, CAVE_VIRUS, CAVE_RESIDENTIAL, CAVE_MUSEUM, CAVE_EGRESS, CAVE_CASINO, CAVE_POWER, CAVE_GAUNTLET]

pattern CAVE_ROGUE, CAVE_ARENA, CAVE_LABORATORY, CAVE_NOISE, CAVE_SHALLOW_ROGUE, CAVE_OUTERMOST, CAVE_RAID, CAVE_BRAWL, CAVE_BRAWL_ALT, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_FLIGHT, CAVE_ZOO, CAVE_AMBUSH, CAVE_BATTLE, CAVE_SAFARI_1, CAVE_SAFARI_2, CAVE_SAFARI_3 :: GroupName CaveKind

pattern CAVE_BRIDGE, CAVE_VIRUS, CAVE_RESIDENTIAL, CAVE_MUSEUM, CAVE_EGRESS, CAVE_CASINO, CAVE_POWER, CAVE_GAUNTLET :: GroupName CaveKind

pattern CAVE_ROGUE = GroupName "caveRogue"
pattern CAVE_ARENA = GroupName "caveArena"
pattern CAVE_LABORATORY = GroupName "caveLaboratory"
pattern CAVE_NOISE = GroupName "caveNoise"
pattern CAVE_SHALLOW_ROGUE = GroupName "caveShallowRogue"
pattern CAVE_OUTERMOST = GroupName "caveOutermost"
pattern CAVE_RAID = GroupName "caveRaid"
pattern CAVE_BRAWL = GroupName "caveBrawl"
pattern CAVE_BRAWL_ALT = GroupName "caveBrawlAlt"
pattern CAVE_SHOOTOUT = GroupName "caveShootout"
pattern CAVE_HUNT = GroupName "caveHunt"
pattern CAVE_FLIGHT = GroupName "caveFlight"
pattern CAVE_ZOO = GroupName "caveZoo"
pattern CAVE_AMBUSH = GroupName "caveAmbush"
pattern CAVE_BATTLE = GroupName "caveBattle"
pattern CAVE_SAFARI_1 = GroupName "caveSafari1"
pattern CAVE_SAFARI_2 = GroupName "caveSafari2"
pattern CAVE_SAFARI_3 = GroupName "caveSafari3"

-- ** Allure-specific
pattern CAVE_BRIDGE = GroupName "caveBridge"
pattern CAVE_VIRUS = GroupName "caveVirus"
pattern CAVE_RESIDENTIAL = GroupName "caveResidential"
pattern CAVE_MUSEUM = GroupName "caveMuseum"
pattern CAVE_EGRESS = GroupName "caveEgress"
pattern CAVE_CASINO = GroupName "caveCasino"
pattern CAVE_POWER = GroupName "cavePower"
pattern CAVE_GAUNTLET = GroupName "caveGauntlet"

-- * Content

content :: [CaveKind]
content =
  [rogue, residential, arena, casino, museum, laboratory, noise, power, empty, egress, outermost, bridge, shallowRogue, virus, gauntlet, raid, brawl, brawlAlt, shootout, hunt, flight, zoo, ambush, battle, safari1, safari2, safari3]

rogue,    residential, arena, casino, museum, laboratory, noise, power, empty, egress, outermost, bridge, shallowRogue, virus, gauntlet, raid, brawl, brawlAlt, shootout, hunt, flight, zoo, ambush, battle, safari1, safari2, safari3 :: CaveKind

-- * On-ship "caves", that is, decks, most of mediocre height and size

rogue = CaveKind
  { cname         = "Maintenance and storage"
  , cfreq         = [(DEFAULT_RANDOM, 100), (CAVE_ROGUE, 1)]
  , cXminSize     = 80
  , cYminSize     = 42
  , ccellSize     = DiceXY (2 `d` 4 + 10) (1 `d` 3 + 6)
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) (1 `d` 2 + 5)  -- sometimes merge vert.
  , cmaxPlaceSize = DiceXY 16 40  -- often maximize vertically
  , cdarkOdds     = 1 `d` 50 + 1 `dL` 50
      -- most rooms lit, to compensate for dark corridors; at the bottom
      -- of the dungeon, half of the rooms lit
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%2
  , cmaxVoid      = 1%8
  , cdoorChance   = 3%4
  , copenChance   = 1%5
  , chidden       = 7
  , cactorCoeff   = 70  -- the maze requires time to explore
  , cactorFreq    = [(MONSTER, 50), (ANIMAL, 20), (ROBOT, 30)]
  , citemNum      = 17 `d` 2 + 25 - 25 `dL` 1
      -- deep down quality over quantity; generally not too random,
      -- sacrificing replayability for consistent balance
  , citemFreq     = [ (IK.COMMON_ITEM, 40), (IK.CRAWL_ITEM, 20)
    -- CRAWL_ITEM items are used only in long scenarios, such as multi-level
    -- dungeon crawl; these may be powerful or a mundate item,
    -- unlike @TREASURE@ items
                    , (IK.TREASURE, 40) ]
  , cplaceFreq    = [(ROGUE, 1)]
  , cpassable     = False
  , clabyrinth    = False
  , cdefTile      = ROGUE_SET
  , cdarkCorTile  = FLOOR_CORRIDOR_DARK
  , clitCorTile   = FLOOR_CORRIDOR_LIT
  , cwallTile     = TRAPPABLE_WALL
  , ccornerTile   = FILLER_WALL
  , cfenceTileN   = S_BASIC_OUTER_FENCE
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = S_BASIC_OUTER_FENCE
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cfenceApart   = False
  , cminStairDist = 15  -- to help Casino stay small
  , cmaxStairsNum = 2 + 1 `d` 2
  , cescapeFreq   = []
  , cstairFreq    = [ (WALLED_LIFT, 50), (OPEN_LIFT, 50)
                    , (TINY_LIFT, 1) ]
  , cstairAllowed = [ (WALLED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cskip         = []
  , cinitSleep    = InitSleepPermitted
  , cdesc         = "Winding tunnels stretch into the dark. A few areas are passable but the remainder is packed with tanks and cells of raw materials and machinery."
  }
residential = rogue  -- an alternative with lit corridors but dark rooms
  { cfreq         = [(DEFAULT_RANDOM, 70), (CAVE_RESIDENTIAL, 1)]
  , cname         = "Residential area"
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) (1 `d` 2 + 9)  -- merge vert.
  , cmaxPlaceSize = DiceXY 20 20  -- fewer vertically long rooms
  , cdarkOdds     = 51  -- all rooms dark
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 1%5  -- differentiate; bare skeleton feel; long span paths,
                         -- but don't overdo or a lot of backtracing needed
  , cplaceFreq    = [(ROGUE, 1), (RESIDENTIAL, 49)]
  , cdefTile      = FILLER_WALL
  , cmaxStairsNum = 3 + 1 `d` 2
  , cstairFreq    = [ (WALLED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cstairAllowed = [ (WALLED_LIFT, 50), (OPEN_LIFT, 50)
                    , (TINY_LIFT, 1) ]
  , cdesc         = "The area has been powered down, except for emergency corridors. Many suites are depressurized and sealed."
  }
arena = rogue
  { cname         = "Recreational deck"
  , cfreq         = [(CAVE_ARENA, 1)]
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
  , chidden       = 0
  , cactorCoeff   = 50  -- smallish level, but easy to view and plan
  , cactorFreq    = [ (EXPLOSIVE_MONSTER, 50), (ANIMAL, 50), (ROBOT, 5)
                    , (IK.AQUATIC, 10) ]
  , citemNum      = 16 `d` 2
  , citemFreq     = [ (IK.COMMON_ITEM, 20), (IK.CRAWL_ITEM, 20)
                    , (IK.TREASURE, 40)
                    , (GARDENING_TOOL, 100)  -- useless and boring by this point
                    , (IK.ANY_POTION, 200) ]  -- nature
  , cplaceFreq    = [(ARENA, 1)]
  , cpassable     = True
  , cdefTile      = ARENA_SET_LIT
  , cdarkCorTile  = TRAIL_LIT  -- let trails give off light
  , clitCorTile   = TRAIL_LIT  -- may be rolled different than the above
  , cwallTile     = OPENABLE_WALL
  , cminStairDist = 20
  , cmaxStairsNum = 1 `d` 3
  , cstairFreq    = [ (WALLED_LIFT, 20), (CLOSED_LIFT, 80)
                    , (TINY_LIFT, 1) ]
  , cstairAllowed = [ (WALLED_STAIRCASE, 20), (CLOSED_STAIRCASE, 80)
                    , (TINY_STAIRCASE, 1) ]
  , cdesc         = "Debris litters the wide streets and all the stalls are either broken or have their shutters down. Nature is taking over, healing the wounds."  -- potions of healing
  }
casino = arena
  { cname         = "Casino"
  , cfreq         = [(CAVE_CASINO, 1)]
  , cXminSize     = 21
  , cYminSize     = 21
  , ccellSize     = DiceXY 19 (1 `d` 3 + 5)
  , cdarkOdds     = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth, though level too small.
  , cnightOdds    = 51  -- always night
  , cactorCoeff   = 100  -- cramped, don't overcrowd
  , cactorFreq    = [(MONSTER, 50), (ANIMAL, 25), (ROBOT, 50)]
  , citemNum      = 14 `d` 2  -- rare, so make it exciting by keeping many items
  , citemFreq     = [ (IK.COMMON_ITEM, 20)
                    , (IK.CRAWL_ITEM, 20)
                    , (IK.TREASURE, 100) ]  -- lives up to its name
  , cdefTile      = ARENA_SET_DARK
  , cfenceTileN   = HABITAT_CONTAINMENT_WALL  -- small cave
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = HABITAT_CONTAINMENT_WALL
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cminStairDist = 10
  , cmaxStairsNum = 2  -- to make possible 2 stairs in the last cave
  , cinitSleep    = InitSleepBanned
  , cdesc         = "The establishment is no longer filled with hollow-eyed gamblers; more dangerous things now lurk in the dark. But the greedy excitement is not gone, even if out of place. Flashing colourful lights and enticing sounds make it a place where one couldn't sleep."
  }
museum = arena
  { cname         = "Museum"
  , cfreq         = [(CAVE_MUSEUM, 1)]
  , cXminSize     = 25
  , cYminSize     = 25
  , ccellSize     = DiceXY 23 (1 `d` 3 + 5)
  , cdarkOdds     = 41 + 1 `d` 10  -- almost all rooms lit (1 in 10 dark)
  -- Trails provide enough light for fun stealth, though level too small.
  , cnightOdds    = 51  -- always night
  , cactorCoeff   = 100  -- cramped, don't overcrowd
  , cactorFreq    = [(MONSTER, 100), (ANIMAL, 25), (ROBOT, 25)]
  , citemNum      = 12 `d` 2  -- rare, so make it exciting despite sleeping foes
  , citemFreq     = [ (IK.COMMON_ITEM, 20)
                    , (IK.CRAWL_ITEM, 20)
                    , (IK.TREASURE, 20)
                    , (MUSEAL, 200) ]  -- lives up to its name
  , cplaceFreq    = [(MUSEUM, 1)]
  , cdefTile      = MUSEUM_SET_DARK
  , cfenceTileN   = HABITAT_CONTAINMENT_WALL  -- small cave
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = HABITAT_CONTAINMENT_WALL
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cminStairDist = 10
  , cinitSleep    = InitSleepAlways
  , cdesc         = "History has shown that museal treasures are safer in space than anywhere on Earth. Passengers eagerly attend exhibitions, even if over the weeks of the journey they become increasingly a captive audience and stifle many a yawn. Spaceship crew are not that enthusiastic but even they find reasons to pay visits despite misgivings of museum security. Quite often a museum is the only place within millions of kilometers to house a desperately needed tool, old but sturdy beyond anything a 3D printer can produce."
  }
laboratory = rogue
  { cname         = "Laboratory"
  , cfreq         = [(CAVE_LABORATORY, 1)]
  , cXminSize     = 60
  , cYminSize     = 42
  , ccellSize     = DiceXY (1 `d` 2 + 5) (1 `d` 2 + 7)
  , cminPlaceSize = DiceXY 6 8  -- merge, usually vertically
  , cmaxPlaceSize = DiceXY 12 40  -- often maximize vertically
  , cnightOdds    = 0  -- always day so that the corridor smoke is lit
  , cauxConnects  = 1%5
  , cmaxVoid      = 1%10
  , cdoorChance   = 1
  , copenChance   = 1%2
  , cactorFreq    = [ (MONSTER, 50), (ANIMAL, 70), (ROBOT, 5)
                    , (IK.AQUATIC, 10) ]
  , citemNum      = 20 `d` 2  -- reward difficulty, despite fewer rooms
  , citemFreq     = [ (IK.COMMON_ITEM, 20), (IK.CRAWL_ITEM, 40)
                    , (IK.TREASURE, 40), (IK.EXPLOSIVE, 80) ]
  , cplaceFreq    = [(LABORATORY, 1)]
  , cdefTile      = FILLER_WALL
  , cdarkCorTile  = LAB_TRAIL_LIT  -- let lab smoke give off light always
  , clitCorTile   = LAB_TRAIL_LIT
  , cminStairDist = 25
  , cmaxStairsNum = 1 `d` 2
  , cstairFreq    = [ (DECON_WALLED_LIFT, 50)
                    , (DECON_OPEN_LIFT, 50)
                    , (DECON_TINY_LIFT, 1) ]
      -- In lone wolf challenge, the player better summoned or dominated
      -- any helpers by this point. If not, good luck fighting bare-handed.
  , cstairAllowed = [ (DECON_WALLED_STAIRCASE, 50)
                    , (DECON_OPEN_STAIRCASE, 50)
                    , (DECON_TINY_STAIRCASE, 1) ]
  , cdesc         = "Shattered glassware and the sharp scent of spilt chemicals show that something terrible happened here. The reinforced bulkheads muffle furious roars."  -- a fair warning to skip the level or regroup
  }
noise = rogue
  { cname         = "Computing hardware hub"
  , cfreq         = [(CAVE_NOISE, 1)]
  , cXminSize     = 25
  , cYminSize     = 42
  , ccellSize     = DiceXY (1 `d` 3 + 7) 8
  , cminPlaceSize = DiceXY 7 7  -- often merge vertically
  , cmaxPlaceSize = DiceXY 8 20
  , cdarkOdds     = 51
  -- Light is deadly, because nowhere to hide and pillars enable spawning
  -- very close to heroes.
  , cnightOdds    = 0  -- harder variant, but looks cheerful
  , cauxConnects  = 1%10
  , cmaxVoid      = 1%100
  , cdoorChance   = 1  -- to prevent openings in solid rooms
  , chidden       = 0
  , cactorCoeff   = 100  -- the maze requires time to explore; also, small
  , cactorFreq    = [(MONSTER, 100), (ANIMAL, 5), (ROBOT, 25)]
  , citemNum      = 16 `d` 2  -- an incentive to explore the labyrinth
  , citemFreq     = [ (IK.COMMON_ITEM, 40), (IK.CRAWL_ITEM, 40)
                    , (IK.TREASURE, 40), (IK.ANY_SCROLL, 200) ]
  , cplaceFreq    = [(NOISE, 1)]
  , cpassable     = True
  , clabyrinth    = True
  , cdefTile      = NOISE_SET_LIT
  , cwallTile     = OPENABLE_WALL
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cdarkCorTile  = DAMP_FLOOR_DARK
  , clitCorTile   = DAMP_FLOOR_LIT
  , cmaxStairsNum = 1
  , cstairFreq    = [ (CLOSED_LIFT, 50), (OPEN_LIFT, 50)
                    , (TINY_LIFT, 1) ]
  , cstairAllowed = [ (CLOSED_STAIRCASE, 50), (OPEN_STAIRCASE, 50)
                    , (TINY_STAIRCASE, 1) ]
  , cinitSleep    = InitSleepBanned
  , cdesc         = "Several machines still function, processors whirring through routines scheduled by dead men. Some scattered chips can still be read."
  }
power = noise
  { cname         = "Power distribution hub"
  , cfreq         = [(CAVE_POWER, 1)]
  , cXminSize     = 32
  , cYminSize     = 42
  , ccellSize     = DiceXY (1 `d` 5 + 9) 9
  , cminPlaceSize = DiceXY 7 7
  , cmaxPlaceSize = DiceXY 20 20
  , cnightOdds    = 51  -- easier variant, but looks sinister
  , citemFreq     = [(IK.COMMON_ITEM, 20), (IK.CRAWL_ITEM, 10), (GEM, 80)]
                      -- can't be "valuable" or template items generated
  , clabyrinth    = True
  , cdefTile      = POWER_SET_DARK
  , cdarkCorTile  = OILY_FLOOR_DARK
  , clitCorTile   = OILY_FLOOR_LIT
  , cmaxStairsNum = 2
    -- This determines that the continuous staircase uses stairs, not lifts.
  , cstairFreq    = [ (GATED_CLOSED_STAIRCASE, 50)
                    , (GATED_OPEN_STAIRCASE, 50)
                    , (GATED_TINY_STAIRCASE, 1) ]
  , cstairAllowed = [ (GATED_CLOSED_LIFT, 50)
                    , (GATED_OPEN_LIFT, 50)
                    , (GATED_TINY_LIFT, 1) ]
  , cinitSleep    = InitSleepBanned
  , cdesc         = "A trickle of energy flows through a hub that could power a city. The air is warm and carries organic stench. Once in a while a young animal scurries across a lit patch of ground, pouncing in low gravity."
  }
empty = rogue
  { cname         = "Construction site"
  , cfreq         = []  -- just a template for some others
  , ccellSize     = DiceXY (1 `d` 5 + 18) 16
  , cminPlaceSize = DiceXY 9 9  -- normally don't merge
  , cmaxPlaceSize = DiceXY 50 20  -- often maximize horizontally
  , cdarkOdds     = 1 `d` 100 + 1 `dL` 100
  , cnightOdds    = 0  -- always day
  , cauxConnects  = 3%2
  , cmaxVoid      = 0  -- too few rooms to have void and fog common anyway
  , cdoorChance   = 1  -- to prevent openings in solid rooms
  , chidden       = 0
  , cactorCoeff   = 50  -- easy to view and plan
  , cactorFreq    = [(MONSTER, 10), (ANIMAL, 5), (ROBOT, 85)]
  , citemNum      = 16 `d` 2  -- lots of free space, but extra loot present
  , cplaceFreq    = [(EMPTY, 1)]
  , cpassable     = True
  , cdefTile      = EMPTY_SET_LIT
  , cdarkCorTile  = FLOOR_ARENA_DARK
  , clitCorTile   = FLOOR_ARENA_LIT
  , cwallTile     = OPENABLE_WALL
  , cfenceApart   = True  -- ensures no cut-off border airlocks and tanks
  , cminStairDist = 30
  , cstairFreq    = [ (WALLED_LIFT, 20), (CLOSED_LIFT, 80)
                    , (TINY_LIFT, 1) ]
  , cstairAllowed = [ (WALLED_STAIRCASE, 20), (CLOSED_STAIRCASE, 80)
                    , (TINY_STAIRCASE, 1) ]
  , cdesc         = "Not much to see here yet."
  }
egress = empty
  { cname         = "Shuttle servicing level"
  , cfreq         = [(CAVE_EGRESS, 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 20) 16
  , cmaxPlaceSize = DiceXY 25 20
  , cdarkOdds     = 51  -- all dark to compensate for the always lit shuttles
  , cplaceFreq    = [(EGRESS, 1)]
  , cdefTile      = EGRESS_SET_LIT
  , cdarkCorTile  = TRANSPORT_ROUTE
  , clitCorTile   = TRANSPORT_ROUTE
  , cfenceTileN   = S_BASIC_OUTER_FENCE
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = AIRLOCK_FENCE
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cmaxStairsNum = 2  -- too many hinder generation of shuttles
  , cescapeFreq   = [(SPACESHIP_ESCAPE_DOWN, 1)]
  , cstairFreq    = [(WALLED_LIFT, 20), (TINY_LIFT, 1)]
  , cstairAllowed = [(WALLED_STAIRCASE, 20), (TINY_STAIRCASE, 1)]
  , cdesc         = "Empty husks and strewn entrails of small craft litter the hangar among cranes and welding machines. The distant main fusion thruster array can be seen to the rear of the spaceship through oriels and airlocks of all sizes."
      -- E and W sides are borders with other level sections, so no oriels.
      -- The meteor shield towards N is not punctured here, because
      -- the cargo bay is too thick here, near the axis of the ship.
  }
outermost = empty
  { cname         = "Outermost deck"
  , cfreq         = [(CAVE_OUTERMOST, 100)]
  , cdarkOdds     = 0  -- all rooms lit, because can be huge and few lights yet
  , cactorCoeff   = 5  -- shallower than LH, so fewer immediate actors, so boost
  , cactorFreq    = [ (ANIMAL, 3), (ROBOT, 1)
                    , (IMMOBILE_ROBOT, 90), (IMMOBILE_ANIMAL, 2)
                    , (AQUATIC_ANIMAL, 2) ]  -- (AQUATIC_ROBOT, 2)
      -- The medbot faucets on lvl 1 act like HP resets. Needed to avoid
      -- cascading failure, if the particular starting conditions were
      -- very hard. Items are not reset, even if they are bad, which provides
      -- enough of a continuity. The faucets on lvl 1 are not OP and can't be
      -- abused, because they spawn less and less often and also HP doesn't
      -- effectively accumulate over max.
  , citemFreq     = [ (IK.COMMON_ITEM, 50), (IK.CRAWL_ITEM, 50)
                    , (GARDENING_TOOL, 600) ]
  , cfenceTileN   = ORIELS_FENCE
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = EMPTY_AIRLOCK_FENCE
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cmaxStairsNum = 2
  , cdesc         = T.intercalate "\n"
      [ "This is as far as one can go \"down\". The void outside sucks light through the oriel and airlock glass in the walls and floor of this outermost level. Each minute, the dusky melancholic light of the distant Sun attempts for a few seconds to squeeze in but is repelled by artificial lighting."
      , "The mucky floor marked by unkempt greenery looks misleadingly straight, its curvature noticeable only across the whole extent of the hull section. Overflowing water basins and series of hanging and stacked tanks double as radiation shields. Hoses writhe on the ground and dangle in thick knots from the ceiling. With proper tools, some of the containers could be opened and working spaces productively employed. There is no junk is space."
      , "This deck is the main pressurized cargo bay and storage, with the only other docking hub for small craft located among the giant spaceship's upper levels. Somewhere here must be the airlock you docked your shuttle to and stacked your supplies against." ]
      -- E and W sides are borders with other level sections, so no oriels.
  }
bridge = rogue
  { cname         = "Captain's bridge"
  , cfreq         = [(CAVE_BRIDGE, 1)]
  , cXminSize     = 37
  , cYminSize     = 30
  , ccellSize     = DiceXY (2 `d` 2 + 7) (1 `d` 2 + 5)
  , cminPlaceSize = DiceXY (2 `d` 2 + 5) (1 `d` 2 + 4)  -- sometimes merge all
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cauxConnects  = 1  -- few rooms, so many corridors
  , cmaxVoid      = 1%4  -- few rooms, so increase the chance of any void
  , cactorCoeff   = 400  -- it's quite deep already, so spawn slowly;
                         -- this is initially the best level for sleeping
  , cactorFreq    = [(ANIMAL, 100)]
  , citemNum      = 16  -- make the initial experience tamer
  , citemFreq     = [(IK.COMMON_ITEM, 100), (GARDENING_TOOL, 800)]
  , cdefTile      = FILLER_WALL
  , cfenceTileN   = HABITAT_CONTAINMENT_WALL  -- cave isolated for safety
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = HABITAT_CONTAINMENT_WALL
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cmaxStairsNum = 1
  , cstairFreq    = [ (WELDED_WALLED_LIFT, 50)
                    , (WELDED_OPEN_LIFT, 50)
                    , (WELDED_TINY_LIFT, 1) ]
  , cstairAllowed = [ (WELDED_WALLED_STAIRCASE, 50)
                    , (WELDED_OPEN_STAIRCASE, 50)
                    , (WELDED_TINY_STAIRCASE, 1) ]
  , cdesc         = "The bridge is gutted out and nonoperational. You saved space on the shuttle by only packing demolition equipment (and booze, long gone, all flasks flung to fend off the annoying vermin) and now you can't even attempt repairs. You are also short on rations and vials of medicine to treat your recent wounds. Only water is plentiful on the ship: gaseous, liquid, frozen. There are animal cries down below and ominous silence up above."
  }
shallowRogue = rogue
  { cfreq         = [(CAVE_SHALLOW_ROGUE, 100)]
  , cXminSize     = 60
  , cYminSize     = 37
  , cactorCoeff   = 150  -- more difficult
  , cactorFreq    = filter ((/= MONSTER) . fst) $ cactorFreq rogue
  , citemNum      = 20  -- make the initial experience tamer
  , citemFreq     = [ (IK.COMMON_ITEM, 40), (IK.CRAWL_ITEM, 60)
                    , (GARDENING_TOOL, 700), (IK.ANY_FLASK, 200) ]
  , cmaxStairsNum = 2
  , cskip         = [0, 1]  -- ban foes camping on either stairs
  , cdesc         = T.intercalate "\n"
      [ "This close to the outermost deck, residence is not permitted and walls and doors are sturdier to contain a theoretically possible micro-meteorite breach. The entry is not closed off, though, because some passengers can't live without a regular pilgrimage to 'look outside' and the only way to the bottom-most level leads through here. Apparently, gazing at the sharp pin-points of stars and planets through the reinforced oriel glass is incomparable to watching the same through the thin polymer of wall displays."
      , "Animals appear to share the fascination of outer decks, perhaps attracted by the increased gravity, nearly Earth-like, unlike elsewhere on the ship. However, they dislike many industrial fluids stored on these floors, so flinging random flasks at them works as an effective deterrent. Moreover, if you throw an unidentified flask, you can be sure you won't waste a badly needed nano medicine, because it's never stored in such large containers. Even tiny vials cost a fortune." ]
  }
virus = rogue  -- this is a hard level requiring preparation; can be skipped
  { cname         = "Machinarium"
  , cfreq         = [(CAVE_VIRUS, 1)]
  , cXminSize     = 17
  , cYminSize     = 13
  , ccellSize     = DiceXY 4 4
  , cminPlaceSize = DiceXY 3 3
  , cmaxPlaceSize = DiceXY 5 4
  , cdarkOdds     = 51  -- all rooms dark
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 0
  , cmaxVoid      = 0
  , cdoorChance   = 0  -- openings in solid rooms are fine, because rooms tiny
  , chidden       = 0
  , cactorCoeff   = 4  -- fast spawning
  , cactorFreq    = [(MOBILE_ROBOT, 100)]  -- only mobile, for fast action
  , citemNum      = 12  -- avoid random excess given the low area
  , citemFreq     = [(IK.COMMON_ITEM, 70), (IK.CRAWL_ITEM, 30)]
  , cplaceFreq    = [(VIRUS, 1)]
  , cpassable     = True
  , clabyrinth    = True  -- don't let aliens explore and farm the robots
  , cdefTile      = VIRUS_SET_DARK
  , cdarkCorTile  = OILY_FLOOR_DARK
  , clitCorTile   = OILY_FLOOR_LIT
  , cwallTile     = OPENABLE_WALL
  , cfenceTileN   = HABITAT_CONTAINMENT_WALL  -- small cave
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = HABITAT_CONTAINMENT_WALL
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cmaxStairsNum = 1
  , cstairFreq    = [(TINY_LIFT, 1)]
  , cstairAllowed = [(TINY_STAIRCASE, 1)]
  , cdesc         = "The hall surrounded by automated warehouses is where ship's robots are repaired, decommissioned, stored and recycled. This one has been completely cut off from the vessel's network and even lights are out. And yet, the hot darkness smelling of ozone carries echos of arcing and clanking all around."
  }

-- * "Caves" on various celestial bodies (including, but not limited to, moons,
--    with virtually no story-wise limits wrt height and size.
--    Here @citemNum@ is very random, for interesting replays; no risk
--    of bad rolls accumulating and trashing a long game, particularly
--    in symmetric scenarios, where both parties can use extra loot
--    equally well.

gauntlet = rogue
  { cname         = "Service tunnel"
  , cfreq         = [(CAVE_GAUNTLET, 1)]
  , cXminSize     = 52  -- long tunnel
  , cYminSize     = 8
  , ccellSize     = DiceXY 6 4
  , cminPlaceSize = DiceXY 5 3
  , cmaxPlaceSize = DiceXY 6 3
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cnightOdds    = 0
  , cauxConnects  = 0
  , cmaxVoid      = 0
  , cdoorChance   = 1%3  -- openings in solid rooms are fine, because rooms tiny
  , chidden       = 0
  , cactorCoeff   = 4  -- fast spawning
  , cactorFreq    = [(GAUNTLET_ROBOT, 100)]  -- no drops; tutorial; avoid stash
  , citemNum      = 0  -- first tutorial mode, no need for a stash
  , citemFreq     = []  -- first tutorial level, no need for a stash
  , cplaceFreq    = [(GAUNTLET, 1)]  -- first tutorial mode; keep it simple
  , cpassable     = True
  , cdefTile      = VIRUS_SET_LIT  -- first tutorial mode; keep it simple
  , cwallTile     = OPENABLE_WALL
  , cfenceTileN   = HABITAT_CONTAINMENT_WALL
  , cfenceTileE   = HABITAT_CONTAINMENT_WALL
  , cfenceTileS   = HABITAT_CONTAINMENT_WALL
  , cfenceTileW   = HABITAT_CONTAINMENT_WALL
  , cmaxStairsNum = 0
  , cescapeFreq   = [(ALARM_ESCAPE_UP, 1)]
  , cstairFreq    = []
  , cstairAllowed = []
  , cinitSleep    = InitSleepBanned  -- to make it harder to escape freely
  , cskip         = [0]  -- don't start heroes nor opponents on escape
  , cdesc         = T.intercalate "\n"
      [ "Triton's cryothermal vents that supply energy and resources to the city are spread far apart. That funnels the unregulated sprawl of the sublunar city wide rather than deep and favours tunnels over domes."
      , "The deepest tunnels, such as this one, serve exclusively as emergency secondary connections between Triton City forges, farms and population centers. They are devoid of amenities and normally unused except by lazy maintenance crews storing and then leaving behind defunct machinery and leftover spare parts. Nobody can be contacted from such a remote corridor, except through a red alarm console, mandatory per every 100m of transport tunnels." ]
  }
raid = rogue
  { cname         = "Triton City sewers"
  , cfreq         = [(CAVE_RAID, 1)]
  , cXminSize     = 60  -- long sewer tunnels
  , cYminSize     = 21
  , ccellSize     = DiceXY (2 `d` 2 + 7) 6
  , cminPlaceSize = DiceXY (2 `d` 2 + 4) 5  -- sometimes merge all
  , cmaxPlaceSize = DiceXY 16 20
  , cdarkOdds     = 0  -- all rooms lit, for a gentle start
  , cmaxVoid      = 1%5
  , cdoorChance   = 1  -- make sure enemies not seen on turn 1
  , copenChance   = 0  -- make sure enemies not seen on turn 1
  , cdefTile      = FILLER_WALL
  , cactorCoeff   = 300  -- deep level with no kit, so slow spawning
  , cactorFreq    = [(ANIMAL, 50), (ROBOT, 50)]
  , citemNum      = 20  -- make the initial experience tamer
  , citemFreq     = [ (IK.COMMON_ITEM, 30)
                    , (STARTING_ARMOR, 100), (STARTING_WEAPON, 300)
                    , (WEAK_ARROW, 100), (LIGHT_ATTENUATOR, 50)
                    , (IK.S_CURRENCY, 400), (IK.ANY_SCROLL, 100) ]
                    -- introducing chips in this scenario
  , cplaceFreq    = [(RAID, 1)]
  , cmaxStairsNum = 0
  , cescapeFreq   = [(INDOOR_ESCAPE_UP, 1)]
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Mold spreads across the walls and scuttling sounds can be heard in the distance."
  }
brawl = rogue  -- many random solid tiles, to break LOS, since it's a day
               -- and this scenario is not focused on ranged combat;
               -- also, sanctuaries against missiles in shadow under trees
  { cname         = "Woodland biosphere"
  , cfreq         = [(CAVE_BRAWL, 1)]
  , cXminSize     = 60
  , cYminSize     = 30
  , ccellSize     = DiceXY (2 `d` 5 + 5) 7
  , cminPlaceSize = DiceXY 3 3  -- rarely merge vertically
  , cmaxPlaceSize = DiceXY 7 5
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 6 `d` 6
  , citemFreq     = [ (IK.COMMON_ITEM, 50)
                    , (STARTING_WEAPON, 200), (STARTING_ARMOR, 400)
                    , (IK.ANY_SCROLL, 100), (IK.ANY_POTION, 600) ]
                      -- introducing vials in this scenario
  , cplaceFreq    = [(BRAWL, 1)]
  , cpassable     = True
  , cdefTile      = BRAWL_SET_LIT
  , cdarkCorTile  = DIRT_LIT
  , clitCorTile   = DIRT_LIT
  , cwallTile     = OPENABLE_WALL
  , cmaxStairsNum = 1
  , cstairFreq    = [(OUTDOOR_TINY_STAIRCASE, 1)]
  , cstairAllowed = []
  , cskip         = []  -- start heroes on stairs, since they are created first
  , cdesc         = "Shadows pool under the trees and leaves crunch underfoot."
  }
brawlAlt = brawl
  { cfreq         = [(CAVE_BRAWL_ALT, 1)]
  , cmaxVoid      = 1%100  -- to ensure enough trees
  , cdefTile      = HUNT_SET_LIT  -- alt
  , cdarkCorTile  = S_FROZEN_PATH  -- alt
  , clitCorTile   = S_FROZEN_PATH
  , cskip         = [0]  -- ban foes camping on stairs
  , cdesc         = "With the Sun so distant, biospheres are stacked vertically rather than horizontally, sharing drainage, not lighting."
  }
shootout = rogue  -- a scenario with strong missiles;
                  -- few solid tiles, but only translucent tiles or walkable
                  -- opaque tiles, to make scouting and sniping more interesting
                  -- and to avoid obstructing view too much, since this
                  -- scenario is about ranged combat at long range
  { cname         = "Hydroponic farm"  -- still a neutral, official wording
  , cfreq         = [(CAVE_SHOOTOUT, 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 5) 6
  , cminPlaceSize = DiceXY 3 3  -- rarely merge vertically
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkOdds     = 0  -- all lit, not to duplicate the @hunt@ ranged tactics
  , cnightOdds    = 0
  , cauxConnects  = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 6 `d` 16
                      -- less items in inventory, more to be picked up,
                      -- to reward explorer and aggressor and punish camper;
                      -- very different experience depending on the roll,
                      -- but it's symmetric, so it's fine
  , citemFreq     = [ (IK.COMMON_ITEM, 30), (GARDENING_TOOL, 500)
                    , (ANY_ARROW, 400), (HARPOON, 200), (IK.EXPLOSIVE, 300) ]
                      -- Many consumable buffs are needed in symmetric maps
                      -- so that aggressor prepares them in advance and camper
                      -- needs to waste initial turns to buff for the defence.
  , cplaceFreq    = [(SHOOTOUT, 1)]
  , cpassable     = True
  , cdefTile      = SHOOTOUT_SET_LIT
  , cdarkCorTile  = DIRT_LIT
  , clitCorTile   = DIRT_LIT
  , cwallTile     = OPENABLE_WALL
  , cmaxStairsNum = 0
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Once so carefully curated, the planting beds are now overgrown and choked with weeds. The recently imposed high taxes make the traditional ways of life in space unsustainable."  -- also explains the gangs elsewhere and the motivation of adventurers to take risks (in addition to male hormones)
  }
hunt = rogue  -- a scenario with strong missiles for ranged and shade for melee;
              -- the human is likely to focus on melee, not having overwatch
  { cname         = "Swamp biosphere"
  , cfreq         = [(CAVE_HUNT, 1)]
  , ccellSize     = DiceXY (1 `d` 2 + 5) 6
  , cminPlaceSize = DiceXY 3 3  -- rarely merge vertically
  , cmaxPlaceSize = DiceXY 5 5
  , cdarkOdds     = 51
  , cnightOdds    = 0
  , cauxConnects  = 1%10
  , cdoorChance   = 1
  , copenChance   = 0
  , chidden       = 0
  , cactorCoeff   = 400  -- spawn slowly
  , cactorFreq    = [(INSECT, 100)]
  , citemNum      = 6 `d` 10
  , citemFreq     = [(IK.COMMON_ITEM, 30), (ANY_ARROW, 60), (HARPOON, 30)]
  , cplaceFreq    = [(BRAWL, 50), (SHOOTOUT, 100)]
  , cpassable     = True
  , cdefTile      = HUNT_SET_LIT  -- much more water than in shootoutSetLit
  , cdarkCorTile  = DIRT_LIT
  , clitCorTile   = DIRT_LIT
  , cwallTile     = OPENABLE_WALL
  , cmaxStairsNum = 0
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Mangrove trees and murky water, inspired by a habitat now eradicated from Earth. A rather deadly habitat."
  }
flight = rogue  -- a scenario with weak missiles, because heroes don't depend
                -- on them; dark, so solid obstacles are to hide from missiles,
                -- not view; obstacles are not lit, to frustrate the AI;
                -- lots of small lights to cross, to have some risks
  { cname         = "Red Collar Bros den"  -- tension rises; non-official name
  , cfreq         = [(CAVE_FLIGHT, 1)]
  , ccellSize     = DiceXY (1 `d` 3 + 6) 7
  , cminPlaceSize = DiceXY 5 4  -- rarely merge
  , cmaxPlaceSize = DiceXY 9 9  -- bias towards larger lamp areas
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 2  -- many lit trails, so easy to aim
  , cmaxVoid      = 1%100
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 8 `d` 8
  , citemFreq     = [ (IK.COMMON_ITEM, 30), (STARTING_ARMOR, 100)
                    , (LIGHT_ATTENUATOR, 300), (GEM, 400)
                    , (WEAK_ARROW, 400), (HARPOON, 200), (IK.EXPLOSIVE, 200) ]
  , cplaceFreq    = [(FLIGHT, 1)]
  , cpassable     = True
  , cdefTile      = FLIGHT_SET_DARK
  , cdarkCorTile  = SAFE_TRAIL_LIT  -- let trails give off light
  , clitCorTile   = SAFE_TRAIL_LIT
  , cwallTile     = OPENABLE_WALL
  , cescapeFreq   = [(OUTDOOR_ESCAPE_DOWN, 1)]
  , cmaxStairsNum = 0
  , cstairFreq    = []
  , cstairAllowed = []
  , cskip         = [0]  -- don't start heroes nor opponents on escape
  , cdesc         = "Graffiti scrawls across the walls and the heavy scents of stimulants hang in the air."
  }
zoo = rogue  -- few lights and many solids, to help the less numerous heroes
  { cname         = "Municipal zoo in flames"  -- non-official adjective
  , cfreq         = [(CAVE_ZOO, 1)]
  , ccellSize     = DiceXY (1 `d` 4 + 7) 8
  , cminPlaceSize = DiceXY 4 4  -- don't merge
  , cmaxPlaceSize = DiceXY 14 7
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 7%10
  , copenChance   = 9%10
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 10 `d` 8
  , citemFreq     = [ (IK.COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 1000)
                    , (STARTING_ARMOR, 500), (STARTING_WEAPON, 1000) ]
  , cplaceFreq    = [(ZOO, 1)]
  , cpassable     = True
  , cdefTile      = ZOO_SET_DARK
  , cdarkCorTile  = SAFE_TRAIL_LIT  -- let trails give off light
  , clitCorTile   = SAFE_TRAIL_LIT
  , cwallTile     = OPENABLE_WALL
  , cmaxStairsNum = 0
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
  { cname         = "Ravaged spaceport"  -- non-official adjective
  , cfreq         = [(CAVE_AMBUSH, 1)]
  , ccellSize     = DiceXY 11 6
  , cminPlaceSize = DiceXY 9 10  -- merge vertically
  , cmaxPlaceSize = DiceXY 40 30  -- allow hangars and shuttles
  , cdarkOdds     = 51  -- all dark to spread loot evenly
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%10  -- few lit trails, so hard to aim
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 10 `d` 10
  , citemFreq     = [ (IK.COMMON_ITEM, 30), (MERCENARY_AMMO, 200)
                    , (HARPOON, 300), (IK.EXPLOSIVE, 50) ]
  , cplaceFreq    = [(AMBUSH, 1)]
  , cpassable     = True
  , cdefTile      = AMBUSH_SET_DARK
  , cdarkCorTile  = TRAIL_LIT  -- let trails give off light
  , clitCorTile   = TRAIL_LIT
  , cwallTile     = OPENABLE_WALL
  , cmaxStairsNum = 0
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Scarred walls and ransacked lockers show the total breakdown of order."  -- seems related to the abandoned farm; perhaps distantly to the existence of the gangs; more closely to the mystery of the lost and found space cruiser and various parties interested in it
  }

-- * Other caves; testing, Easter egg, future work

battle = rogue  -- few lights and many solids, to help the less numerous heroes
  { cname         = "Old industrial plant"
  , cfreq         = [(CAVE_BATTLE, 1)]
  , ccellSize     = DiceXY (5 `d` 3 + 11) 7
  , cminPlaceSize = DiceXY 4 4
  , cmaxPlaceSize = DiceXY 9 7
  , cdarkOdds     = 0
  , cnightOdds    = 51  -- always night
  , cauxConnects  = 1%4
  , cmaxVoid      = 1%20
  , cdoorChance   = 2%10
  , copenChance   = 9%10
  , chidden       = 0
  , cactorFreq    = []
  , citemNum      = 6 `d` 8
  , citemFreq     = [(IK.COMMON_ITEM, 100), (LIGHT_ATTENUATOR, 200)]
  , cplaceFreq    = [(BATTLE, 50), (ROGUE, 50)]
  , cpassable     = True
  , cdefTile      = BATTLE_SET_DARK
  , cdarkCorTile  = SAFE_TRAIL_LIT  -- let trails give off light
  , clitCorTile   = SAFE_TRAIL_LIT
  , cwallTile     = OPENABLE_WALL
  , cfenceApart   = True  -- ensures no cut-off parts from collapsed
  , cmaxStairsNum = 0
  , cstairFreq    = []
  , cstairAllowed = []
  , cdesc         = "Huge machines stand silent and powerless in the dark."
  }
safari1 = brawl
  { cname         = "Hunam habitat"
  , cfreq         = [(CAVE_SAFARI_1, 1)]
  , cminPlaceSize = DiceXY 5 3
  , cstairFreq    = [ (OUTDOOR_WALLED_STAIRCASE, 20)
                    , (OUTDOOR_CLOSED_STAIRCASE, 80)
                    , (OUTDOOR_TINY_STAIRCASE, 1) ]
  , cskip         = [0]
  , cdesc         = "\"DLC 1. Hunams scavenge in a forest in their usual disgusting way.\""
  }
safari2 = flight  -- lamps instead of trees, but ok, it's only a simulation
  { cname         = "Deep into the jungle"
  , cfreq         = [(CAVE_SAFARI_2, 1)]
  , cmaxStairsNum = 1
  , cescapeFreq   = []
  , cstairFreq    = [ (OUTDOOR_WALLED_STAIRCASE, 20)
                    , (OUTDOOR_CLOSED_STAIRCASE, 80)
                    , (OUTDOOR_TINY_STAIRCASE, 1) ]
  , cskip         = [0]
  , cdesc         = "\"DLC 2. In the dark pure heart of the jungle noble animals roam freely.\""
  }
safari3 = zoo  -- glass rooms, but ok, it's only a simulation
  { cname         = "Jungle in flames"
  , cfreq         = [(CAVE_SAFARI_3, 1)]
  , cminPlaceSize = DiceXY 5 4
  , cescapeFreq   = [(OUTDOOR_ESCAPE_DOWN, 1)]
  , cmaxStairsNum = 1
  , cstairFreq    = [ (OUTDOOR_WALLED_STAIRCASE, 20)
                    , (OUTDOOR_CLOSED_STAIRCASE, 80)
                    , (OUTDOOR_TINY_STAIRCASE, 1) ]
  , cdesc         = "\"DLC 3. Jealous hunams set jungle on fire and flee.\""
  }
