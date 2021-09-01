-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Definitions of tile kinds. Every terrain tile in the game is
-- an instantiated tile kind.
module Content.TileKind
  ( -- * Group name patterns
    -- ** Used in CaveKind and perhaps elsewhere.
    pattern FILLER_WALL, pattern FLOOR_CORRIDOR_LIT, pattern FLOOR_CORRIDOR_DARK, pattern TRAIL_LIT, pattern SAFE_TRAIL_LIT, pattern LAB_TRAIL_LIT, pattern DAMP_FLOOR_LIT, pattern DAMP_FLOOR_DARK, pattern DIRT_LIT, pattern DIRT_DARK, pattern FLOOR_ARENA_LIT, pattern FLOOR_ARENA_DARK
  , pattern HABITAT_CONTAINMENT_WALL, pattern TRANSPORT_ROUTE, pattern ORIELS_FENCE, pattern AIRLOCK_FENCE, pattern EMPTY_AIRLOCK_FENCE, pattern OPENABLE_WALL, pattern TRAPPABLE_WALL, pattern OILY_FLOOR_LIT, pattern OILY_FLOOR_DARK
  , pattern EMPTY_SET_LIT, pattern EMPTY_SET_DARK, pattern NOISE_SET_LIT, pattern POWER_SET_LIT, pattern POWER_SET_DARK, pattern BATTLE_SET_LIT, pattern BATTLE_SET_DARK, pattern BRAWL_SET_LIT, pattern SHOOTOUT_SET_LIT, pattern ZOO_SET_LIT, pattern ZOO_SET_DARK, pattern ESCAPE_SET_LIT, pattern ESCAPE_SET_DARK, pattern AMBUSH_SET_LIT, pattern AMBUSH_SET_DARK, pattern ARENA_SET_LIT, pattern ARENA_SET_DARK
  , pattern ROGUE_SET, pattern MUSEUM_SET_LIT, pattern MUSEUM_SET_DARK, pattern HUNT_SET_LIT, pattern EXIT_SET_LIT
    -- ** Used in PlaceKind, but not in CaveKind.
  , pattern TREE_SHADE_WALKABLE_LIT, pattern TREE_SHADE_WALKABLE_DARK, pattern SMOKE_CLUMP_LIT, pattern SMOKE_CLUMP_DARK, pattern BUSH_CLUMP_LIT, pattern BUSH_CLUMP_DARK, pattern FOG_CLUMP_LIT, pattern FOG_CLUMP_DARK, pattern STAIR_TERMINAL_LIT, pattern STAIR_TERMINAL_DARK, pattern SIGNBOARD, pattern STAIRCASE_UP, pattern ORDINARY_STAIRCASE_UP, pattern STAIRCASE_OUTDOOR_UP, pattern GATED_STAIRCASE_UP, pattern STAIRCASE_DOWN, pattern ORDINARY_STAIRCASE_DOWN, pattern STAIRCASE_OUTDOOR_DOWN, pattern GATED_STAIRCASE_DOWN, pattern ESCAPE_UP, pattern ESCAPE_DOWN, pattern ESCAPE_OUTDOOR_DOWN, pattern TRANSPARENT_WALL, pattern ICE_BUILDUP, pattern WORKSHOP, pattern FLOOR_ACTOR_ITEM, pattern FLOOR_ACTOR_ITEM_LIT, pattern FLOOR_ACTOR_ITEM_DARK
  , pattern S_PILLAR, pattern S_RUBBLE_PILE, pattern S_LAMP_POST, pattern S_TREE_LIT, pattern S_TREE_DARK, pattern S_PULPIT, pattern S_BUSH_LIT, pattern S_FOG_LIT, pattern S_SMOKE_LIT, pattern S_FLOOR_ACTOR_LIT, pattern S_FLOOR_ACTOR_DARK, pattern S_FLOOR_ASHES_LIT, pattern S_FLOOR_ASHES_DARK, pattern S_SHADED_GROUND, pattern S_SHALLOW_WATER_LIT, pattern S_SHALLOW_WATER_DARK
  , pattern BUSH_GROVE_LIT, pattern BUSH_GROVE_DARK, pattern UNDERBRUSH_CLUMP_LIT, pattern UNDERBRUSH_CLUMP_DARK, pattern ASHES_SMOKE_LIT, pattern ASHES_SMOKE_DARK, pattern RECT_WINDOWS, pattern DOORLESS_MACHINERY, pattern PUMPS_LIT, pattern PUMPS_DARK, pattern DOORLESS_WALL, pattern OIL_RESIDUE_LIT, pattern OIL_RESIDUE_DARK, pattern LIFT_TERMINAL_LIT, pattern LIFT_TERMINAL_DARK, pattern STAIRCASE_LIFT_UP, pattern STAIRCASE_LIFT_DOWN, pattern GATED_LIFT_UP, pattern GATED_LIFT_DOWN, pattern DECON_STAIRCASE_UP, pattern DECON_STAIRCASE_DOWN, pattern DECON_LIFT_UP, pattern DECON_LIFT_DOWN, pattern WELDED_STAIRCASE_UP, pattern WELDED_LIFT_UP, pattern ESCAPE_SPACESHIP_DOWN, pattern ORDINARY_LIFT_UP, pattern ORDINARY_LIFT_DOWN, pattern RUBBLE_OR_WASTE_LIT, pattern RUBBLE_OR_WASTE_DARK, pattern CACHE_DEPOSIT, pattern CACHE_JEWELRY, pattern CACHE_MAZE, pattern CACHE_SHUTTLE, pattern TRAPPED_DOOR, pattern STUCK_DOOR, pattern BARREL
  , pattern S_POOL_LIT, pattern S_POOL_DARK, pattern S_CLOSED_DOOR, pattern S_OPEN_DOOR, pattern S_OIL_SPILL, pattern S_FROZEN_PATH, pattern S_LIFT_SHAFT, pattern S_REINFORCED_WALL, pattern S_SHUTTLE_HULL, pattern S_HARDWARE_RACK, pattern S_STAIRCASE_TRAP_DOWN_OIL, pattern S_UNDERBRUSH_LIT, pattern S_UNDERBRUSH_DARK
  , groupNamesSingleton, groupNames
    -- * Content
  , content
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Content.ItemKindActor
import Content.ItemKindBlast
import Content.ItemKindEmbed
import Content.ItemKindOrgan
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Definition.Color
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal

-- * Group name patterns

-- Warning, many of these are also sythesized, so typos can happen.

groupNamesSingleton :: [GroupName TileKind]
groupNamesSingleton =
       [S_PILLAR, S_RUBBLE_PILE, S_LAMP_POST, S_TREE_LIT, S_TREE_DARK, S_PULPIT, S_BUSH_LIT, S_FOG_LIT, S_SMOKE_LIT, S_FLOOR_ACTOR_LIT, S_FLOOR_ACTOR_DARK, S_FLOOR_ASHES_LIT, S_FLOOR_ASHES_DARK, S_SHADED_GROUND, S_SHALLOW_WATER_LIT, S_SHALLOW_WATER_DARK]
    ++ [S_POOL_LIT, S_POOL_DARK, S_CLOSED_DOOR, S_OPEN_DOOR, S_OIL_SPILL, S_FROZEN_PATH, S_LIFT_SHAFT, S_REINFORCED_WALL, S_SHUTTLE_HULL, S_HARDWARE_RACK]
    ++ [ S_SIGNBOARD_UNREAD]
    ++ [S_SUSPECT_WALL, S_STAIRCASE_TRAP_DOWN_OIL, S_BURNING_INSTALLATION, S_BURNING_TREE, S_BURNING_BUSH, S_BURNING_UNDERBRUSH, S_BURNING_OIL, S_UNDERBRUSH_LIT, S_UNDERBRUSH_DARK]
    ++ [S_BUSH_DARK]

-- ** Used in PlaceKind, but not in CaveKind.
pattern S_PILLAR, S_RUBBLE_PILE, S_LAMP_POST, S_TREE_LIT, S_TREE_DARK, S_PULPIT, S_BUSH_LIT, S_FOG_LIT, S_SMOKE_LIT, S_FLOOR_ACTOR_LIT, S_FLOOR_ACTOR_DARK, S_FLOOR_ASHES_LIT, S_FLOOR_ASHES_DARK, S_SHADED_GROUND, S_SHALLOW_WATER_LIT, S_SHALLOW_WATER_DARK :: GroupName TileKind

-- ** Allure-specific
pattern S_POOL_LIT, S_POOL_DARK, S_CLOSED_DOOR, S_OPEN_DOOR, S_OIL_SPILL, S_FROZEN_PATH, S_LIFT_SHAFT, S_REINFORCED_WALL, S_SHUTTLE_HULL, S_HARDWARE_RACK :: GroupName TileKind

-- ** Used only internally in other TileKind definitions or never used.
pattern S_SIGNBOARD_UNREAD :: GroupName TileKind

-- ** Allure-specific
pattern S_SUSPECT_WALL, S_STAIRCASE_TRAP_DOWN_OIL, S_BURNING_INSTALLATION, S_BURNING_TREE, S_BURNING_BUSH, S_BURNING_UNDERBRUSH, S_BURNING_OIL, S_UNDERBRUSH_LIT, S_UNDERBRUSH_DARK :: GroupName TileKind

-- * Not used, but needed, because auto-generated. Singletons.
pattern S_BUSH_DARK :: GroupName TileKind

-- TODO: if we stick to the current system of generating extra kinds and their
-- group names, let's also add the generated group names to @groupNames@.
groupNames :: [GroupName TileKind]
groupNames =
       [AQUATIC]
    ++ [FILLER_WALL, FLOOR_CORRIDOR_LIT, FLOOR_CORRIDOR_DARK, TRAIL_LIT, SAFE_TRAIL_LIT, LAB_TRAIL_LIT, DAMP_FLOOR_LIT, DAMP_FLOOR_DARK, DIRT_LIT, DIRT_DARK, FLOOR_ARENA_LIT, FLOOR_ARENA_DARK]
    ++ [HABITAT_CONTAINMENT_WALL, TRANSPORT_ROUTE, ORIELS_FENCE, AIRLOCK_FENCE, EMPTY_AIRLOCK_FENCE, OPENABLE_WALL, TRAPPABLE_WALL, OILY_FLOOR_LIT, OILY_FLOOR_DARK]
    ++ [EMPTY_SET_LIT, EMPTY_SET_DARK, NOISE_SET_LIT, POWER_SET_LIT, POWER_SET_DARK, BATTLE_SET_LIT, BATTLE_SET_DARK, BRAWL_SET_LIT, SHOOTOUT_SET_LIT, ZOO_SET_LIT, ZOO_SET_DARK, ESCAPE_SET_LIT, ESCAPE_SET_DARK, AMBUSH_SET_LIT, AMBUSH_SET_DARK, ARENA_SET_LIT, ARENA_SET_DARK]
    ++ [ROGUE_SET, MUSEUM_SET_LIT, MUSEUM_SET_DARK, HUNT_SET_LIT, EXIT_SET_LIT]
    ++ [TREE_SHADE_WALKABLE_LIT, TREE_SHADE_WALKABLE_DARK, SMOKE_CLUMP_LIT, SMOKE_CLUMP_DARK, BUSH_CLUMP_LIT, BUSH_CLUMP_DARK, FOG_CLUMP_LIT, FOG_CLUMP_DARK, STAIR_TERMINAL_LIT, STAIR_TERMINAL_DARK, SIGNBOARD, STAIRCASE_UP, ORDINARY_STAIRCASE_UP, STAIRCASE_OUTDOOR_UP, GATED_STAIRCASE_UP, STAIRCASE_DOWN, ORDINARY_STAIRCASE_DOWN, STAIRCASE_OUTDOOR_DOWN, GATED_STAIRCASE_DOWN, ESCAPE_UP, ESCAPE_DOWN, ESCAPE_OUTDOOR_DOWN, TRANSPARENT_WALL, ICE_BUILDUP, WORKSHOP, FLOOR_ACTOR_ITEM, FLOOR_ACTOR_ITEM_LIT, FLOOR_ACTOR_ITEM_DARK]
    ++ [BUSH_GROVE_LIT, BUSH_GROVE_DARK, UNDERBRUSH_CLUMP_LIT, UNDERBRUSH_CLUMP_DARK, ASHES_SMOKE_LIT, ASHES_SMOKE_DARK, RECT_WINDOWS, DOORLESS_MACHINERY, PUMPS_LIT, PUMPS_DARK, DOORLESS_WALL, OIL_RESIDUE_LIT, OIL_RESIDUE_DARK, LIFT_TERMINAL_LIT, LIFT_TERMINAL_DARK, STAIRCASE_LIFT_UP, STAIRCASE_LIFT_DOWN, GATED_LIFT_UP, GATED_LIFT_DOWN, DECON_STAIRCASE_UP, DECON_STAIRCASE_DOWN, DECON_LIFT_UP, DECON_LIFT_DOWN, WELDED_STAIRCASE_UP, WELDED_LIFT_UP, ESCAPE_SPACESHIP_DOWN, ORDINARY_LIFT_UP, ORDINARY_LIFT_DOWN, RUBBLE_OR_WASTE_LIT, RUBBLE_OR_WASTE_DARK, CACHE_DEPOSIT, CACHE_JEWELRY, CACHE_MAZE, CACHE_SHUTTLE, TRAPPED_DOOR, STUCK_DOOR, BARREL]
    ++ [TREE_BURNING_OR_NOT, BUSH_BURNING_OR_NOT]
    ++ [OBSCURED_WALL, CACHE_DEPOSIT_OR_NOT, CACHE_DEPOSIT_BREACHED, CACHE_JEWELRY_OR_NOT, CACHE_JEWELRY_TRAPPED_OR_NOT, CACHE_ABANDONED_OR_NOT, RUBBLE_BURNING_OR_NOT]
    ++ [BRAWL_SET_DARK, NOISE_SET_DARK, SHOOTOUT_SET_DARK, EXIT_SET_DARK, HUNT_SET_DARK]

pattern FILLER_WALL, FLOOR_CORRIDOR_LIT, FLOOR_CORRIDOR_DARK, TRAIL_LIT, SAFE_TRAIL_LIT, LAB_TRAIL_LIT, DAMP_FLOOR_LIT, DAMP_FLOOR_DARK, DIRT_LIT, DIRT_DARK, FLOOR_ARENA_LIT, FLOOR_ARENA_DARK :: GroupName TileKind

-- ** Allure-specific
pattern HABITAT_CONTAINMENT_WALL, TRANSPORT_ROUTE, ORIELS_FENCE, AIRLOCK_FENCE, EMPTY_AIRLOCK_FENCE, OPENABLE_WALL, TRAPPABLE_WALL, OILY_FLOOR_LIT, OILY_FLOOR_DARK :: GroupName TileKind

pattern EMPTY_SET_LIT, EMPTY_SET_DARK, NOISE_SET_LIT, POWER_SET_LIT, POWER_SET_DARK, BATTLE_SET_LIT, BATTLE_SET_DARK, BRAWL_SET_LIT, SHOOTOUT_SET_LIT, ZOO_SET_LIT, ZOO_SET_DARK, ESCAPE_SET_LIT, ESCAPE_SET_DARK, AMBUSH_SET_LIT, AMBUSH_SET_DARK, ARENA_SET_LIT, ARENA_SET_DARK :: GroupName TileKind

-- ** Allure-specific
pattern ROGUE_SET, MUSEUM_SET_LIT, MUSEUM_SET_DARK, HUNT_SET_LIT, EXIT_SET_LIT :: GroupName TileKind

-- ** Used in PlaceKind, but not in CaveKind.
pattern TREE_SHADE_WALKABLE_LIT, TREE_SHADE_WALKABLE_DARK, SMOKE_CLUMP_LIT, SMOKE_CLUMP_DARK, BUSH_CLUMP_LIT, BUSH_CLUMP_DARK, FOG_CLUMP_LIT, FOG_CLUMP_DARK, STAIR_TERMINAL_LIT, STAIR_TERMINAL_DARK, SIGNBOARD, STAIRCASE_UP, ORDINARY_STAIRCASE_UP, STAIRCASE_OUTDOOR_UP, GATED_STAIRCASE_UP, STAIRCASE_DOWN, ORDINARY_STAIRCASE_DOWN, STAIRCASE_OUTDOOR_DOWN, GATED_STAIRCASE_DOWN, ESCAPE_UP, ESCAPE_DOWN, ESCAPE_OUTDOOR_DOWN, TRANSPARENT_WALL, ICE_BUILDUP, WORKSHOP, FLOOR_ACTOR_ITEM, FLOOR_ACTOR_ITEM_LIT, FLOOR_ACTOR_ITEM_DARK :: GroupName TileKind

-- ** Allure-specific
pattern BUSH_GROVE_LIT, BUSH_GROVE_DARK, UNDERBRUSH_CLUMP_LIT, UNDERBRUSH_CLUMP_DARK, ASHES_SMOKE_LIT, ASHES_SMOKE_DARK, RECT_WINDOWS, DOORLESS_MACHINERY, PUMPS_LIT, PUMPS_DARK, DOORLESS_WALL, OIL_RESIDUE_LIT, OIL_RESIDUE_DARK, LIFT_TERMINAL_LIT, LIFT_TERMINAL_DARK, STAIRCASE_LIFT_UP, STAIRCASE_LIFT_DOWN, GATED_LIFT_UP, GATED_LIFT_DOWN, DECON_STAIRCASE_UP, DECON_STAIRCASE_DOWN, DECON_LIFT_UP, DECON_LIFT_DOWN, WELDED_STAIRCASE_UP, WELDED_LIFT_UP, ESCAPE_SPACESHIP_DOWN, ORDINARY_LIFT_UP, ORDINARY_LIFT_DOWN, RUBBLE_OR_WASTE_LIT, RUBBLE_OR_WASTE_DARK, CACHE_DEPOSIT, CACHE_JEWELRY, CACHE_MAZE, CACHE_SHUTTLE, TRAPPED_DOOR, STUCK_DOOR, BARREL :: GroupName TileKind

-- ** Used only internally in other TileKind definitions or never used.
pattern TREE_BURNING_OR_NOT, BUSH_BURNING_OR_NOT :: GroupName TileKind

-- ** Allure-specific
pattern OBSCURED_WALL, CACHE_DEPOSIT_OR_NOT, CACHE_DEPOSIT_BREACHED, CACHE_JEWELRY_OR_NOT, CACHE_JEWELRY_TRAPPED_OR_NOT, CACHE_ABANDONED_OR_NOT, RUBBLE_BURNING_OR_NOT :: GroupName TileKind

-- * Not used, but needed, because auto-generated. Not singletons.
pattern BRAWL_SET_DARK, NOISE_SET_DARK, SHOOTOUT_SET_DARK, EXIT_SET_DARK, HUNT_SET_DARK :: GroupName TileKind

-- ** Used in CaveKind and perhaps elsewhere (or a dark/lit version thereof).
pattern FILLER_WALL = GroupName "fillerWall"
pattern FLOOR_CORRIDOR_LIT = GroupName "floorCorridorLit"
pattern FLOOR_CORRIDOR_DARK = GroupName "floorCorridorDark"
pattern TRAIL_LIT = GroupName "trailLit"
pattern SAFE_TRAIL_LIT = GroupName "safeTrailLit"
pattern LAB_TRAIL_LIT = GroupName "labTrailLit"
  -- these three would work without @_LIT@, but it will be needed when
  -- in the future a lit trail is made from terrain that has an autogenerated
  -- dark variant
pattern DAMP_FLOOR_LIT = GroupName "damp floor Lit"
pattern DAMP_FLOOR_DARK = GroupName "damp floor Dark"
pattern DIRT_LIT = GroupName "dirt Lit"
pattern DIRT_DARK = GroupName "dirt Dark"
pattern FLOOR_ARENA_LIT = GroupName "floorArenaLit"
pattern FLOOR_ARENA_DARK = GroupName "floorArenaDark"

-- ** Allure-specific
pattern HABITAT_CONTAINMENT_WALL = GroupName "habitat containment wall"
pattern TRANSPORT_ROUTE = GroupName "transport route"
pattern ORIELS_FENCE = GroupName "oriels fence"
pattern AIRLOCK_FENCE = GroupName "airlock fence"
pattern EMPTY_AIRLOCK_FENCE = GroupName "empty airlock fence"
pattern OPENABLE_WALL = GroupName "openableWall"
pattern TRAPPABLE_WALL = GroupName "trappableWall"
pattern OILY_FLOOR_LIT = GroupName "oily floor Lit"
pattern OILY_FLOOR_DARK = GroupName "oily floor Dark"

-- ** Used in CaveKind and perhaps elsewhere; sets of tiles for filling cave.
pattern EMPTY_SET_LIT = GroupName "emptySetLit"
pattern EMPTY_SET_DARK = GroupName "emptySetDark"
pattern NOISE_SET_LIT = GroupName "noiseSetLit"
pattern POWER_SET_LIT = GroupName "powerSetLit"
pattern POWER_SET_DARK = GroupName "powerSetDark"
pattern BATTLE_SET_LIT = GroupName "battleSetLit"
pattern BATTLE_SET_DARK = GroupName "battleSetDark"
pattern BRAWL_SET_LIT = GroupName "brawlSetLit"
pattern SHOOTOUT_SET_LIT = GroupName "shootoutSetLit"
pattern ZOO_SET_LIT = GroupName "zooSetLit"
pattern ZOO_SET_DARK = GroupName "zooSetDark"
pattern ESCAPE_SET_LIT = GroupName "escapeSetLit"
pattern ESCAPE_SET_DARK = GroupName "escapeSetDark"
pattern AMBUSH_SET_LIT = GroupName "ambushSetLit"
pattern AMBUSH_SET_DARK = GroupName "ambushSetDark"
pattern ARENA_SET_LIT = GroupName "arenaSetLit"
pattern ARENA_SET_DARK = GroupName "arenaSetDark"

-- ** Allure-specific
pattern ROGUE_SET = GroupName "rogueSet"
pattern MUSEUM_SET_LIT = GroupName "museumSetLit"
pattern MUSEUM_SET_DARK = GroupName "museumSetDark"
pattern HUNT_SET_LIT = GroupName "huntSetLit"
pattern EXIT_SET_LIT = GroupName "exitSetLit"

-- ** Used in PlaceKind, but not in CaveKind. Not singletons.
pattern TREE_SHADE_WALKABLE_LIT = GroupName "treeShadeWalkableLit"
pattern TREE_SHADE_WALKABLE_DARK = GroupName "treeShadeWalkableDark"
pattern SMOKE_CLUMP_LIT = GroupName "smokeClumpLit"
pattern SMOKE_CLUMP_DARK = GroupName "smokeClumpDark"
pattern BUSH_CLUMP_LIT = GroupName "bushClumpLit"
pattern BUSH_CLUMP_DARK = GroupName "bushClumpDark"
pattern FOG_CLUMP_LIT = GroupName "fogClumpLit"
pattern FOG_CLUMP_DARK = GroupName "fogClumpDark"
pattern STAIR_TERMINAL_LIT = GroupName "stair terminal Lit"
pattern STAIR_TERMINAL_DARK = GroupName "stair terminal Dark"
pattern SIGNBOARD = GroupName "signboard"
pattern STAIRCASE_UP = GroupName "staircase up"
pattern ORDINARY_STAIRCASE_UP = GroupName "ordinary staircase up"
pattern STAIRCASE_OUTDOOR_UP = GroupName "staircase outdoor up"
pattern GATED_STAIRCASE_UP = GroupName "gated staircase up"
pattern STAIRCASE_DOWN = GroupName "staircase down"
pattern ORDINARY_STAIRCASE_DOWN = GroupName "ordinary staircase down"
pattern STAIRCASE_OUTDOOR_DOWN = GroupName "staircase outdoor down"
pattern GATED_STAIRCASE_DOWN = GroupName "gated staircase down"
pattern ESCAPE_UP = GroupName "escape up"
pattern ESCAPE_DOWN = GroupName "escape down"
pattern ESCAPE_OUTDOOR_DOWN = GroupName "escape outdoor down"
pattern TRANSPARENT_WALL  = GroupName "transparent wall"
pattern ICE_BUILDUP = GroupName "ice buildup"
pattern WORKSHOP = GroupName "workshop"
pattern FLOOR_ACTOR_ITEM = GroupName "floorActorItem"
pattern FLOOR_ACTOR_ITEM_LIT = GroupName "floorActorItemLit"
pattern FLOOR_ACTOR_ITEM_DARK = GroupName "floorActorItemDark"

-- ** Used in PlaceKind, but not in CaveKind. Singletons.
pattern S_PILLAR = GroupName "pillar"
pattern S_RUBBLE_PILE = GroupName "rubble pile"
pattern S_LAMP_POST = GroupName "lamp post"
pattern S_TREE_LIT = GroupName "tree Lit"
pattern S_TREE_DARK = GroupName "tree Dark"
pattern S_PULPIT = GroupName "pulpit"
pattern S_BUSH_LIT = GroupName "bush Lit"
pattern S_FOG_LIT = GroupName "fog Lit"
pattern S_SMOKE_LIT = GroupName "smoke Lit"
pattern S_FLOOR_ACTOR_LIT = GroupName "floor with actors Lit"
pattern S_FLOOR_ACTOR_DARK = GroupName "floor with actors Dark"
pattern S_FLOOR_ASHES_LIT = GroupName "floor with ashes Lit"
pattern S_FLOOR_ASHES_DARK = GroupName "floor with ashes Dark"
pattern S_SHADED_GROUND = GroupName "shaded ground"
pattern S_SHALLOW_WATER_LIT = GroupName "shallow water Lit"
pattern S_SHALLOW_WATER_DARK = GroupName "shallow water Dark"

-- ** Allure-specific
pattern BUSH_GROVE_LIT = GroupName "bushGroveLit"
pattern BUSH_GROVE_DARK = GroupName "bushGroveDark"
pattern UNDERBRUSH_CLUMP_LIT = GroupName "underbrushClumpLit"
pattern UNDERBRUSH_CLUMP_DARK = GroupName "underbrushClumpDark"
pattern ASHES_SMOKE_LIT = GroupName "ashesSmokeLit"
pattern ASHES_SMOKE_DARK = GroupName "ashesSmokeDark"
pattern RECT_WINDOWS = GroupName "rectWindows"
pattern DOORLESS_MACHINERY = GroupName "doorlessMachinery"
pattern PUMPS_LIT = GroupName "pumpsLit"
pattern PUMPS_DARK = GroupName "pumpsDark"
pattern DOORLESS_WALL = GroupName "doorlessWall"
pattern OIL_RESIDUE_LIT = GroupName "oilResidueLit"
pattern OIL_RESIDUE_DARK = GroupName "oilResidueDark"
pattern LIFT_TERMINAL_LIT = GroupName "lift terminal Lit"
pattern LIFT_TERMINAL_DARK = GroupName "lift terminal Dark"
pattern STAIRCASE_LIFT_UP = GroupName "staircase lift up"
pattern STAIRCASE_LIFT_DOWN = GroupName "staircase lift down"
pattern GATED_LIFT_UP = GroupName "gated lift up"
pattern GATED_LIFT_DOWN = GroupName "gated lift down"
pattern DECON_STAIRCASE_UP = GroupName "decon staircase up"
pattern DECON_STAIRCASE_DOWN = GroupName "decon staircase down"
pattern DECON_LIFT_UP = GroupName "decon lift up"
pattern DECON_LIFT_DOWN = GroupName "decon lift down"
pattern WELDED_STAIRCASE_UP = GroupName "welded staircase up"
pattern WELDED_LIFT_UP = GroupName "welded lift up"
pattern ESCAPE_SPACESHIP_DOWN = GroupName "escape spaceship down"
pattern ORDINARY_LIFT_UP = GroupName "ordinary lift up"
pattern ORDINARY_LIFT_DOWN = GroupName "ordinary lift down"
pattern RUBBLE_OR_WASTE_LIT = GroupName "rubbleOrWaste_Lit"
pattern RUBBLE_OR_WASTE_DARK = GroupName "rubbleOrWaste_Dark"
pattern CACHE_DEPOSIT = GroupName "cache deposit"
pattern CACHE_JEWELRY = GroupName "cache jewelry"
pattern CACHE_MAZE = GroupName "cache maze"
pattern CACHE_SHUTTLE = GroupName "cache shuttle"
pattern TRAPPED_DOOR = GroupName "trapped door"
pattern STUCK_DOOR = GroupName "stuck door"
pattern BARREL = GroupName "barrel"

pattern S_POOL_LIT = GroupName "poolLit"
pattern S_POOL_DARK = GroupName "poolDark"
pattern S_CLOSED_DOOR = GroupName "closed door"
pattern S_OPEN_DOOR = GroupName "open door"
pattern S_OIL_SPILL = GroupName "oil spill"
pattern S_FROZEN_PATH = GroupName "frozen path"
pattern S_LIFT_SHAFT = GroupName "lift shaft"
pattern S_REINFORCED_WALL = GroupName "reinforced wall"
pattern S_SHUTTLE_HULL = GroupName "shuttle hull"
pattern S_HARDWARE_RACK = GroupName "hardware rack"

-- ** Used only internally in other TileKind definitions. Not singletons.
pattern TREE_BURNING_OR_NOT = GroupName "tree burning or not"
pattern BUSH_BURNING_OR_NOT = GroupName "bush burning or not"

-- ** Used only internally in other TileKind definitions. Singletons.

pattern S_SIGNBOARD_UNREAD = GroupName "signboard unread"

-- ** Allure-specific
pattern OBSCURED_WALL = GroupName "obscured wall"
pattern CACHE_DEPOSIT_OR_NOT = GroupName "cache deposit or not"
pattern CACHE_DEPOSIT_BREACHED = GroupName "cache deposit breached"
pattern CACHE_JEWELRY_OR_NOT = GroupName "cache jewelry or not"
pattern CACHE_JEWELRY_TRAPPED_OR_NOT = GroupName "cache jewelry trapped or not"
pattern CACHE_ABANDONED_OR_NOT = GroupName "cache abandoned or not"
pattern RUBBLE_BURNING_OR_NOT = GroupName "rubble burning or not"

pattern S_SUSPECT_WALL = GroupName "suspect wall"
pattern S_STAIRCASE_TRAP_DOWN_OIL = GroupName "slippery staircase down"
pattern S_BURNING_INSTALLATION = GroupName "burning installation"
pattern S_BURNING_TREE = GroupName "burning tree"
pattern S_BURNING_BUSH = GroupName "burning bush"
pattern S_BURNING_UNDERBRUSH = GroupName "burning underbrush"
pattern S_BURNING_OIL = GroupName "burning oil"
pattern S_UNDERBRUSH_LIT = GroupName "underbrush Lit"
pattern S_UNDERBRUSH_DARK = GroupName "underbrush Dark"

-- * Not used, but needed, because auto-generated. Not singletons.
-- This is a rotten compromise, because these are synthesized below,
-- so typos can happen. Similarly below
pattern BRAWL_SET_DARK = GroupName "brawlSetDark"
pattern NOISE_SET_DARK = GroupName "noiseSetDark"
pattern SHOOTOUT_SET_DARK = GroupName "shootoutSetDark"
pattern EXIT_SET_DARK = GroupName "exitSetDark"
pattern HUNT_SET_DARK = GroupName "huntSetDark"

-- * Not used, but needed, because auto-generated. Singletons.
pattern S_BUSH_DARK = GroupName "bush Dark"

-- * Content

content :: [TileKind]
content =
  [unknown, unknownOuterFence, basicOuterFence, bedrock, wall, wallSuspect, wallObscured, wallObscuredDefaced, wallObscuredFrescoed, pillar, pillarCache, lampPost, signboardUnread, signboardRead, treeLit, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, pillarIce, pulpit, bushLit, bushBurnt, bushBurning, fog, fogDark, smoke, smokeDark, doorOpen, floorCorridor, floorArena, floorDamp, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorAshes, shallowWater, shallowWaterSpice, shallowWater2, floorRed, floorBlue, floorBrown, floorArenaShade ]
  ++ map makeDarkColor ldarkColorable
  -- Allure-specific
  ++ [oriel, outerHullWall, rubbleBurning, rubbleBurningSpice, wallOpenable, wallObscuredSafety, signboardReadExtinguisher, wallObscured3dBillboard, wallObscuredPipework, wallObscuredScary, liftShaft, rock, pillarCache2, pillarCache3, pillarCache4, pillarCache5, stairsTrappedDownOil, stairsDecontaminatingUp, stairsWelded, stairsLiftUp, stairsLiftTrappedUp, stairsLiftGatedUp, stairsLiftDecontaminatingUp, stairsLiftWelded, stairsDecontaminatingDown, stairsLiftDown, stairsLiftTrappedDown, stairsLiftGatedDown, stairsLiftDecontaminatingDown, escapeSpaceshipDown, emptyAirlock, reinforcedWall, reinforcedWallSpice, wallShuttle, wallShuttleSpice, doorStuck, barrel, barrelSpice, machineWall, machineWallSpice, bushEdible, bushEdibleSpice, underbrushBurning, floorOily, oilSpill, oilSpillSpice, oilBurning, floorWindow, underbrush, workshop]

unknown,    unknownOuterFence, basicOuterFence, bedrock, wall, wallSuspect, wallObscured, wallObscuredDefaced, wallObscuredFrescoed, pillar, pillarCache, lampPost, signboardUnread, signboardRead, treeLit, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, pillarIce, pulpit, bushLit, bushBurnt, bushBurning, fog, fogDark, smoke, smokeDark, doorOpen, floorCorridor, floorArena, floorDamp, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorAshes, shallowWater, shallowWaterSpice, shallowWater2, floorRed, floorBlue, floorBrown, floorArenaShade :: TileKind
-- Allure-specific
oriel,       outerHullWall, rubbleBurning, rubbleBurningSpice, wallOpenable, wallObscuredSafety, signboardReadExtinguisher, wallObscured3dBillboard, wallObscuredPipework, wallObscuredScary, liftShaft, rock, pillarCache2, pillarCache3, pillarCache4, pillarCache5, stairsTrappedDownOil, stairsDecontaminatingUp, stairsWelded, stairsLiftUp, stairsLiftTrappedUp, stairsLiftGatedUp, stairsLiftDecontaminatingUp, stairsLiftWelded, stairsDecontaminatingDown, stairsLiftDown, stairsLiftTrappedDown, stairsLiftGatedDown, stairsLiftDecontaminatingDown, escapeSpaceshipDown, emptyAirlock, reinforcedWall, reinforcedWallSpice, wallShuttle, wallShuttleSpice, doorStuck, barrel, barrelSpice, machineWall, machineWallSpice, bushEdible, bushEdibleSpice, underbrushBurning, floorOily, oilSpill, oilSpillSpice, oilBurning, floorWindow, underbrush, workshop :: TileKind

ldarkColorable :: [TileKind]
ldarkColorable = [treeLit, bushLit, floorCorridor, floorArena, floorDamp, floorDirt, floorDirtSpice, floorActor, floorActorItem, shallowWater, shallowWaterSpice, shallowWater2, floorOily]

-- Symbols to be used:
--         LOS    noLOS
-- Walk    .'~    :;
-- noWalk  %^     #O&<>+
--
-- can be opened ^&+
-- can be closed '
-- some noWalk can be changed without opening, regardless of symbol
-- not used yet:
-- : (curtain, etc., not flowing, but solid and static)
-- ` (not distinct enough from ' and already used for some blasts)

-- White, cyan and green terrain is usually inert, red is burning or trapped,
-- blue activable or trapped, magenta searchable or activable.

-- Note that for AI hints and UI comfort, most multiple-use @Embed@ tiles
-- should have a variant, which after first use transforms into a different
-- colour tile without @ChangeTo@ and similar (which then AI no longer touches).
-- If a tile is supposed to be repeatedly activated by AI (e.g., cache),
-- it should keep @ChangeTo@ for the whole time.

-- * Main tiles, modified for Allure; some removed

-- ** Not walkable

-- *** Not clear

unknown = TileKind  -- needs to have index 0 and alter 1; no other with 1
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [(S_UNKNOWN_SPACE, 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark]
  }
unknownOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [(S_UNKNOWN_OUTER_FENCE, 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
basicOuterFence = TileKind
  { tsymbol  = '#'
  , tname    = "habitat containment wall"
  , tfreq    = [(HABITAT_CONTAINMENT_WALL, 1)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , talter   = maxBound  -- impenetrable
  , tfeature = []
  }
bedrock = TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tfreq    = [ (FILLER_WALL, 1)
               , (ROGUE_SET, 60), (MUSEUM_SET_DARK, 4), (NOISE_SET_LIT, 450)
               , (POWER_SET_DARK, 450), (BATTLE_SET_DARK, 250)
               , (ESCAPE_SET_DARK, 4)
               , (STAIR_TERMINAL_LIT, 100), (STAIR_TERMINAL_DARK, 100)
               , (DOORLESS_WALL, 80), (DOORLESS_MACHINERY, 1) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = []
  }
wall = bedrock  -- fireproof
  { tfreq    = [(TRAPPABLE_WALL, 1), (RECT_WINDOWS, 80)]
  , tfeature = [BuildAs S_SUSPECT_WALL]
  }
wallSuspect = TileKind  -- only on client
  { tsymbol  = '#'
  , tname    = "suspect wall"
  , tfreq    = [(S_SUSPECT_WALL, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs TRAPPED_DOOR
               , ObscureAs OBSCURED_WALL
               ]
  }
wallObscured = TileKind
  { tsymbol  = '#'
  , tname    = "scratched wall"
  , tfreq    = [(OBSCURED_WALL, 25)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed SCRATCH_ON_WALL
               , HideAs S_SUSPECT_WALL
               ]
  }
wallObscuredDefaced = TileKind
  { tsymbol  = '#'
  , tname    = "defaced wall"
  , tfreq    = [ (OBSCURED_WALL, 25), (ESCAPE_SET_DARK, 2)
               , (MUSEUM_SET_DARK, 2) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed OBSCENE_PICTOGRAM
               , HideAs S_SUSPECT_WALL
               ]
  }
wallObscuredFrescoed = TileKind
  { tsymbol  = '#'
  , tname    = "subtle mural"
  , tfreq    = [(OBSCURED_WALL, 5), (MUSEUM_SET_DARK, 2)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed SUBTLE_FRESCO
               , HideAs S_SUSPECT_WALL
               ]  -- a bit beneficial, but AI would loop if allowed to trigger
                  -- so no @ConsideredByAI@
  }
pillar = TileKind
  { tsymbol  = '0'
  , tname    = "construction beam"
  , tfreq    = [(S_PILLAR, 1), (MUSEUM_SET_DARK, 20), (EMPTY_SET_LIT, 60)]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = []
  }
pillarCache = TileKind
  { tsymbol  = '#'
  , tname    = "abandoned stash"
  , tfreq    = [ (CACHE_ABANDONED_OR_NOT, 40)
               , (CACHE_MAZE, 33), (CACHE_SHUTTLE, 25) ]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [ Embed ABANDONED_CACHE
               , ChangeTo CACHE_ABANDONED_OR_NOT, ConsideredByAI ]
      -- Not explorable, but prominently placed, so hard to miss.
      -- Very beneficial, so AI eager to trigger.
  }
lampPost = TileKind
  { tsymbol  = '0'
  , tname    = "lamp post"
  , tfreq    = [(S_LAMP_POST, 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = []  -- embed something and explain how there's often
                   -- artificial ambient light in the habitats, but not in all
                   -- of them and in both cases lamps are used to provide fancy
                   -- (extra) lighting; say how low energy drain, such as
                   -- permanent ambient light, is not a problem due to tech
                   -- and also because it's a tiny fraction of what is needed
                   -- for the ecosystem/life support
  }
signboardUnread = TileKind  -- client only, indicates never used by this faction
  { tsymbol  = '0'
  , tname    = "signboard"
  , tfreq    = [(S_SIGNBOARD_UNREAD, 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [ ConsideredByAI  -- changes after use, so safe for AI, which
                                 -- in this way uses all kinds of signboards
               , RevealAs SIGNBOARD  -- to display as hidden
               ]
  }
signboardRead = TileKind
  { tsymbol  = '0'
  , tname    = "signboard"
  , tfreq    = [ (SIGNBOARD, 80), (EMPTY_SET_LIT, 7)
               , (ARENA_SET_LIT, 1), (ARENA_SET_DARK, 2), (MUSEUM_SET_DARK, 1)
               , (ESCAPE_SET_DARK, 2) ]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [Embed SIGNAGE, HideAs S_SIGNBOARD_UNREAD]
                 -- can't transform or the hidden version would not trigger
                 -- but only reveal the real version; needed extra keystroke
  }
treeLit = TileKind
  { tsymbol  = '0'
  , tname    = "tree"
  , tfreq    = [ (ARENA_SET_LIT, 9), (EMPTY_SET_LIT, 4), (BRAWL_SET_LIT, 140)
               , (SHOOTOUT_SET_LIT, 10), (HUNT_SET_LIT, 10)
               , (ESCAPE_SET_LIT, 35), (ZOO_SET_DARK, 20)
               , (S_TREE_LIT, 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 4
  , tfeature = [ChangeWith ProjYes [(1, FIRE_SOURCE)] S_BURNING_TREE]
  }
treeBurnt = TileKind
  { tsymbol  = '0'
  , tname    = "burnt tree"
  , tfreq    = [(ZOO_SET_DARK, 10), (TREE_BURNING_OR_NOT, 30)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [Dark]  -- even burned too hard to topple
  }
treeBurning = TileKind  -- present in EMPTY_SET_LIT as early light/fire source
  { tsymbol  = '0'
  , tname    = "burning tree"
  , tfreq    = [ (EMPTY_SET_LIT, 2), (ZOO_SET_DARK, 60)
               , (TREE_BURNING_OR_NOT, 70), (S_BURNING_TREE, 1) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = [Embed BIG_FIRE, ChangeTo TREE_BURNING_OR_NOT]
      -- too tall to douse with a fireproof cloth or water; have to break off
      -- and isolate smaller branches and let it smolder out
      -- TODO: breaking the burning tree has more use when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
rubble = TileKind
  { tsymbol  = '&'
  , tname    = "rubble pile"
  , tfreq    = [ (S_RUBBLE_PILE, 1), (RUBBLE_BURNING_OR_NOT, 50)
               , (STAIR_TERMINAL_LIT, 6), (STAIR_TERMINAL_DARK, 6)
               , (LIFT_TERMINAL_LIT, 6), (LIFT_TERMINAL_DARK, 6)
               , (EMPTY_SET_LIT, 12), (EXIT_SET_LIT, 6)
               , (NOISE_SET_LIT, 40), (POWER_SET_DARK, 120)
               , (ZOO_SET_DARK, 150), (AMBUSH_SET_DARK, 3) ]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 4  -- boss can dig through
  , tfeature = [ OpenWith ProjYes [(1, BLAST_SOURCE)] S_FLOOR_ASHES_LIT
                   -- can as well be first, because projectiles can't activate
                   -- embeds with non-zero talter; for non-projectiles,
                   -- if a @BLAST_SOURCE@ item could be found,
                   -- this is a safe way to open rubble, with no loot though
               , Embed RUBBLE
               , OpenTo S_FLOOR_ASHES_LIT ]
      -- Getting the item is risky and, e.g., AI doesn't attempt it.
      -- Also, AI doesn't go out of its way to clear the way for heroes.
      -- Rubble can't be ignited, but burning installation, when doused,
      -- becomes rubble. That's different than with trees and bushes.
  }
rubbleSpice = rubble
  { tfreq    = [ (SMOKE_CLUMP_LIT, 1), (SMOKE_CLUMP_DARK, 1)
               , (RUBBLE_OR_WASTE_LIT, 1), (RUBBLE_OR_WASTE_DARK, 1)
               , (CACHE_DEPOSIT, 33) ]
  , tfeature = Spice : tfeature rubble
  }
doorTrapped = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [(TRAPPED_DOOR, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ ChangeWith ProjNo [(1, WIRECUTTING_TOOL)] S_CLOSED_DOOR
               , Embed DOORWAY_TRAP
               , OpenTo S_OPEN_DOOR
               , HideAs S_SUSPECT_WALL
               ]
  }
doorClosed = TileKind  -- fireproof
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [(S_CLOSED_DOOR, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [ OpenTo S_OPEN_DOOR  -- never hidden
               , OpenWith ProjYes [(1, BLAST_SOURCE)] S_OPEN_DOOR ]
  }
stairsUp = TileKind  -- fireproof
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [(STAIRCASE_UP, 90), (ORDINARY_STAIRCASE_UP, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0  -- very easy stairs, unlike all others; projectiles trigger
  , tfeature = [Embed STAIRS_UP, ConsideredByAI]
  }
stairsTrappedUp = TileKind
  { tsymbol  = '<'
  , tname    = "windy staircase up"
  , tfreq    = [(STAIRCASE_UP, 10)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed STAIRS_UP
               , Embed STAIRS_TRAP_UP
               , ConsideredByAI, ChangeTo ORDINARY_STAIRCASE_UP ]
                 -- AI uses despite the trap; exploration more important
  }
stairsOutdoorUp = stairsUp
  { tfreq    = [(STAIRCASE_OUTDOOR_UP, 1)]
  , talter   = talterForStairs
  , tfeature = [Embed STAIRS_UP_OUTDOOR, ConsideredByAI]
  }
stairsGatedUp = stairsUp
  { tname    = "gated staircase up"
  , tfreq    = [(GATED_STAIRCASE_UP, 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [(STAIRCASE_DOWN, 90), (ORDINARY_STAIRCASE_DOWN, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0  -- very easy stairs, unlike all others; projectiles trigger
  , tfeature = [ ChangeWith ProjYes [(1, OIL_SOURCE)] S_STAIRCASE_TRAP_DOWN_OIL
                   -- even random oil explosions can create this trap
               , Embed STAIRS_DOWN
               , ConsideredByAI ]
  }
stairsTrappedDown = TileKind
  { tsymbol  = '>'
  , tname    = "cracked staircase down"
  , tfreq    = [(STAIRCASE_DOWN, 5)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed STAIRS_DOWN
               , Embed STAIRS_TRAP_DOWN
               , ConsideredByAI, ChangeTo ORDINARY_STAIRCASE_DOWN ]
  }
stairsOutdoorDown = stairsDown
  { tfreq    = [(STAIRCASE_OUTDOOR_DOWN, 1)]
  , talter   = talterForStairs
  , tfeature = [Embed STAIRS_DOWN_OUTDOOR, ConsideredByAI]
  }
stairsGatedDown = stairsDown
  { tname    = "gated staircase down"
  , tfreq    = [(GATED_STAIRCASE_DOWN, 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit hatch up"
  , tfreq    = [(ESCAPE_UP, 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed ESCAPE, ConsideredByAI]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [(ESCAPE_DOWN, 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed ESCAPE, ConsideredByAI]
  }
escapeOutdoorDown = escapeDown
  { tname    = "exit back to town"
  , tfreq    = [(ESCAPE_OUTDOOR_DOWN, 1)]
  }

-- *** Clear

wallGlass = TileKind
  { tsymbol  = '%'
  , tname    = "transparent polymer wall"
  , tfreq    = [(TRANSPARENT_WALL, 1), (MUSEUM_SET_DARK, 8)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 10
  , tfeature = [ BuildAs S_CLOSED_DOOR  -- when ending a corridor, have doors
               , Clear ]
  }
wallGlassSpice = wallGlass
  { tfreq    = [ (RECT_WINDOWS, 20)
               , (CACHE_JEWELRY, 40)
               , (CACHE_JEWELRY_OR_NOT, 60)
               , (CACHE_JEWELRY_TRAPPED_OR_NOT, 60) ]
  , tfeature = Spice : tfeature wallGlass
  }
pillarIce = TileKind
  { tsymbol  = '^'
  , tname    = "ice buildup"
  , tfreq    = [ (ICE_BUILDUP, 1), (NOISE_SET_LIT, 200)
               , (BRAWL_SET_LIT, 15), (LIFT_TERMINAL_DARK, 4) ]
                 -- ice only in dark staircases
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 4  -- boss can dig through
  , tfeature = [ Clear
               , Embed FROST
               , OpenTo S_SHALLOW_WATER_LIT
               , OpenWith ProjYes [(1, BLAST_SOURCE)] DAMP_FLOOR_LIT ]
  }
pulpit = TileKind
  { tsymbol  = '%'
  , tname    = "VR booth"
  , tfreq    = [(S_PULPIT, 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 5
  , tfeature = [ ChangeWith ProjYes [(1, FIRE_SOURCE)] S_BURNING_INSTALLATION
                   -- wastes the loot; cruel, but rare; usually player's fault
               , Embed LECTERN
               , Clear ]
                 -- mixed blessing, so AI ignores, saved for player's fun
  }
bushLit = TileKind
  { tsymbol  = '%'
  , tname    = "bush"
  , tfreq    = [ (S_BUSH_LIT, 1), (EMPTY_SET_LIT, 8), (ARENA_SET_LIT, 13)
               , (BRAWL_SET_LIT, 5), (SHOOTOUT_SET_LIT, 30), (HUNT_SET_LIT, 30)
               , (ESCAPE_SET_LIT, 40), (ZOO_SET_DARK, 100)
               , (BUSH_CLUMP_LIT, 2), (BUSH_CLUMP_DARK, 2)  -- always lit
               , (BUSH_GROVE_LIT, 5), (BUSH_GROVE_DARK, 5)  -- always lit
               , (PUMPS_LIT, 300)  -- dark in PUMPS_DARK
               , (LIFT_TERMINAL_LIT, 4) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 4
  , tfeature = [ ChangeWith ProjYes [(1, FIRE_SOURCE)] S_BURNING_BUSH
               , Clear ]
                 -- too tough to topple, has to be burned first
  }
bushBurnt = TileKind
  { tsymbol  = '%'
  , tname    = "burnt bush"
  , tfreq    = [ (BATTLE_SET_DARK, 30), (AMBUSH_SET_DARK, 3)
               , (ZOO_SET_DARK, 100), (BUSH_BURNING_OR_NOT, 50) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [Dark, Clear, OpenTo DIRT_DARK]
  }
bushBurning = TileKind
  { tsymbol  = '%'
  , tname    = "burning bush"
  , tfreq    = [ (EMPTY_SET_LIT, 3), (AMBUSH_SET_DARK, 10), (ZOO_SET_DARK, 300)
               , (BUSH_BURNING_OR_NOT, 50), (S_BURNING_BUSH, 1) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = [ Clear
               , Embed SMALL_FIRE_5
                   -- crafting via embed first, transformation a fallback
               , OpenWith ProjYes [(3, WATER_SOURCE)] S_SMOKE_LIT
               , ChangeWith ProjNo [(1, FIREPROOF_CLOTH)] S_BUSH_LIT
                   -- full effects experienced, but bush saved for repeat
               , ChangeTo BUSH_BURNING_OR_NOT ]
  }

-- ** Walkable

-- *** Not clear

fog = TileKind  -- always lit
  { tsymbol  = ';'
  , tname    = "faint fog"
  , tfreq    = [ (S_FOG_LIT, 1), (EMPTY_SET_LIT, 200), (NOISE_SET_LIT, 120)
               , (SHOOTOUT_SET_LIT, 30), (HUNT_SET_LIT, 30)
               , (FOG_CLUMP_LIT, 60), (FOG_CLUMP_DARK, 60)
               , (LIFT_TERMINAL_LIT, 20) ]
      -- lit fog is OK for shootout, because LOS is mutual, as opposed
      -- to dark fog, and so camper has little advantage, especially
      -- on big maps, where he doesn't know on which side of fog patch to hide
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = [Walkable, NoItem, OftenActor]
  }
fogDark = fog  -- always dark
  { tname    = "thick fog"
  , tfreq    = [(ESCAPE_SET_DARK, 50), (LIFT_TERMINAL_DARK, 40)]
  , tfeature = Dark : tfeature fog
  }
smoke = TileKind  -- always lit
  { tsymbol  = ';'
  , tname    = "billowing smoke"
  , tfreq    = [ (S_SMOKE_LIT, 1), (LAB_TRAIL_LIT, 1)
               , (STAIR_TERMINAL_LIT, 2), (LIFT_TERMINAL_LIT, 6)
               , (SMOKE_CLUMP_LIT, 3), (SMOKE_CLUMP_DARK, 3)
               , (ASHES_SMOKE_LIT, 1), (ASHES_SMOKE_DARK, 1)
               , (EXIT_SET_LIT, 20), (AMBUSH_SET_DARK, 20) ]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 0
  , tfeature = [Walkable, NoItem]  -- not dark, embers
  }
smokeDark = smoke  -- always dark
  { tname    = "lingering smoke"
  , tfreq    = [ (POWER_SET_DARK, 100)
               , (ZOO_SET_DARK, 20), (AMBUSH_SET_DARK, 40), (BATTLE_SET_DARK, 5)
               , (STAIR_TERMINAL_DARK, 2), (LIFT_TERMINAL_DARK, 6) ]
  , tfeature = Dark : tfeature smoke
  }

-- *** Clear

doorOpen = TileKind  -- fireproof
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [(S_OPEN_DOOR, 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo S_CLOSED_DOOR  -- not explorable due to that
               ]
  }
floorCorridor = TileKind
  { tsymbol  = floorSymbol
  , tname    = "floor"
  , tfreq    = [(FLOOR_CORRIDOR_LIT, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0
  , tfeature = [Walkable, Clear]  -- porous, so spilling doesn't transform
  }
floorArena = floorCorridor
  { tfreq    = [ (FLOOR_ARENA_LIT, 1), (ARENA_SET_LIT, 400)
               , (MUSEUM_SET_LIT, 400), (NOISE_SET_LIT, 50), (POWER_SET_LIT, 50)
               , (EMPTY_SET_LIT, 400), (EXIT_SET_LIT, 100), (ZOO_SET_LIT, 500) ]
  }
floorDamp = floorArena
  { tname    = "damp floor"
  , tfreq    = [ (NOISE_SET_LIT, 550), (EMPTY_SET_LIT, 3000)
               , (ESCAPE_SET_LIT, 300), (DAMP_FLOOR_LIT, 1)
               , (STAIR_TERMINAL_LIT, 20), (LIFT_TERMINAL_LIT, 6) ]
  , tfeature = ChangeWith ProjYes [(1, OIL_SOURCE)] S_OIL_SPILL  -- oil floats
               : ChangeWith ProjYes [(1, COLD_SOURCE)] S_FROZEN_PATH
               : ChangeWith ProjYes [(5, WATER_SOURCE)] S_SHALLOW_WATER_LIT
               : tfeature floorArena
  }
floorDirt = floorArena
  { tname    = "dirt floor"
  , tfreq    = [ (BRAWL_SET_LIT, 900), (SHOOTOUT_SET_LIT, 900)
               , (HUNT_SET_LIT, 900), (ESCAPE_SET_LIT, 700)
               , (AMBUSH_SET_LIT, 1000), (BATTLE_SET_LIT, 500)
               , (DIRT_LIT, 1) ]
  }
floorDirtSpice = floorDirt
  { tfreq    = [ (TREE_SHADE_WALKABLE_LIT, 1), (BUSH_CLUMP_LIT, 1)
               , (UNDERBRUSH_CLUMP_LIT, 1), (PUMPS_LIT, 100) ]
  , tfeature = Spice : tfeature floorDirt
  }
floorActor = floorArena
  { tfreq    = [(S_FLOOR_ACTOR_LIT, 1)]
  , tfeature = OftenActor : tfeature floorArena
  }
floorActorItem = floorActor
  { tfreq    = [(FLOOR_ACTOR_ITEM, 1), (FLOOR_ACTOR_ITEM_LIT, 1)]
  , tfeature = VeryOftenItem : tfeature floorActor
  }
floorAshes = floorActor  -- always lit
  { tfreq    = [ (SMOKE_CLUMP_LIT, 1), (SMOKE_CLUMP_DARK, 1)
               , (S_FLOOR_ASHES_LIT, 1), (S_FLOOR_ASHES_DARK, 1)
               , (ASHES_SMOKE_LIT, 2), (ASHES_SMOKE_DARK, 5)
               , (RUBBLE_BURNING_OR_NOT, 25) ]
  , tname    = "dirt and ash pile"
  , tcolor   = Brown
  , tcolor2  = Brown
  }
shallowWater = TileKind
  { tsymbol  = '~'
  , tname    = "water puddle"
  , tfreq    = [ (AQUATIC, 1), (S_SHALLOW_WATER_LIT, 1)
               , (EMPTY_SET_LIT, 20), (NOISE_SET_LIT, 30), (SHOOTOUT_SET_LIT, 5)
               , (HUNT_SET_LIT, 250), (LIFT_TERMINAL_LIT, 4) ]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 2  -- projectiles won't trigger embeds
  , tfeature = Embed SHALLOW_WATER
                 -- crafting via embed first, transformations a fallback
               : ChangeWith ProjYes [(1, OIL_SOURCE)] S_OIL_SPILL  -- oil floats
               : ChangeWith ProjYes [(1, COLD_SOURCE)] S_FROZEN_PATH
               : tfeature floorActor
      -- can't make fog from water, because air would need to be cool, too;
      -- if concealment needed, make smoke from fire instead
  }
shallowWaterSpice = shallowWater
  { tfreq    = [ (FOG_CLUMP_LIT, 40), (PUMPS_LIT, 200)
               , (RUBBLE_OR_WASTE_LIT, 1) ]
  , tfeature = Spice : tfeature shallowWater
  }
shallowWater2 = shallowWater
  { tname    = "water pool"
  , tfreq    = [(S_POOL_LIT, 1)]
  }
floorRed = floorCorridor  -- always lit
  { tname    = "emergency walkway"
  , tfreq    = [(TRAIL_LIT, 50), (SAFE_TRAIL_LIT, 50)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = [ ChangeWith ProjYes [(1, OIL_SOURCE)] S_OIL_SPILL
                   -- non-porous enough
               , Embed STRAIGHT_PATH
               , Trail, Walkable, Clear ]
  }
floorBlue = floorRed  -- always lit
  { tname    = "frozen path"
  , tfreq    = [(TRAIL_LIT, 50), (S_FROZEN_PATH, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 0
  , tfeature = [ ChangeWith ProjYes [(1, FIRE_SOURCE)] S_SHALLOW_WATER_LIT
               , ChangeWith ProjYes [(1, OIL_SOURCE)] S_OIL_SPILL
                   -- non-porous enough
               , Embed FROZEN_GROUND
               , Trail, Walkable, Clear ]
  }
floorBrown = floorRed  -- always lit
  { tname    = "transport route"
  , tfreq    = [ (TRAIL_LIT, 50), (SAFE_TRAIL_LIT, 50)
               , (TRANSPORT_ROUTE, 1) ]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
floorArenaShade = floorActor  -- always dark
  { tname    = "shaded ground"
  , tfreq    = [ (S_SHADED_GROUND, 1)
               , (TREE_SHADE_WALKABLE_LIT, 2), (TREE_SHADE_WALKABLE_DARK, 2)]
  , tcolor   = BrYellow  -- match others, even though no lit counterpart
  , tcolor2  = BrBlack
  , tfeature = Dark : NoItem : tfeature floorActor
  }

-- * Allure-specific

-- ** Not walkable

-- *** Not clear

oriel = TileKind
  { tsymbol  = '%'  -- story-wise it's transparent, hence the symbol
  , tname    = "oriel"
  , tfreq    = [ (ORIELS_FENCE, 15)
               , (AIRLOCK_FENCE, 5), (EMPTY_AIRLOCK_FENCE, 5) ]
  , tcolor   = White
  , tcolor2  = Black
  , talter   = 5
  , tfeature = [Embed BLACK_STARRY_SKY, Dark]
  }
outerHullWall = basicOuterFence
  { tname    = "outer hull wall"
  , tfreq    = [ (S_BASIC_OUTER_FENCE, 1), (ORIELS_FENCE, 85)
               , (AIRLOCK_FENCE, 40), (EMPTY_AIRLOCK_FENCE, 40) ]
  }
rubbleBurning = TileKind  -- present in EMPTY_SET_LIT as early light/fire source
  { tsymbol  = '&'
  , tname    = "burning installation"
  , tfreq    = [ (EMPTY_SET_LIT, 2), (POWER_SET_DARK, 20)
               , (AMBUSH_SET_DARK, 15), (ZOO_SET_DARK, 30)
               , (STAIR_TERMINAL_LIT, 4), (STAIR_TERMINAL_DARK, 4)
               , (LIFT_TERMINAL_LIT, 4), (LIFT_TERMINAL_DARK, 4)
               , (S_BURNING_INSTALLATION, 1), (RUBBLE_BURNING_OR_NOT, 25) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 4  -- boss can dig through
  , tfeature = [ OpenWith ProjYes [(3, WATER_SOURCE)] S_SMOKE_LIT
               , OpenWith ProjYes [(1, BLAST_SOURCE)] ASHES_SMOKE_LIT
               , Embed BIG_FIRE  -- not as tall as a tree, so quenchable
               , ChangeWith ProjNo [(1, FIREPROOF_CLOTH)] S_RUBBLE_PILE
                   -- full effects experienced, but rubble saved for repeat
               , OpenWith ProjNo [] RUBBLE_BURNING_OR_NOT ]
                   -- no pathfinding through
  }
rubbleBurningSpice = rubbleBurning
  { tfreq    = [ (SMOKE_CLUMP_LIT, 1), (SMOKE_CLUMP_DARK, 1)
               , (CACHE_DEPOSIT, 33) ]
  , tfeature = Spice : tfeature rubbleBurning
  }
wallOpenable = bedrock
  { tfreq    = [(OPENABLE_WALL, 1)]
  , tfeature = [BuildAs S_CLOSED_DOOR]  -- when ending a corridor, have doors
  }
wallObscuredSafety = TileKind
  { tsymbol  = '#'
  , tname    = "safety procedures board"
  , tfreq    = [(OBSCURED_WALL, 4), (EXIT_SET_LIT, 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed RUINED_FIRST_AID_KIT
               , HideAs S_SUSPECT_WALL
               ]
  }
signboardReadExtinguisher = TileKind
  { tsymbol  = '0'
  , tname    = "fire extinguisher cabinet"
  , tfreq    = [ (SIGNBOARD, 20)
               , (LIFT_TERMINAL_LIT, 10), (LIFT_TERMINAL_DARK, 10)
               , (EMPTY_SET_LIT, 7), (ARENA_SET_LIT, 1), (ARENA_SET_DARK, 2)
               , (MUSEUM_SET_DARK, 1), (EXIT_SET_LIT, 1), (NOISE_SET_LIT, 1)
               , (ESCAPE_SET_DARK, 1), (AMBUSH_SET_DARK, 1) ]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [ Embed FIRE_FIGHTING_GEAR
               , HideAs S_SIGNBOARD_UNREAD
               ]
  }
wallObscured3dBillboard = TileKind
  { tsymbol  = '#'
  , tname    = "3D billboard"
  , tfreq    = [(OBSCURED_WALL, 15)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed DISPLAY_3D
               , HideAs S_SUSPECT_WALL
               ]
  }
wallObscuredPipework = TileKind
  { tsymbol  = '#'
  , tname    = "exposed pipework"
  , tfreq    = [(OBSCURED_WALL, 25)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed CRACKED_FLUE
               , HideAs S_SUSPECT_WALL
               ]
  }
wallObscuredScary = TileKind
  { tsymbol  = '#'
  , tname    = "stained wall"
  , tfreq    = [(OBSCURED_WALL, 35)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed BLOOD_ON_WALL
               , HideAs S_SUSPECT_WALL
               ]
  }
liftShaft = pillar
  { tname    = "lift shaft"
  , tfreq    = [(S_LIFT_SHAFT, 1)]
  }
rock = pillar
  { tname    = "rock outcrop"
  , tfreq    = [(ARENA_SET_LIT, 6), (BRAWL_SET_LIT, 30)]
  }
pillarCache2 = pillarCache
  { tname    = "rack of deposit boxes"
  , tfreq    = [ (CACHE_DEPOSIT_OR_NOT, 33)
               , (CACHE_DEPOSIT_BREACHED, 1) ]
  , tfeature = [ Embed DEPOSIT_BOX
               , ChangeTo CACHE_DEPOSIT_OR_NOT
               , ConsideredByAI ]
  }
pillarCache3 = pillarCache
  { tname    = "rack of sealed deposit boxes"
  , tfreq    = [ (CACHE_DEPOSIT, 33), (ARENA_SET_DARK, 4)
               , (STAIR_TERMINAL_LIT, 1), (STAIR_TERMINAL_DARK, 1) ]
  , tfeature = [ ChangeWith ProjNo [(1, BREACHING_TOOL)] CACHE_DEPOSIT_BREACHED
                   -- @BLAST_SOURCE@ not enough
               , ConsideredByAI ]
  }
pillarCache4 = pillarCache
  { tname    = "jewelry display"
  , tfreq    = [(CACHE_JEWELRY_OR_NOT, 40)]
  , tfeature = [ Embed JEWELRY_CASE
               , ChangeTo CACHE_JEWELRY_OR_NOT
               , ConsideredByAI ]
  }
pillarCache5 = pillarCache
  { tname    = "jewelry display"
  , tfreq    = [ (CACHE_JEWELRY_TRAPPED_OR_NOT, 40), (CACHE_JEWELRY, 60)
               , (MUSEUM_SET_DARK, 2) ]
  , tfeature = [ Embed JEWELRY_CASE
               , ChangeWith ProjNo [(1, COLD_SOURCE)]
                            CACHE_JEWELRY_TRAPPED_OR_NOT
                   -- halts watchdog
               , ChangeWith ProjNo [(1, WIRECUTTING_TOOL)] CACHE_JEWELRY_OR_NOT
                   -- disarms trap altogether
               , Embed JEWELRY_DISPLAY_TRAP
               , ChangeTo CACHE_JEWELRY_TRAPPED_OR_NOT ]
               -- not ConsideredByAI, to leave the risk and reward to the player
  }
stairsTrappedDownOil = TileKind
  { tsymbol  = '>'
  , tname    = "slippery staircase down"
  , tfreq    = [(STAIRCASE_DOWN, 5), (S_STAIRCASE_TRAP_DOWN_OIL, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ ChangeWith ProjNo [(1, THICK_CLOTH)] ORDINARY_STAIRCASE_DOWN
                   -- safely soaks oil; marginal --- no warning if fails
               , Embed STAIRS_DOWN
               , Embed STAIRS_TRAP_DOWN_OIL
               , ChangeTo ORDINARY_STAIRCASE_DOWN
               , ConsideredByAI ]
 }
stairsDecontaminatingUp = stairsUp
  { tname    = "decontaminating staircase up"
  , tfreq    = [(DECON_STAIRCASE_UP, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = talterForStairs
  , tfeature = Embed DECONTAMINATION_CHAMBER : tfeature stairsUp
  }
stairsWelded = stairsUp
  { tname    = "staircase up welded shut"
  , tfreq    = [(WELDED_STAIRCASE_UP, 1)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  , talter   = talterForStairs + 3  -- gear or level up needed
  , tfeature = [ Embed S_CRUDE_WELD
                   -- the embed goes first, because the embed is marginal here
               , ChangeWith ProjNo [(1, COLD_SOURCE)] ORDINARY_STAIRCASE_UP
               , ChangeWith ProjNo [(1, BLOWTORCH)] ORDINARY_STAIRCASE_UP
               , ConsideredByAI ]
  }
stairsLiftUp = stairsUp  -- fireproof
  { tname    = "lift up"
  , tfreq    = [(STAIRCASE_LIFT_UP, 9), (ORDINARY_LIFT_UP, 1)]
  , talter   = talterForStairs
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , tfeature = [Embed LIFT_UP, ConsideredByAI]
  }
stairsLiftTrappedUp = stairsTrappedUp
  { tname    = "corroded lift up"
  , tfreq    = [(STAIRCASE_LIFT_UP, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = [ Embed LIFT_UP
               , Embed LIFT_TRAP
               , ConsideredByAI, ChangeTo ORDINARY_LIFT_UP ]
                 -- AI uses despite the trap; exploration more important
  }
stairsLiftGatedUp = stairsLiftUp
  { tname    = "manually opened lift up"
  , tfreq    = [(GATED_LIFT_UP, 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
stairsLiftDecontaminatingUp = stairsLiftUp
  { tname    = "decontaminating lift up"
  , tfreq    = [(DECON_LIFT_UP, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = Embed DECONTAMINATION_CHAMBER : tfeature stairsLiftUp
  }
stairsLiftWelded = stairsLiftUp
  { tname    = "lift up welded shut"
  , tfreq    = [(WELDED_LIFT_UP, 1)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  , talter   = talterForStairs + 3  -- gear or level up needed
  , tfeature = [ Embed S_CRUDE_WELD
                   -- the embed goes first, because marginal
               , ChangeWith ProjNo [(1, COLD_SOURCE)] ORDINARY_LIFT_UP
               , ChangeWith ProjNo [(1, BLOWTORCH)] ORDINARY_LIFT_UP
               , ConsideredByAI ]
  }
stairsDecontaminatingDown = stairsDown
  { tname    = "decontaminating staircase down"
  , tfreq    = [(DECON_STAIRCASE_DOWN, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = talterForStairs
  , tfeature = Embed DECONTAMINATION_CHAMBER : tfeature stairsDown
  }
stairsLiftDown = stairsDown
  { tname    = "lift down"
  , tfreq    = [(STAIRCASE_LIFT_DOWN, 9), (ORDINARY_LIFT_DOWN, 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = talterForStairs
  , tfeature = [Embed LIFT_DOWN, ConsideredByAI]
  }
stairsLiftTrappedDown = stairsTrappedDown
  { tname    = "corroded lift down"
  , tfreq    = [(STAIRCASE_LIFT_DOWN, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = [ Embed LIFT_DOWN
               , Embed LIFT_TRAP
               , ConsideredByAI, ChangeTo ORDINARY_LIFT_DOWN ]
  }
stairsLiftGatedDown = stairsLiftDown
  { tname    = "manually opened lift down"
  , tfreq    = [(GATED_LIFT_DOWN, 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
stairsLiftDecontaminatingDown = stairsLiftDown
  { tname    = "decontaminating lift down"
  , tfreq    = [(DECON_LIFT_DOWN, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = Embed DECONTAMINATION_CHAMBER : tfeature stairsLiftDown
  }
escapeSpaceshipDown = escapeDown
  { tname    = "airlock to a shuttle"
  , tfreq    = [(ESCAPE_SPACESHIP_DOWN, 1), (AIRLOCK_FENCE, 3)]
  }
emptyAirlock = escapeDown
  { tname    = "empty airlock"
  , tfreq    = [ (AIRLOCK_FENCE, 2), (EMPTY_AIRLOCK_FENCE, 7)
               , (EMPTY_SET_LIT, 8), (AMBUSH_SET_DARK, 7) ]
                   -- not in exitSetLit; space can't be seen
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = [Embed DISENGAGED_DOCKING_GEAR]
  }
reinforcedWall = TileKind
  { tsymbol  = '#'
  , tname    = "reinforced wall"
  , tfreq    = [(S_REINFORCED_WALL, 1), (ROGUE_SET, 15), (EXIT_SET_LIT, 20)]
  , tcolor   = White
  , tcolor2  = BrBlack
  , talter   = 100
  , tfeature = []
  }
reinforcedWallSpice = reinforcedWall
  { tfreq    = [ (DOORLESS_WALL, 20), (CACHE_MAZE, 33)
               , (CACHE_ABANDONED_OR_NOT, 60), (CACHE_DEPOSIT_OR_NOT, 66) ]
  , tfeature = Spice : tfeature reinforcedWall
  }
wallShuttle = bedrock
  { tname    = "shuttle hull"
  , tfreq    = [(S_SHUTTLE_HULL, 1)]
  , tfeature = [Embed SHUTTLE_HARDWARE]
  }
wallShuttleSpice = wallShuttle
  { tfreq    = [(CACHE_SHUTTLE, 75)]
  , tfeature = Spice : tfeature wallShuttle
  }
doorStuck = TileKind
  { tsymbol  = '+'
  , tname    = "stuck door"
  , tfreq    = [(STUCK_DOOR, 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 2
  , tfeature = [ OpenWith ProjNo [(1, BREACHING_TOOL)] S_OPEN_DOOR
               , OpenWith ProjYes [(1, BLAST_SOURCE)] S_OPEN_DOOR
               , Embed DOOR_TRAP_PUSH
               , OpenWith ProjNo [] S_OPEN_DOOR ]  -- no pathfinding
  }
barrel = TileKind
  { tsymbol  = '0'
  , tname    = "large barrel"
  , tfreq    = [ (BARREL, 1)
               , (EXIT_SET_LIT, 3), (NOISE_SET_LIT, 20), (POWER_SET_DARK, 50)
               , (ZOO_SET_DARK, 30), (AMBUSH_SET_DARK, 2) ]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 0  -- projectiles can trigger the embed
  , tfeature = [ Embed BARREL_CONTENTS
               , OpenWith ProjYes [] ASHES_SMOKE_LIT ]
                   -- no pathfinding through
  }
barrelSpice = barrel
  { tfreq    = [ (SMOKE_CLUMP_LIT, 1), (SMOKE_CLUMP_DARK, 1)
               , (RUBBLE_OR_WASTE_LIT, 2), (RUBBLE_OR_WASTE_DARK, 2)
               , (CACHE_MAZE, 33) ]
  , tfeature = Spice : tfeature barrel
  }

-- *** Clear

machineWall = TileKind
  { tsymbol  = '%'
  , tname    = "hardware rack"
  , tfreq    = [ (S_HARDWARE_RACK, 1)
               , (ROGUE_SET, 25), (NOISE_SET_LIT, 250), (POWER_SET_DARK, 250)
               , (EXIT_SET_LIT, 30)
               , (LIFT_TERMINAL_LIT, 40), (LIFT_TERMINAL_DARK, 40) ]
  , tcolor   = White
  , tcolor2  = BrBlack
  , talter   = 100
  , tfeature = [Clear]
  }
machineWallSpice = machineWall
  { tfreq    = [(DOORLESS_MACHINERY, 1)]
  , tfeature = Spice : tfeature machineWall
  }
bushEdible = TileKind
  { tsymbol  = '%'
  , tname    = "ripe bush"
  , tfreq    = [ (EMPTY_SET_LIT, 4), (ARENA_SET_LIT, 2), (ARENA_SET_DARK, 4)
               , (SHOOTOUT_SET_LIT, 1), (HUNT_SET_LIT, 1)
               , (ESCAPE_SET_DARK, 4), (ZOO_SET_DARK, 1)
               , (LIFT_TERMINAL_LIT, 1), (LIFT_TERMINAL_DARK, 1) ]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  , talter   = 4
  , tfeature = [ Clear
               , Embed EDIBLE_PLANT_RIPE
                   -- loot granted even when ignited, but missiles can't reap
               , ChangeWith ProjYes [(1, FIRE_SOURCE)] S_BURNING_BUSH
               , ChangeTo S_BUSH_LIT ]
  }
bushEdibleSpice = bushEdible
  { tfreq    = [ (BUSH_GROVE_LIT, 1), (BUSH_GROVE_DARK, 1)
               , (PUMPS_LIT, 100), (PUMPS_DARK, 100) ]
  , tfeature = Spice : tfeature bushEdible
  }

-- ** Walkable

-- *** Not clear

underbrushBurning = TileKind  -- always lit
  { tsymbol  = ';'
  , tname    = "burning underbrush"
  , tfreq    = [ (AMBUSH_SET_DARK, 1), (ZOO_SET_DARK, 5)
               , (S_BURNING_UNDERBRUSH, 1) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2  -- due to this, ordinary projectiles can't put out the fire
                  -- and don't get burned
  , tfeature = [ Walkable, NoItem, NoActor  -- not clear, due to smoke
               , Embed SMALL_FIRE  -- little mass, so one fire only
               , ChangeWith ProjYes [(1, WATER_SOURCE)] S_SMOKE_LIT
                   -- transformation only a fallback if crafting unintended
               , ChangeWith ProjNo [(1, FIREPROOF_CLOTH)] S_UNDERBRUSH_LIT
                   -- underbrush saved for repeat
               , ChangeTo S_FLOOR_ASHES_LIT ]  -- not enough matter for smoke
  }

-- *** Clear

floorOily = floorArena
  { tname    = "oily floor"
  , tfreq    = [ (MUSEUM_SET_LIT, 40), (POWER_SET_LIT, 550), (EXIT_SET_LIT, 800)
               , (BATTLE_SET_LIT, 1000)
               , (OILY_FLOOR_LIT, 1), (RUBBLE_OR_WASTE_LIT, 1)
               , (OIL_RESIDUE_LIT, 4) ]
  , tfeature = ChangeWith ProjYes [(1, OIL_SOURCE)] S_OIL_SPILL
                 -- non-porous enough
               : tfeature floorArena
  }
oilSpill = TileKind  -- always lit
  { tsymbol  = '~'
  , tname    = "oil spill"
  , tfreq    = [ (POWER_SET_DARK, 35), (EXIT_SET_LIT, 9)
               , (AMBUSH_SET_DARK, 20), (S_OIL_SPILL, 1) ]
  , tcolor   = BrYellow
  , tcolor2  = BrGreen
  , talter   = 2  -- projectiles won't trigger embeds; doesn't matter
                  -- for others, because walkable;
                  -- TODO: not everything should be able/willing to enter
  , tfeature = [ Embed OIL_PUDDLE
                   -- embed goes first, because transformation and crafting
                   -- may use the same item and crafting needs priority
               , ChangeWith ProjYes [(1, FIRE_SOURCE)] S_BURNING_OIL
               , ChangeWith ProjNo [(1, THICK_CLOTH)] OILY_FLOOR_LIT
               , Walkable, Clear ]
  }
oilSpillSpice = oilSpill
  { tfreq    = [ (RUBBLE_OR_WASTE_LIT, 1), (RUBBLE_OR_WASTE_DARK, 1)
               , (OIL_RESIDUE_LIT, 1), (OIL_RESIDUE_DARK, 1) ]
  , tfeature = Spice : tfeature oilSpill
  }

oilBurning = TileKind  -- always lit
  { tsymbol  = '~'
  , tname    = "burning oil"
  , tfreq    = [(POWER_SET_DARK, 1), (AMBUSH_SET_DARK, 1), (S_BURNING_OIL, 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2  -- due to this, projectiles can't put out the fire
                  -- and don't get burned
  , tfeature = [ Walkable, Clear  -- clear, no smoke, as in oil lamps
               , NoItem, NoActor
               , Embed SMALL_FIRE_5
               , Embed OIL_PUDDLE
               , ChangeWith ProjNo [(1, FIREPROOF_CLOTH)] OILY_FLOOR_LIT
                   -- safely soaks oil, if crafting fails or unintended
               , ChangeTo S_OIL_SPILL ]
                   -- TODO: change only after all 5 fires used up
  }
floorWindow = floorArena  -- always lit
  { tsymbol  = ' '  -- story-wise it's transparent, hence the symbol
  , tname    = "floor window"
  , tfreq    = [(EMPTY_SET_LIT, 24)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , tfeature = Embed BLACK_STARRY_SKY : tfeature floorCorridor
  }
underbrush = TileKind  -- always lit
  { tsymbol  = floorSymbol
  , tname    = "underbrush"
  , tfreq    = [ (S_UNDERBRUSH_LIT, 1), (S_UNDERBRUSH_DARK, 1)
               , (UNDERBRUSH_CLUMP_LIT, 1), (UNDERBRUSH_CLUMP_DARK, 1)
               , (EMPTY_SET_LIT, 200), (ARENA_SET_LIT, 80), (BRAWL_SET_LIT, 100)
               , (SHOOTOUT_SET_LIT, 100), (HUNT_SET_LIT, 100)
               , (ESCAPE_SET_LIT, 100), (ZOO_SET_DARK, 5)
               , (AMBUSH_SET_DARK, 20)
               , (BUSH_CLUMP_LIT, 1), (BUSH_CLUMP_DARK, 1)
               , (TRAIL_LIT, 50), (SAFE_TRAIL_LIT, 50) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 0
  , tfeature = [ ChangeWith ProjYes [(1, FIRE_SOURCE)] S_BURNING_UNDERBRUSH
               , Trail, Walkable, Clear, NoItem ]
  }
workshop = TileKind  -- always lit
  { tsymbol  = ':'
  , tname    = "workshop"
  , tfreq    = [ (WORKSHOP, 100), (EMPTY_SET_LIT, 16), (SHOOTOUT_SET_LIT, 2)
               , (AMBUSH_SET_DARK, 4), (BATTLE_SET_DARK, 4) ]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 2  -- projectiles cannot craft (otherwise durable tools
                  -- would be applicable without harmful side-effects)
  , tfeature = [ Walkable, NoItem, NoActor  -- not clear, due to overhang
               , Embed WORKSHOP_BENCH ]
  }

-- * Helper functions

makeDark :: TileKind -> TileKind
makeDark k = let darkenText :: GroupName TileKind -> GroupName TileKind
                 darkenText t = maybe t (GroupName . (<> "Dark"))
                                $ T.stripSuffix "Lit" $ fromGroupName t
                 darkenFreq :: (GroupName TileKind, Int)
                            -> [(GroupName TileKind, Int)]
                 darkenFreq (t, n) =
                   case T.stripSuffix "Lit" $ fromGroupName t of
                     Nothing -> [(t, n)]
                     Just stripped ->
                       let dark = GroupName $ stripped <> "Dark"
                       in -- lit plays the role of dark in a @Dark@ group
                          [(dark, n) | isNothing $ dark `lookup` darkFreq]
                 (darkFreq, notDarkFreq) =
                   partition (T.isSuffixOf "Dark" . fromGroupName . fst)
                             (tfreq k)
                 darkFrequency :: Freqs TileKind
                 darkFrequency = concatMap darkenFreq notDarkFreq
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkenText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkenText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkenText t
                 darkFeat (OpenWith proj grps t) =
                   Just $ OpenWith proj grps $ darkenText t
                 darkFeat (CloseWith proj grps t) =
                   Just $ CloseWith proj grps $ darkenText t
                 darkFeat (ChangeWith proj grps t) =
                   Just $ ChangeWith proj grps $ darkenText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkenText t
                 darkFeat (BuildAs t) = Just $ BuildAs $ darkenText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkenText t
                 darkFeat (ObscureAs t) = Just $ ObscureAs $ darkenText t
                 darkFeat VeryOftenItem = Just OftenItem
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

-- The yellow colour represents a dark tile lit by artificial light source,
-- or seen (felt, if very close) via noctovision. It is used to distinguish
-- ambiently lit tiles and dark tiles lit by dynamic light.
makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) { tcolor  = if tsymbol k == floorSymbol
                                              && tcolor k == BrWhite
                                           then BrYellow
                                           else tcolor k
                               , tcolor2 = BrBlack
                               }
