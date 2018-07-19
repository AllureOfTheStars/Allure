-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Terrain tile definitions.
module Content.TileKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.TileKind

content :: [TileKind]
content =
  [unknown, unknownOuterFence, basicOuterFence, wall, wallSuspect, wallObscured, wallObscuredDefaced, wallObscuredFrescoed, pillar, pillarCache, lampPost, signboardUnread, signboardRead, tree, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, pillarIce, pulpit, bush, bushBurnt, bushBurning, floorFog, floorFogDark, floorSmoke, floorSmokeDark, doorOpen, floorCorridor, floorArena, floorNoise, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorRed, floorBlue, floorGreen, floorBrown, floorArenaShade ]
  ++ map makeDarkColor ldarkColorable
  -- Allure-specific
  ++ [oriel, outerHullWall, doorlessWall, machineWall, rubbleBurning, rubbleSpiceBurning, wallObscuredSafety, wallObscured3dBillboard, rock, pillarCache2, stairsLiftUp, stairsLiftTrappedUp, stairsLiftDown, stairsLiftTrappedDown, escapeSpaceshipDown, emptyAirlock, floorWindow]

unknown,    unknownOuterFence, basicOuterFence, wall, wallSuspect, wallObscured, wallObscuredDefaced, wallObscuredFrescoed, pillar, pillarCache, lampPost, signboardUnread, signboardRead, tree, treeBurnt, treeBurning, rubble, rubbleSpice, doorTrapped, doorClosed, stairsUp, stairsTrappedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTrappedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, wallGlass, wallGlassSpice, pillarIce, pulpit, bush, bushBurnt, bushBurning, floorFog, floorFogDark, floorSmoke, floorSmokeDark, doorOpen, floorCorridor, floorArena, floorNoise, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorRed, floorBlue, floorGreen, floorBrown, floorArenaShade :: TileKind
-- Allure-specific
oriel,       outerHullWall, doorlessWall, machineWall, rubbleBurning, rubbleSpiceBurning, wallObscuredSafety, wallObscured3dBillboard, rock, pillarCache2, stairsLiftUp, stairsLiftTrappedUp, stairsLiftDown, stairsLiftTrappedDown, escapeSpaceshipDown, emptyAirlock, floorWindow :: TileKind

ldarkColorable :: [TileKind]
ldarkColorable = [tree, bush, floorCorridor, floorArena, floorNoise, floorDirt, floorDirtSpice, floorActor, floorActorItem, floorWindow]

-- Symbols to be used:
--         LOS    noLOS
-- Walk    .'     :;
-- noWalk  %^     #O&<>+
--
-- can be opened ^&+
-- can be closed '
-- some noWalk can be changed without opening, regardless of symbol
-- not used yet:
-- ~ (water, acid, ect.)
-- : (curtain, etc., not flowing, but solid and static)
-- ` (not visible enough, would need font modification)

-- Note that for AI hints and UI comfort, most multiple-use @Embed@ tiles
-- should have a variant, which after first use transforms into a different
-- colour tile without @ChangeTo@ and similar (which then AI no longer touches).
-- If a tile is supposed to be repeatedly activated by AI (e.g., cache),
-- it should keep @ChangeTo@ for the whole time.

-- * Main tiles, modified for Allure; some removed

-- ** Not walkable

-- *** Not clear

unknown = TileKind  -- needs to have index 0 and alter 1
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark]
  }
unknownOuterFence = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown outer fence", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
basicOuterFence = TileKind
  { tsymbol  = '#'
  , tname    = "habitat containment wall"
  , tfreq    = [("habitat containment wall", 1)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , talter   = maxBound  -- impenetrable
  , tfeature = []
  }
wall = TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tfreq    = [ ("fillerWall", 1), ("legendLit", 100), ("legendDark", 100)
               , ("cachable deposit", 80), ("cachable jewelry", 80)
               , ("stair terminal", 30)
               , ("battleSet", 250), ("rectWindowsOver_%_Lit", 80) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs "suspect wall"]
  }
wallSuspect = TileKind  -- only on client
  { tsymbol  = '#'
  , tname    = "suspect wall"
  , tfreq    = [("suspect wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs "trapped door"
               , ObscureAs "obscured wall"
               ]
  }
wallObscured = TileKind
  { tsymbol  = '#'
  , tname    = "scratched wall"
  , tfreq    = [("obscured wall", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "scratch on wall"
               , HideAs "suspect wall"
               ]
  }
wallObscuredDefaced = TileKind
  { tsymbol  = '#'
  , tname    = "defaced wall"
  , tfreq    = [("obscured wall", 45), ("escapeSetDark", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "obscene pictogram"
               , HideAs "suspect wall"
               ]
  }
wallObscuredFrescoed = TileKind
  { tsymbol  = '#'
  , tname    = "subtle mural"
  , tfreq    = [("obscured wall", 5), ("brawlSetLit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "subtle fresco"
               , HideAs "suspect wall"
               ]  -- a bit beneficial, but AI would loop if allowed to trigger
                  -- so no @ConsideredByAI@
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "construction beam"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = []
  }
pillarCache = TileKind
  { tsymbol  = '#'
  , tname    = "rack of deposit boxes"
  , tfreq    = [ ("cachable deposit", 20), ("cache deposit", 1)
               , ("stair terminal", 1) ]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [ Embed "treasure cache", Embed "treasure cache trap"
               , ChangeTo "cachable deposit", ConsideredByAI ]
      -- Not explorable, but prominently placed, so hard to miss.
      -- Very beneficial, so AI eager to trigger, unless wary of traps.
  }
lampPost = TileKind
  { tsymbol  = 'O'
  , tname    = "lamp post"
  , tfreq    = [("lampPostOver_O", 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = []
  }
signboardUnread = TileKind  -- client only, indicates never used by this faction
  { tsymbol  = 'O'
  , tname    = "signboard"
  , tfreq    = [("signboard unread", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [ ConsideredByAI  -- changes after use, so safe for AI
               , RevealAs "signboard"  -- to display as hidden
               ]
  }
signboardRead = TileKind
  { tsymbol  = 'O'
  , tname    = "signboard"
  , tfreq    = [("signboard", 1), ("escapeSetDark", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [Embed "signboard", HideAs "signboard unread"]
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [ ("brawlSetLit", 140), ("shootoutSetLit", 10)
               , ("escapeSetLit", 35), ("treeShadeOver_O_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
treeBurnt = tree
  { tname    = "burnt tree"
  , tfreq    = [("zooSet", 7), ("tree with fire", 30)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurning = tree
  { tname    = "burning tree"
  , tfreq    = [("zooSet", 40), ("tree with fire", 70)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "big fire" : ChangeTo "tree with fire" : tfeature tree
      -- dousing off the tree will have more sense when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
rubble = TileKind
  { tsymbol  = '&'
  , tname    = "rubble pile"
  , tfreq    = [ ("rubble", 1), ("stair terminal", 30)
               , ("emptySet", 7), ("emptyExitSet", 7), ("noiseSet", 80)
               , ("zooSet", 100), ("ambushSet", 18) ]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 4  -- boss can dig through
  , tfeature = [OpenTo "floorArenaLit", Embed "rubble"]
      -- It's not explorable, due to not being walkable nor clear and due
      -- to being a door (@OpenTo@), which is kind of OK, because getting
      -- the item is risky and, e.g., AI doesn't attempt it.
      -- Also, AI doesn't go out of its way to clear the way for heroes.
  }
rubbleSpice = rubble
  { tfreq    = [("smokeClumpOver_f_Lit", 1)]
  , tfeature = Spice : tfeature rubble
  }
doorTrapped = TileKind
  { tsymbol  = '+'
  , tname    = "trapped door"
  , tfreq    = [("trapped door", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 2
  , tfeature = [ Embed "doorway trap"
               , OpenTo "open door"
               , HideAs "suspect wall"
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo "open door"]  -- never hidden
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("staircase up", 9), ("ordinary staircase up", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0  -- very easy stairs, unlike all others
  , tfeature = [Embed "staircase up", ConsideredByAI]
  }
stairsTrappedUp = TileKind
  { tsymbol  = '<'
  , tname    = "windy staircase up"
  , tfreq    = [("staircase up", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed "staircase up", Embed "staircase trap up"
               , ConsideredByAI, ChangeTo "ordinary staircase up" ]
                 -- AI uses despite the trap; exploration more important
  }
stairsOutdoorUp = stairsUp
  { tname    = "signpost pointing backward"
  , tfreq    = [("staircase outdoor up", 1)]
  , talter   = talterForStairs
  }
stairsGatedUp = stairsUp
  { tname    = "gated staircase up"
  , tfreq    = [("gated staircase up", 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("staircase down", 9), ("ordinary staircase down", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0  -- very easy stairs, unlike all others
  , tfeature = [Embed "staircase down", ConsideredByAI]
  }
stairsTrappedDown = TileKind
  { tsymbol  = '>'
  , tname    = "crooked staircase down"
  , tfreq    = [("staircase down", 1)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = talterForStairs
  , tfeature = [ Embed "staircase down", Embed "staircase trap down"
               , ConsideredByAI, ChangeTo "ordinary staircase down" ]
  }
stairsOutdoorDown = stairsDown
  { tname    = "signpost pointing forward"
  , tfreq    = [("staircase outdoor down", 1)]
  , talter   = talterForStairs
  }
stairsGatedDown = stairsDown
  { tname    = "gated staircase down"
  , tfreq    = [("gated staircase down", 1)]
  , talter   = talterForStairs + 2  -- animals and bosses can't use
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit hatch up"
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed "escape", ConsideredByAI]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = 0  -- anybody can escape (or guard escape)
  , tfeature = [Embed "escape", ConsideredByAI]
  }
escapeOutdoorDown = escapeDown
  { tname    = "exit back to town"
  , tfreq    = [("escape outdoor down", 1)]
  }

-- *** Clear

wallGlass = TileKind
  { tsymbol  = '%'
  , tname    = "transparent polymer wall"
  , tfreq    = [("glasshouseOver_%_Lit", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs "suspect wall", Clear]
  }
wallGlassSpice = wallGlass
  { tfreq    = [("rectWindowsOver_%_Lit", 20)]
  , tfeature = Spice : tfeature wallGlass
  }
pillarIce = TileKind
  { tsymbol  = '^'
  , tname    = "ice"
  , tfreq    = [("brawlSetLit", 20)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 4  -- boss can dig through
  , tfeature = [Clear, Embed "frost", OpenTo "damp stone floor"]
      -- Is door, due to @OpenTo@, so is not explorable, but it's OK, because
      -- it doesn't generate items nor clues. This saves on the need to
      -- get each ice pillar into sight range when exploring level.
  }
pulpit = TileKind
  { tsymbol  = '%'
  , tname    = "VR booth"
  , tfreq    = [("pulpit", 1)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 5
  , tfeature = [Clear, Embed "pulpit"]
                 -- mixed blessing, so AI ignores, saved for player fun
  }
bush = TileKind
  { tsymbol  = '%'
  , tname    = "bush"
  , tfreq    = [ ("bush Lit", 1), ("shootoutSetLit", 30), ("escapeSetLit", 40)
               , ("arenaSetLit", 3), ("bushClumpOver_f_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 10
  , tfeature = [Clear]
  }
bushBurnt = bush
  { tname    = "burnt bush"
  , tfreq    = [ ("battleSet", 30), ("ambushSet", 4), ("zooSet", 30)
               , ("bush with fire", 70) ]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurning = bush
  { tname    = "burning bush"
  , tfreq    = [("ambushSet", 40), ("zooSet", 300), ("bush with fire", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "small fire" : ChangeTo "bush with fire" : tfeature bush
  }

-- ** Walkable

-- *** Not clear

floorFog = TileKind
  { tsymbol  = ';'
  , tname    = "faint fog"
  , tfreq    = [ ("lit fog", 1), ("emptySet", 50), ("shootoutSetLit", 20)
               , ("emptyExitSet", 20)
               , ("noiseSet", 100), ("fogClumpOver_f_Lit", 60) ]
      -- lit fog is OK for shootout, because LOS is mutual, as opposed
      -- to dark fog, and so camper has little advantage, especially
      -- on big maps, where he doesn't know on which side of fog patch to hide
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = [Walkable, NoItem, OftenActor]
  }
floorFogDark = floorFog
  { tname    = "thick fog"
  , tfreq    = [("noiseSet", 100), ("escapeSetDark", 50)]
  , tfeature = Dark : tfeature floorFog
  }
floorSmoke = TileKind
  { tsymbol  = ';'
  , tname    = "billowing smoke"
  , tfreq    = [ ("lit smoke", 1), ("labTrailLit", 1), ("stair terminal", 2)
               , ("smokeClumpOver_f_Lit", 1), ("emptyExitSet", 20) ]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 0
  , tfeature = [Walkable, NoItem]  -- not dark, embers
  }
floorSmokeDark = floorSmoke
  { tname    = "lingering smoke"
  , tfreq    = [("ambushSet", 60), ("zooSet", 20), ("battleSet", 5)]
  , tfeature = Dark : tfeature floorSmoke
  }

-- *** Clear

doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , CloseTo "closed door"
               ]
  }
floorCorridor = TileKind
  { tsymbol  = floorSymbol
  , tname    = "floor"
  , tfreq    = [("floorCorridorLit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0
  , tfeature = [Walkable, Clear]
  }
floorArena = floorCorridor
  { tfreq    = [ ("floorArenaLit", 1), ("arenaSetLit", 96), ("emptySet", 900)
               , ("zooSet", 600) ]
  }
floorNoise = floorArena
  { tname    = "oily floor"
  , tfreq    = [ ("noiseSet", 600), ("emptyExitSet", 880)
               , ("damp stone floor", 1) ]
  }
floorDirt = floorArena
  { tname    = "dirt"
  , tfreq    = [ ("battleSet", 1000), ("brawlSetLit", 1000)
               , ("shootoutSetLit", 1000), ("escapeSetLit", 1000)
               , ("ambushSet", 1000) ]
  }
floorDirtSpice = floorDirt
  { tfreq    = [ ("treeShadeOver_s_Lit", 1), ("fogClumpOver_f_Lit", 40)
               , ("smokeClumpOver_f_Lit", 1), ("bushClumpOver_f_Lit", 1) ]
  , tfeature = Spice : tfeature floorDirt
  }
floorActor = floorArena
  { tfreq    = [("floorActorLit", 1)]  -- lit even in dark cave, so no items
  , tfeature = OftenActor : tfeature floorArena
  }
floorActorItem = floorActor
  { tfreq    = [("legendLit", 100)]
  , tfeature = VeryOftenItem : tfeature floorActor
  }
floorRed = floorCorridor
  { tname    = "emergency walkway"
  , tfreq    = [ ("emergency walkway", 1)
               , ("trailLit", 20), ("alarmingTrailLit", 70) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = [Trail, Walkable, Clear]
  }
floorBlue = floorRed
  { tname    = "transport route"
  , tfreq    = [("transport route", 1), ("trailLit", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreen = floorRed
  { tname    = "greenery trail"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "overgrown path"
  , tfreq    = [("alarmingTrailLit", 30)]
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
floorArenaShade = floorActor
  { tname    = "shaded ground"
  , tfreq    = [("shaded ground", 1), ("treeShadeOver_s_Lit", 2)]
  , tcolor2  = BrBlack
  , tfeature = Dark : NoItem : tfeature floorActor
  }

-- * Allure-specific

-- ** Not walkable

-- *** Not clear

oriel = TileKind
  { tsymbol  = '%'  -- story-wise it's transparent, hence the symbol
  , tname    = "oriel"
  , tfreq    = [ ("oriels fence", 5)
               , ("airlock fence", 5), ("empty airlock fence", 5) ]
  , tcolor   = White
  , tcolor2  = Black
  , talter   = 5
  , tfeature = [Embed "black starry sky", Dark]
  }
outerHullWall = basicOuterFence
  { tname    = "outer hull wall"
  , tfreq    = [ ("basic outer fence", 1), ("oriels fence", 95)
               , ("airlock fence", 40), ("empty airlock fence", 40) ]
  }
doorlessWall = TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tfreq    = [("noiseSet", 600), ("doorlessWallOver_#", 20)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [HideAs "fillerWall"]
  }
rubbleBurning = TileKind
  { tsymbol  = '&'
  , tname    = "burning installation"
  , tfreq    = [ ("emptySet", 4), ("emptyExitSet", 6), ("noiseSet", 7)
               , ("ambushSet", 2), ("zooSet", 40), ("stair terminal", 40) ]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 4  -- boss can dig through
  , tfeature = [ChangeTo "rubble", Embed "big fire"]
  }
rubbleSpiceBurning = rubbleBurning
  { tfreq    = [("smokeClumpOver_f_Lit", 1)]
  , tfeature = Spice : tfeature rubbleBurning
  }
wallObscuredSafety = TileKind
  { tsymbol  = '#'
  , tname    = "safety procedures wall"
  , tfreq    = [("obscured wall", 5)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "ruined first aid kit"
               , HideAs "suspect wall"
               ]
  }
wallObscured3dBillboard = TileKind
  { tsymbol  = '#'
  , tname    = "3D billboard"
  , tfreq    = [("obscured wall", 45)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "3D display"
               , HideAs "suspect wall"
               ]
  }
rock = pillar
  { tname    = "rock"
  , tfreq    = [("brawlSetLit", 30), ("arenaSetLit", 1), ("arenaSetDark", 1)]
  }
pillarCache2 = pillarCache
  { tname    = "jewelry display"
  , tfreq    = [ ("cachable jewelry", 20), ("cache jewelry", 1)
               , ("escapeSetDark", 1) ]
  , tfeature = [ Embed "jewelry case", Embed "treasure cache trap"
               , ChangeTo "cachable jewelry", ConsideredByAI ]
  }
stairsLiftUp = stairsUp
  { tname    = "lift up"
  , tfreq    = [("staircase lift up", 9), ("ordinary lift up", 1)]
  , talter   = talterForStairs
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , tfeature = [Embed "lift up", ConsideredByAI]
  }
stairsLiftTrappedUp = stairsTrappedUp
  { tname    = "corroded lift up"
  , tfreq    = [("staircase lift up", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = [ Embed "lift up", Embed "lift trap"
               , ConsideredByAI, ChangeTo "ordinary lift up" ]
                 -- AI uses despite the trap; exploration more important
  }
stairsLiftDown = stairsDown
  { tname    = "lift down"
  , tfreq    = [("staircase lift down", 9), ("ordinary lift down", 1)]
  , talter   = talterForStairs
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , tfeature = [Embed "lift down", ConsideredByAI]
  }
stairsLiftTrappedDown = stairsTrappedDown
  { tname    = "corroded lift down"
  , tfreq    = [("staircase lift down", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfeature = [ Embed "lift down", Embed "lift trap"
               , ConsideredByAI, ChangeTo "ordinary lift down" ]
  }
escapeSpaceshipDown = escapeDown
  { tname    = "airlock to a shuttle"
  , tfreq    = [("escape spaceship down", 1), ("airlock fence", 3)]
  }
emptyAirlock = escapeUp
  { tname    = "disengaged airlock"
  , tfreq    = [ ("airlock fence", 2), ("empty airlock fence", 7)
               , ("emptySet", 2) ]  -- not in emptyExitSet; space can't be seen
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , talter   = maxBound  -- suicide forbidden
  , tfeature = [Embed "black starry sky"]  -- but can look at the airlock
  }
floorWindow = floorArena
  { tsymbol  = ' '  -- story-wise it's transparent, hence the symbol
  , tname    = "floor window"
  , tfreq    = [("emptySet", 30)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , tfeature = Embed "black starry sky" : tfeature floorCorridor
  }

-- *** Clear

machineWall = TileKind
  { tsymbol  = '%'
  , tname    = "hardware rack"
  , tfreq    = [ ("noiseSet", 350), ("emptyExitSet", 60)
               , ("doorlessWallOver_#", 80) ]
  , tcolor   = White
  , tcolor2  = BrBlack
  , talter   = 100
  , tfeature = [Spice, Clear]
  }

-- ** Walkable

-- none

-- * Helper functions

makeDark :: TileKind -> TileKind
makeDark k = let darkText :: GroupName TileKind -> GroupName TileKind
                 darkText t = maybe t (toGroupName . (<> "Dark"))
                              $ T.stripSuffix "Lit" $ tshow t
                 darkFrequency = map (first darkText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkText t
                 darkFeat (BuildAs t) = Just $ BuildAs $ darkText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkText t
                 darkFeat (ObscureAs t) = Just $ ObscureAs $ darkText t
                 darkFeat VeryOftenItem = Just OftenItem
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) { tcolor  = if tsymbol k == floorSymbol
                                           then BrYellow
                                           else tcolor k
                               , tcolor2 = BrBlack
                               }
