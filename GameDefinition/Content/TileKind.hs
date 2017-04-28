-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Terrain tile definitions.
module Content.TileKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validateSingle = validateSingleTileKind
  , validateAll = validateAllTileKind
  , content = contentFromList $
      [unknown, hardRock, pillar, pillarIce, pulpit, pillarCache, lampPost, signboardUnread, signboardRead, bush, bushDark, bushBurnt, bushBurning, tree, treeDark, treeBurnt, treeBurning, wall, wallGlass, wallGlassSpice, wallSuspect, wallObscured, doorTrapped, doorClosed, doorOpen, stairsUp, stairsTaintedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTaintedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, rubble, rubblePlace, floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorDirtSpiceLit, floorArenaShade, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit, floorFog, floorFogDark, floorSmoke, floorSmokeDark]
      ++ map makeDarkColor ldarkColorable
      ++ [oriel, rock, doorlessWall, wallObscuredDefaced, wallObscuredFrescoed, stairsLiftUp, stairsLiftDown, escapeSpaceshipDown]
  }
unknown,        hardRock, pillar, pillarIce, pulpit, pillarCache, lampPost, signboardUnread, signboardRead, bush, bushDark, bushBurnt, bushBurning, tree, treeDark, treeBurnt, treeBurning, wall, wallGlass, wallGlassSpice, wallSuspect, wallObscured, doorTrapped, doorClosed, doorOpen, stairsUp, stairsTaintedUp, stairsOutdoorUp, stairsGatedUp, stairsDown, stairsTaintedDown, stairsOutdoorDown, stairsGatedDown, escapeUp, escapeDown, escapeOutdoorDown, rubble, rubblePlace, floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorDirtSpiceLit, floorArenaShade, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit, floorFog, floorFogDark, floorSmoke, floorSmokeDark :: TileKind
oriel, rock, doorlessWall, wallObscuredDefaced, wallObscuredFrescoed, stairsLiftUp, stairsLiftDown, escapeSpaceshipDown :: TileKind

ldarkColorable :: [TileKind]
ldarkColorable = [floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorActorLit, floorItemLit, floorActorItemLit]

-- Symbols to be used:
--         LOS    noLOS
-- Walk    .'     :;
-- noWalk  &^     #O%<>+
--
-- can be opened ^%+
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

unknown = TileKind  -- needs to have index 0 and alter 1
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark]
  }
hardRock = TileKind
  { tsymbol  = '#'
  , tname    = "outer hull"
  , tfreq    = [ ("basic outer fence", 1), ("noise fence", 1)
               , ("oriels fence", 96)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , talter   = maxBound  -- impenetrable
  , tfeature = []
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = [Indistinct]
  }
pillarIce = TileKind
  { tsymbol  = '^'
  , tname    = "ice"
  , tfreq    = [("brawlSet", 2), ("ice", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [Clear, Embed "frost", OpenTo "damp stone floor"]
      -- Is door, due to @OpenTo@, so is not explorable, but it's OK, because
      -- it doesn't generate items nor clues. This saves on the need to
      -- get each ice pillar into sight range when exploring level.
  }
pulpit = TileKind
  { tsymbol  = 'O'
  , tname    = "VR harness"
  , tfreq    = [("pulpit", 1), ("zooSet", 5)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [Clear, Embed "pulpit", Indistinct]
  }
pillarCache = TileKind
  { tsymbol  = 'O'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30), ("stair terminal", 1)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 5
  , tfeature = [ Embed "terrain cache", Embed "terrain cache trap"
               , ChangeTo "cachable", ConsideredByAI, Indistinct ]
      -- Not explorable, but prominently placed, so hard to miss.
      -- Very beneficial, so AI eager to trigger.
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
  , tfeature = [ Embed "signboard", Indistinct
               , ConsideredByAI  -- changes after use, so safe for AI
               , RevealAs "signboard" ]  -- to display as hidden
  }
signboardRead = TileKind  -- after first use revealed to be this one
  { tsymbol  = 'O'
  , tname    = "signboard"
  , tfreq    = [("signboard", 1)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 5
  , tfeature = [Embed "signboard", HideAs "signboard unread", Indistinct]
  }
bush = TileKind
  { tsymbol  = '&'
  , tname    = "bush"
  , tfreq    = [ ("lit bush", 1), ("shootoutSet", 30)
               , ("bushClumpOver_f_Lit", 1) ]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 10
  , tfeature = [Clear]
  }
bushDark = bush
  { tfreq    = [("escapeSet", 30)]
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurnt = bush
  { tname    = "burnt bush"
  , tfreq    = [("battleSet", 30), ("bush with fire", 70)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature bush
  }
bushBurning = bush
  { tname    = "burning bush"
  , tfreq    = [("ambushSet", 30), ("zooSet", 300), ("bush with fire", 30)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "small fire" : ChangeTo "bush with fire" : tfeature bush
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [("brawlSet", 140), ("treeShadeOver_O_Lit", 1)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
treeDark = tree
  { tfreq    = [("escapeSet", 30)]
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurnt = tree
  { tname    = "burnt tree"
  , tfreq    = [("tree with fire", 30)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature tree
  }
treeBurning = tree
  { tname    = "burning tree"
  , tfreq    = [("ambushSet", 30), ("zooSet", 30), ("tree with fire", 70)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 5
  , tfeature = Embed "big fire" : ChangeTo "tree with fire" : tfeature tree
      -- dousing off the tree will have more sense when it periodically
      -- explodes, hitting and lighting up the team and so betraying it
  }
wall = TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tfreq    = [ ("fillerWall", 1), ("legendLit", 100), ("legendDark", 100)
               , ("cachable", 70), ("stair terminal", 100)
               , ("noiseSet", 95), ("shootoutSet", 10)
               , ("zooSet", 30), ("battleSet", 250)
               , ("rectWindowsOver_%_Lit", 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [BuildAs "suspect wall", Indistinct]
  }
wallGlass = TileKind
  { tsymbol  = '#'
  , tname    = "transparent polymer wall"
  , tfreq    = [("wallGlass", 1), ("rectWindowsOver_%_Lit", 10)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [BuildAs "suspect wall", Clear]
  }
wallGlassSpice = wallGlass
  { tfreq    = [("rectWindowsOver_%_Lit", 10)]
  , tfeature = Spice : tfeature wallGlass
  }
wallSuspect = TileKind  -- only on client
  { tsymbol  = '#'
  , tname    = "suspect uneven wall"
  , tfreq    = [("suspect wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [ RevealAs "trapped door"
               , ObscureAs "obscured wall"
               , Indistinct ]
  }
wallObscured = TileKind
  { tsymbol  = '#'
  , tname    = "scratched wall"
  , tfreq    = [("obscured wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "scratch on wall"
               , HideAs "suspect wall"
               , Indistinct
               ]
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
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("staircase up", 9), ("ordinary staircase up", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed "staircase up", ConsideredByAI]
  }
stairsTaintedUp = TileKind
  { tsymbol  = '<'
  , tname    = "tainted staircase up"
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
  }
stairsGatedUp = stairsUp
  { tname    = "gated staircase up"
  , tfreq    = [("gated staircase up", 1)]
  , talter   = talterForStairs + 1  -- animals and bosses can't use
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("staircase down", 9), ("ordinary staircase down", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Embed "staircase down", ConsideredByAI]
  }
stairsTaintedDown = TileKind
  { tsymbol  = '>'
  , tname    = "tainted staircase down"
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
  }
stairsGatedDown = stairsDown
  { tname    = "gated staircase down"
  , tfreq    = [("gated staircase down", 1)]
  , talter   = talterForStairs + 1  -- animals and bosses can't use
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
rubble = TileKind
  { tsymbol  = '%'
  , tname    = "rubble"
  , tfreq    = []  -- [("floorCorridorLit", 1)]
                   -- disabled while it's all or nothing per cave and per room;
                   -- we need a new mechanism, Spice is not enough, because
                   -- we don't want multicolor trailLit corridors
      -- ("rubbleOrNot", 70)
      -- until we can sync change of tile and activation, it always takes 1 turn
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [OpenTo "rubbleOrNot", Embed "rubble", Indistinct]
  }
rubblePlace = TileKind
  { tsymbol  = '%'
  , tname    = "rubble"
  , tfreq    = [("smokeClumpOver_f_Lit", 1), ("noiseSet", 5), ("zooSet", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [Spice, OpenTo "rubblePlaceOrNot", Embed "rubble", Indistinct]
      -- It's not explorable, due to not being walkable nor clear and due
      -- to being a door (@OpenTo@), which is kind of OK, because getting
      -- the item is risky and, e.g., AI doesn't attempt it.
      -- Also, AI doesn't go out of its way to clear the way for heroes.
  }
floorCorridorLit = TileKind
  { tsymbol  = floorSymbol
  , tname    = "floor"
  , tfreq    = [("floorCorridorLit", 99), ("rubbleOrNot", 30)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 0
  , tfeature = [Walkable, Clear, Indistinct]
  }
floorArenaLit = floorCorridorLit
  { tfreq    = [ ("floorArenaLit", 1), ("rubblePlaceOrNot", 30)
               , ("arenaSet", 1), ("emptySet", 97), ("zooSet", 1000) ]
  }
floorNoiseLit = floorArenaLit
  { tname    = "oily floor"
  , tfreq    = [("noiseSet", 60), ("damp stone floor", 1)]
  }
floorDirtLit = floorArenaLit
  { tname    = "dirt"
  , tfreq    = [ ("battleSet", 1000), ("brawlSet", 1000), ("shootoutSet", 1000)
               , ("ambushSet", 1000), ("escapeSet", 1000) ]
  }
floorDirtSpiceLit = floorDirtLit
  { tfreq    = [ ("treeShadeOver_s_Lit", 1), ("fogClumpOver_f_Lit", 1)
               , ("smokeClumpOver_f_Lit", 1), ("bushClumpOver_f_Lit", 1) ]
  , tfeature = Spice : tfeature floorDirtLit
  }
floorActorLit = floorArenaLit
  { tfreq    = [("floorActorLit", 1)]
  , tfeature = OftenActor : tfeature floorArenaLit
  }
floorItemLit = floorArenaLit
  { tfreq    = []
  , tfeature = OftenItem : tfeature floorArenaLit
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("legendLit", 100)]  -- no OftenItem in legendDark
  , tfeature = OftenActor : tfeature floorItemLit
  }
floorArenaShade = floorActorLit
  { tname    = "shaded ground"
  , tfreq    = [("shaded ground", 1), ("treeShadeOver_s_Lit", 2)]
  , tcolor2  = BrBlack
  , tfeature = Dark : NoItem : tfeature floorActorLit
  }
floorRedLit = floorCorridorLit
  { tname    = "emergency walkway"
  , tfreq    = [("emergency walkway", 1), ("trailLit", 20)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Trail : tfeature floorCorridorLit  -- no Indistinct
  }
floorBlueLit = floorRedLit
  { tname    = "transport route"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreenLit = floorRedLit
  { tname    = "greenery trail"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorFog = TileKind
  { tsymbol  = ';'
  , tname    = "faint fog"
  , tfreq    = [ ("lit fog", 1), ("emptySet", 3), ("shootoutSet", 20)
               , ("fogClumpOver_f_Lit", 2) ]
      -- lit fog is OK for shootout, because LOS is mutual, as opposed
      -- to dark fog, and so camper has little advantage, especially
      -- on big maps, where he doesn't know on which side of fog patch to hide
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = 0
  , tfeature = [Walkable, NoItem, Indistinct]
  }
floorFogDark = floorFog
  { tname    = "thick fog"
  , tfreq    = [("noiseSet", 10), ("escapeSet", 60)]
  , tfeature = Dark : tfeature floorFog
  }
floorSmoke = TileKind
  { tsymbol  = ';'
  , tname    = "billowing smoke"
  , tfreq    = [ ("lit smoke", 1)
               , ("ambushSet", 30), ("zooSet", 15), ("battleSet", 5)
               , ("labTrailLit", 1), ("stair terminal", 2)
               , ("smokeClumpOver_f_Lit", 1) ]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 0
  , tfeature = [Walkable, NoItem, Indistinct]  -- not dark, embers
  }
floorSmokeDark = floorSmoke
  { tname    = "lingering smoke"
  , tfreq    = [("ambushSet", 30)]
  , tfeature = Dark : tfeature floorSmoke
  }

-- * Allure-specific

oriel = TileKind
  { tsymbol  = '&'  -- story-wise it's transparent, hence the symbol
  , tname    = "oriel"
  , tfreq    = [("oriels fence", 4)]
  , tcolor   = White
  , tcolor2  = Black
  , talter   = maxBound  -- impenetrable
  , tfeature = [Dark]
  }
rock = pillar
  { tname    = "rock"
  , tfreq    = [("brawlSet", 50)]
  }
doorlessWall = TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tfreq    = [("doorlessWallOver_#", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [HideAs "fillerWall", Indistinct]
  }
wallObscuredDefaced = TileKind
  { tsymbol  = '#'
  , tname    = "defaced wall"
  , tfreq    = [("obscured wall", 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "obscene pictograms"
               , HideAs "suspect wall"
               , Indistinct
               ]
  }
wallObscuredFrescoed = TileKind
  { tsymbol  = '#'
  , tname    = "subtle mural"
  , tfreq    = [("obscured wall", 10)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Embed "subtle fresco"
               , HideAs "suspect wall"
               , Indistinct
               ]
  }
stairsLiftUp = stairsUp
  { tname    = "lift up"
  , tfreq    = [("staircase lift up", 1)]
  }
stairsLiftDown = stairsDown
  { tname    = "lift down"
  , tfreq    = [("staircase lift down", 1)]
  }
escapeSpaceshipDown = escapeDown
  { tname    = "airlock to the shuttle"
  , tfreq    = [("escape spaceship down", 1)]
  }

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
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) { tcolor  = BrYellow
                               , tcolor2 = BrBlack
                               }
