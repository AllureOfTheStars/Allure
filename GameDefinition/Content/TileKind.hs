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
import qualified Game.LambdaHack.Content.ItemKind as IK
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validateSingle = validateSingleTileKind
  , validateAll = validateAllTileKind
  , content = contentFromList $
      [unknown, wall, wallGlass, wallCache, hardRock, doorlessWall, oriel, pillar, pillarIce, lampPost, burningBush, bush, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsOutdoorUp, stairsLiftUp, stairsDown, stairsOutdoorDown, stairsLiftDown, escapeUp, escapeDown, escapeOutdoorDown, escapeSpaceshipDown, floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorActorLit, floorItemLit, floorActorItemLit, floorArenaShade, floorRedLit, floorBlueLit, floorGreenLit, floorFog, floorSmoke]
      ++ map makeDarkColor [floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorActorLit, floorItemLit, floorActorItemLit]
  }
unknown,        wall, wallGlass, wallCache, hardRock, doorlessWall, oriel, pillar, pillarIce, lampPost, burningBush, bush, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsOutdoorUp, stairsLiftUp, stairsDown, stairsOutdoorDown, stairsLiftDown, escapeUp, escapeDown, escapeOutdoorDown, escapeSpaceshipDown, floorCorridorLit, floorArenaLit, floorNoiseLit, floorDirtLit, floorActorLit, floorItemLit, floorActorItemLit, floorArenaShade, floorRedLit, floorBlueLit, floorGreenLit, floorFog, floorSmoke :: TileKind

unknown = TileKind  -- needs to have index 0 and alter 1
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , talter   = 1
  , tfeature = [Dark]
  }
oriel = TileKind
  { tsymbol  = '\''
  , tname    = "oriel"
  , tfreq    = [("oriels fence", 4)]
  , tcolor   = White
  , tcolor2  = Black
  , talter   = maxBound
  , tfeature = [Dark, Impenetrable]
  }
wall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [ ("fillerWall", 1), ("legendLit", 100), ("legendDark", 100)
               , ("cachable", 70)
               , ("staircase terminal", 100), ("staircase lift terminal", 100)
               , ("noiseSet", 95), ("battleSet", 250)
               , ("wallOrGlassOver_%_Lit", 90)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [HideAs "suspect wall", Indistinct]
  }
wallGlass = TileKind
  { tsymbol  = '#'
  , tname    = "polished crystal wall"
  , tfreq    = [("wallOrGlassOver_%_Lit", 10)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [Clear]
  }
hardRock = TileKind
  { tsymbol  = '#'
  , tname    = "outer hull"
  , tfreq    = [("basic outer fence", 100), ("oriels fence", 96)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , talter   = maxBound
  , tfeature = [Impenetrable]
  }
doorlessWall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [("doorlessWallOver_#", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 100
  , tfeature = [HideAs "fillerWall"]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [ ("legendLit", 100), ("legendDark", 100)
               , ("brawlSet", 50) ]
  , tcolor   = BrCyan  -- not BrWhite, to tell from heroes
  , tcolor2  = Cyan
  , talter   = 100
  , tfeature = []
  }
pillarIce = pillar
  { tname    = "ice"
  , tfreq    = [("brawlSet", 2)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , talter   = 10
  , tfeature = [Clear]
  }
wallCache = TileKind
  { tsymbol  = '&'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30), ("staircase lift terminal", 5)  -- only lifts
               , ("legendLit", 100), ("legendDark", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 5
  , tfeature = [ Cause $ IK.CreateItem CGround "useful" IK.TimerNone
               , ChangeTo "cachable" ]
  }
lampPost = TileKind
  { tsymbol  = 'O'
  , tname    = "lamp post"
  , tfreq    = [("lampPostOver_O", 90)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , talter   = 100
  , tfeature = []
  }
burningBush = TileKind
  { tsymbol  = 'O'
  , tname    = "burning bush"
  , tfreq    = [("lampPostOver_O", 10), ("ambushSet", 3)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , talter   = 10
  , tfeature = []
  }
bush = TileKind
  { tsymbol  = 'O'
  , tname    = "bush"
  , tfreq    = [("ambushSet", 100), ("battleSet", 30)]
  , tcolor   = Green
  , tcolor2  = BrBlack
  , talter   = 10
  , tfeature = [Dark]
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [("brawlSet", 140), ("treeShadeOver_O", 1)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , talter   = 50
  , tfeature = []
  }
wallSuspect = TileKind
  { tsymbol  = '#'
  , tname    = "moldy wall"
  , tfreq    = [("suspect wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = 2
  , tfeature = [Suspect, RevealAs "closed door", Indistinct]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 2
  , tfeature = [OpenTo "open door", HideAs "suspect wall"]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = 4
  , tfeature = [Walkable, Clear, NoItem, NoActor, CloseTo "closed door"]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("staircase up", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Cause $ IK.Ascend 1]
  }
stairsOutdoorUp = stairsUp
  { tname    = "signpost pointing backward"
  , tfreq    = [("staircase outdoor up", 1)]
  }
stairsLiftUp = stairsUp
  { tname    = "lift up"
  , tfreq    = [("staircase lift up", 1)]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("staircase down", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = talterForStairs
  , tfeature = [Cause $ IK.Ascend (-1)]
  }
stairsOutdoorDown = stairsDown
  { tname    = "signpost pointing forward"
  , tfreq    = [("staircase outdoor down", 1)]
  }
stairsLiftDown = stairsDown
  { tname    = "lift down"
  , tfreq    = [("staircase lift down", 1)]
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit hatch up"
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = talterForStairs
  , tfeature = [Cause $ IK.Escape 1]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendLit", 1), ("legendDark", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , talter   = talterForStairs
  , tfeature = [Cause $ IK.Escape (-1)]
  }
escapeOutdoorDown = escapeDown
  { tname    = "exit back to town"
  , tfreq    = [("escape outdoor down", 1)]
  }
escapeSpaceshipDown = escapeDown
  { tname    = "airlock to the shuttle"
  , tfreq    = [("escape spaceship down", 1)]
  }
floorCorridorLit = TileKind
  { tsymbol  = '.'
  , tname    = "floor"
  , tfreq    = [("floorCorridorLit", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , talter   = maxBound
  , tfeature = [Walkable, Clear, Indistinct]
  }
floorArenaLit = floorCorridorLit
  {  tfreq    = [("floorArenaLit", 1), ("arenaSet", 1), ("emptySet", 99)]
  }
floorNoiseLit = floorArenaLit
  { tname    = "oily stone floor"
  , tfreq    = [("noiseSet", 50)]
  }
floorDirtLit = floorArenaLit
  { tname    = "dirt"
  , tfreq    = [("battleSet", 1000), ("brawlSet", 1000), ("ambushSet", 1000)]
  }
floorActorLit = floorArenaLit
  { tfreq    = []
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
  , tfreq    = [("treeShadeOrFogOver_s", 95)]
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature floorActorLit  -- no OftenItem
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
  , tname    = "dense fog"
  , tfreq    = [("emptySet", 1), ("labTrail", 30), ("treeShadeOrFogOver_s", 5)]
  , tcolor   = BrCyan
  , tcolor2  = Cyan
  , talter   = maxBound
  , tfeature = [Walkable, Dark, NoItem]
  }
floorSmoke = floorFog
  { tname    = "billowing smoke"
  , tfreq    = [("battleSet", 5), ("labTrail", 70), ("staircase terminal", 5)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , talter   = maxBound
  , tfeature = [Walkable, NoItem]  -- not dark, embers
  }

makeDark :: TileKind -> TileKind
makeDark k = let darkText :: GroupName TileKind -> GroupName TileKind
                 darkText t = maybe t (toGroupName . (<> "Dark"))
                              $ T.stripSuffix "Lit" $ tshow t
                 darkFrequency = map (first darkText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkText t
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) { tcolor  = BrYellow
                               , tcolor2 = BrBlack
                               }
