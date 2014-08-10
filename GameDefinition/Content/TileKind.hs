-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Terrain tile definitions.
module Content.TileKind ( cdefs ) where

import Control.Arrow (first)
import Data.Maybe
import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Feature
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validateSingle = validateSingleTileKind
  , validateAll = validateAllTileKind
  , content =
      [wall, wallCache, hardRock, doorlessWall, oriel, pillar, lampPost, burningBush, bush, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorActorLit, floorItemLit, floorActorItemLit, floorArenaShade, floorRedLit, floorBlueLit, floorGreenLit]
      ++ map makeDarkColor [floorCorridorLit, floorActorLit, floorItemLit, floorActorItemLit]
  }
wall,        wallCache, hardRock, doorlessWall, oriel, pillar, lampPost, burningBush, bush, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorActorLit, floorItemLit, floorActorItemLit, floorArenaShade, floorRedLit, floorBlueLit, floorGreenLit :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [ ("fillerWall", 1), ("cachable", 70)
               , ("legendLit", 100), ("legendDark", 100)
               , ("noiseSet", 100), ("battleSet", 250) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [HideAs "suspect wall"]
  }
wallCache = TileKind
  { tsymbol  = '&'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30)
               , ("legendLit", 100), ("legendDark", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Cause $ Effect.CreateItem 1, ChangeTo "cachable"]
  }
hardRock = TileKind
  { tsymbol  = '#'
  , tname    = "outer hull"
  , tfreq    = [("basic outer fence", 100), ("oriels fence", 98)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = [Impenetrable]
  }
doorlessWall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [("doorlessWallOver_#", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
oriel = TileKind
  { tsymbol  = '\''
  , tname    = "oriel"
  , tfreq    = [("oriels fence", 2)]
  , tcolor   = White
  , tcolor2  = Black
  , tfeature = [Dark, Impenetrable]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [ ("legendLit", 100), ("legendDark", 100)
               , ("skirmishSet", 5) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
lampPost = TileKind
  { tsymbol  = 'O'
  , tname    = "lamp post"
  , tfreq    = [("lampPostOver_O", 90)]
  , tcolor   = BrYellow
  , tcolor2  = Brown
  , tfeature = []
  }
burningBush = TileKind
  { tsymbol  = 'O'
  , tname    = "burning bush"
  , tfreq    = [("lampPostOver_O", 10), ("ambushSet", 3), ("battleSet", 2)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = []
  }
bush = TileKind
  { tsymbol  = 'O'
  , tname    = "bush"
  , tfreq    = [("ambushSet", 100) ]
  , tcolor   = Green
  , tcolor2  = BrBlack
  , tfeature = [Dark]
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [("skirmishSet", 14), ("battleSet", 20), ("treeShadeOver_O", 1)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , tfeature = []
  }
wallSuspect = TileKind
  { tsymbol  = '#'
  , tname    = "moldy wall"
  , tfreq    = [("suspect wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Suspect, RevealAs "closed door"]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [OpenTo "open door", HideAs "suspect wall"]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, NoItem, NoActor, CloseTo "closed door"]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = []  -- TODO: [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Ascend 1]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = []  -- TODO: [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Ascend (-1)]
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "airlock to the shuttle"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Escape 1]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "airlock to the shuttle"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Escape (-1)]
  }
liftUp = TileKind
  { tsymbol  = '<'
  , tname    = "lift up"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrCyan
  , tcolor2  = BrCyan
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Ascend 1]
  }
lift = TileKind
  { tsymbol  = '<'
  , tname    = "lift"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrBlue
  , tcolor2  = BrBlue
  , tfeature = [ Walkable, Clear, NoItem, NoActor
               , Cause $ Effect.Ascend 1
               , Cause $ Effect.Ascend (-1) ]
  }
liftDown = TileKind
  { tsymbol  = '>'
  , tname    = "lift down"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrCyan
  , tcolor2  = BrCyan
  , tfeature = [Walkable, Clear, NoItem, NoActor, Cause $ Effect.Ascend (-1)]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , tfeature = [Dark]
  }
floorCorridorLit = TileKind
  { tsymbol  = '.'
  , tname    = "floor"
  , tfreq    = [ ("floorCorridorLit", 1), ("floorArenaLit", 1)
               , ("arenaSet", 1), ("emptySet", 1), ("noiseSet", 50)
               , ("battleSet", 1000), ("skirmishSet", 100)
               , ("ambushSet", 1000) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear]
  }
floorActorLit = floorCorridorLit
  { tfreq    = [("floorActorLit", 1)]
  , tfeature = OftenActor : tfeature floorCorridorLit
  }
floorItemLit = floorCorridorLit
  { tfreq    = []
  , tfeature = OftenItem : tfeature floorCorridorLit
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("legendLit", 100)]
  , tfeature = OftenActor : tfeature floorItemLit
  }
floorArenaShade = floorActorLit
  { tname    = "floor"  -- TODO: "shaded ground"
  , tfreq    = [("treeShadeOver_s", 1)]
  , tcolor2  = BrBlack
  , tfeature = Dark : tfeature floorActorLit  -- no OftenItem
  }
floorRedLit = floorCorridorLit
  { tname    = "emergency walkway"
  , tfreq    = [("trailLit", 20)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Trail : tfeature floorCorridorLit
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


makeDark :: TileKind -> TileKind
makeDark k = let darkText :: GroupName -> GroupName
                 darkText t = maybe t (toGroupName . (<> "Dark"))
                              $ T.stripSuffix "Lit" $ tshow t
                 darkFrequency = map (first darkText) $ tfreq k
                 darkFeat (OpenTo t) = Just $ OpenTo $ darkText t
                 darkFeat (CloseTo t) = Just $ CloseTo $ darkText t
                 darkFeat (ChangeTo t) = Just $ ChangeTo $ darkText t
                 darkFeat (HideAs t) = Just $ HideAs $ darkText t
                 darkFeat (RevealAs t) = Just $ RevealAs $ darkText t
                 darkFeat OftenItem = Nothing  -- items not common in the dark
                 darkFeat feat = Just $ feat
             in k { tfreq    = darkFrequency
                  , tfeature = Dark : mapMaybe darkFeat (tfeature k)
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) { tcolor  = BrYellow
                               , tcolor2 = BrBlack
                               }
