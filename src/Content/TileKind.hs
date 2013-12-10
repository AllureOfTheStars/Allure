-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Terrain tiles for Allure of the Stars.
module Content.TileKind ( cdefs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Feature
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = tvalidate
  , content =
      [wall, wallCache, hardRock, pillar, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorCorridorDark, floorItemLit, floorItemDark, floorActorItemLit, floorActorItemDark, floorRedDark, floorBlueDark, floorGreenDark, floorRedLit, floorBlueLit, floorGreenLit]
  }
wall,        wallCache, hardRock, pillar, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorCorridorDark, floorItemLit, floorItemDark, floorActorItemLit, floorActorItemDark, floorRedDark, floorBlueDark, floorGreenDark, floorRedLit, floorBlueLit, floorGreenLit :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [ ("fillerWall", 1), ("cachable", 70)
               , ("litLegend", 100), ("darkLegend", 100)
               , ("noiseSet", 55) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [HideAs "suspect wall"]
  }
wallCache = TileKind
  { tsymbol  = '&'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30)
               , ("litLegend", 100), ("darkLegend", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Cause $ Effect.CreateItem 1, ChangeTo "cachable"]
  }
hardRock = TileKind
  { tsymbol  = '#'
  , tname    = "outer hull"
  , tfreq    = [("outer fence", 1)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = [Impenetrable]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100), ("combatSet", 3)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [("combatSet", 8)]
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
  , tfeature = [ Suspect
               , RevealAs "closed door"
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100), ("closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [Exit, OpenTo "open door", HideAs "suspect wall"]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100), ("open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, CloseTo "closed door"]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = []  -- TODO: [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit, Exit, Cause $ Effect.Ascend 1]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = []  -- TODO: [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit, Exit, Cause $ Effect.Ascend (-1)]
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit airlock up"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Lit, Exit, Cause Effect.Escape]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Lit, Exit, Cause Effect.Escape]
  }
liftUp = TileKind
  { tsymbol  = '<'
  , tname    = "lift up"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrCyan
  , tcolor2  = BrCyan
  , tfeature = [Walkable, Clear, Lit, Exit, Cause $ Effect.Ascend 1]
  }
lift = TileKind
  { tsymbol  = '<'
  , tname    = "lift"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrBlue
  , tcolor2  = BrBlue
  , tfeature = [ Walkable, Clear, Lit, Exit
               , Cause $ Effect.Ascend 1
               , Cause $ Effect.Ascend (-1) ]
  }
liftDown = TileKind
  { tsymbol  = '>'
  , tname    = "lift down"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrCyan
  , tcolor2  = BrCyan
  , tfeature = [Walkable, Clear, Lit, Exit, Cause $ Effect.Ascend (-1)]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = BrWhite
  , tfeature = []
  }
floorCorridorLit = TileKind
  { tsymbol  = '.'
  , tname    = "floor"
  , tfreq    = [ ("floorCorridorLit", 1), ("floorArenaLit", 1)
               , ("arenaSet", 1), ("noiseSet", 100), ("combatSet", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit]
  }
floorCorridorDark = floorCorridorLit
  { tfreq    = [ ("floorCorridorDark", 1)
               , ("arenaSet", 1), ("noiseSet", 100), ("combatSet", 100) ]
  , tcolor   = BrYellow
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear]
  }
floorItemLit = floorCorridorLit
  { tfreq    = []
  , tfeature = CanItem : tfeature floorCorridorLit
  }
floorItemDark = floorCorridorDark
  { tfreq    = []
  , tfeature = CanItem : tfeature floorCorridorDark
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("litLegend", 100), ("emptySet", 1)]
  , tfeature = CanActor : tfeature floorItemLit
  }
floorActorItemDark = floorItemDark
  { tfreq    = [("darkLegend", 100), ("emptySet", 1)]
  , tfeature = CanActor : tfeature floorItemDark
  }
floorRedDark = floorCorridorDark
  { tname    = "emergency walkway"
  , tfreq    = [("pathDark", 20)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Path : tfeature floorCorridorDark
  }
floorRedLit  = floorRedDark
  { tfreq    = [("pathLit", 20)]
  , tfeature = Lit : tfeature floorRedDark
  }
floorBlueDark = floorRedDark
  { tname    = "transport route"
  , tfreq    = [("pathDark", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorBlueLit = floorBlueDark
  { tfreq    = [("pathLit", 100)]
  , tfeature = Lit : tfeature floorBlueDark
  }
floorGreenDark = floorRedDark
  { tname    = "greenery path"
  , tfreq    = [("pathDark", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorGreenLit = floorGreenDark
  { tfreq    = [("pathLit", 100)]
  , tfeature = Lit : tfeature floorGreenDark
  }
