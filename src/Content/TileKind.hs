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
      [wall, hardRock, pillar, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorCorridorDark, floorItemLit, floorItemDark, floorActorItemLit, floorActorItemDark, floorRed, floorBlue, floorGreen]
  }
wall,        hardRock, pillar, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorCorridorDark, floorItemLit, floorItemDark, floorActorItemLit, floorActorItemDark, floorRed, floorBlue, floorGreen :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [ ("litLegend", 100), ("darkLegend", 100), ("fillerWall", 1)
               , ("noiseSet", 55), ("combatSet", 5) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [HideAs "suspect wall"]
  }
hardRock = TileKind
  { tsymbol  = '#'
  , tname    = "hard rock"
  , tfreq    = [("hard rock", 1)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = [Impenetrable]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
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
  , tfeature = [Walkable, Clear, Lit, Exit, Cause $ Effect.Descend 1]
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
  { tsymbol  = '>'
  , tname    = "lift"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrBlue
  , tcolor2  = BrBlue
  , tfeature = [ Walkable, Clear, Lit, Exit
               , Cause $ Effect.Ascend 1
               , Cause $ Effect.Descend 1 ]
  }
liftDown = TileKind
  { tsymbol  = '>'
  , tname    = "lift down"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrCyan
  , tcolor2  = BrCyan
  , tfeature = [Walkable, Clear, Lit, Exit, Cause $ Effect.Descend 1]
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
  , tfreq    = [("floorArenaLit", 1), ("noiseSet", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit]
  }
floorCorridorDark = floorCorridorLit
  { tfreq    = [("darkCorridor", 1)]
  , tcolor   = BrYellow
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear]
  }
floorItemLit = floorCorridorLit
  { tfreq    = [("combatSet", 100)]
  , tfeature = CanItem : tfeature floorCorridorLit
  }
floorItemDark = floorCorridorDark
  { tfreq    = []
  , tfeature = CanItem : tfeature floorCorridorDark
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("litLegend", 100), ("floorRoomLit", 1)]
  , tfeature = CanActor : tfeature floorItemLit
  }
floorActorItemDark = floorItemDark
  { tfreq    = [("darkLegend", 100)]
  , tfeature = CanActor : tfeature floorItemDark
  }
floorRed = floorCorridorLit
  { tname    = "emergency walkway"
  , tfreq    = [("path", 20)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Path : tfeature floorCorridorLit
  }
floorBlue = floorRed
  { tname    = "transport route"
  , tfreq    = [("path", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreen = floorRed
  { tname    = "greenery path"
  , tfreq    = [("path", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
