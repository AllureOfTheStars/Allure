{-# LANGUAGE OverloadedStrings #-}
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
      [wall, hardRock, pillar, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen]
  }
wall,        hardRock, pillar, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [ ("litLegend", 100), ("darkLegend", 100)
               , ("fillerWall", 1), ("noiseSet", 55) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [HiddenAs "suspect wall"]
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
               , ChangeTo "closed door"  -- never triggered, hack 47
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100), ("closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [ Exit, Openable
               , ChangeTo "open door"
               , HiddenAs "suspect wall"
               ]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100), ("open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, Closable, ChangeTo "closed door"]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [ Walkable, Clear, Lit, Exit, Ascendable
               , Cause $ Effect.Ascend 1 ]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("litLegend", 100), ("darkLegend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [ Walkable, Clear, Lit, Exit, Descendable
               , Cause $ Effect.Descend 1 ]
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
  , tfreq    = [("noiseSet", 100), ("floorArenaLit", 1)]
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
floorRoomLit = floorCorridorLit
  { tfreq    = [("litLegend", 100), ("floorRoomLit", 1)]
  , tfeature = Boring : tfeature floorCorridorLit
  }
floorRoomDark = floorCorridorDark
  { tfreq    = [("darkLegend", 100)]
  , tfeature = Boring : tfeature floorCorridorDark
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
