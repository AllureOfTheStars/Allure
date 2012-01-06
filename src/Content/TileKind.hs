-- | Terrain tiles for Allure of the Stars.
module Content.TileKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content.Content as Content
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Feature
import Game.LambdaHack.Content.TileKind
import Game.LambdaHack.Random

cdefs :: Content.CDefs TileKind
cdefs = Content.CDefs
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = tvalidate
  , content =
      [wall, doorHidden, doorClosed, doorOpen, pillar, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen]
  }
wall,        doorHidden, doorClosed, doorOpen, pillar, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tfreq    = [("legend", 100), ("fillerWall", 1), ("noiseSet", 60)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
doorHidden = wall
  { tfreq    = [("hidden", 100)]
  , tfeature = [Hidden, Secret (RollDice 7 2), ChangeTo "closed door"]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("legend", 100), ("closed door", 1)]
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfeature = [Exit, Openable, ChangeTo "open door"]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [("legend", 100), ("open door", 1)]
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear, Exit, Closable, ChangeTo "closed door"]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tfreq    = [("legend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = [("legend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit, Exit, Ascendable, Cause Effect.Ascend]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = [("legend", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Lit, Exit, Descendable, Cause Effect.Descend]
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
  { tfreq    = [("legend", 100), ("floorRoomLit", 1)]
  , tfeature = Boring : tfeature floorCorridorLit
  }
floorRoomDark = floorCorridorDark
  { tfreq    = [("floorRoomDark", 1)]
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
