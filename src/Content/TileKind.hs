module Content.TileKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content.Content as Content
import qualified Game.LambdaHack.Effect as Effect
import Game.LambdaHack.Feature
import Game.LambdaHack.Content.TileKind

cdefs :: Content.CDefs TileKind
cdefs = Content.CDefs
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = tvalidate
  , content =
      [wall, pillar, doorOpen, doorClosed, doorSecret, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown]
  }
wall,        pillar, doorOpen, doorClosed, doorSecret, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen, floorBrown :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "wall"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = []
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "pillar"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Special]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Exit, Change '+', Closable]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [Exit, Change '\'', Openable]
  }
doorSecret = wall
  { tfeature = [Hidden, Change '+', Secret (7, 2)]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Climbable, Cause Effect.Teleport]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit, Exit, Descendable, Cause Effect.Teleport]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tcolor   = defFG
  , tcolor2  = BrWhite
  , tfreq    = 0
  , tfeature = [Boring]
  }
floorCorridorLit = TileKind
  { tsymbol  = '.'
  , tname    = "dirt"
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfreq    = 100
  , tfeature = [Walkable, Clear, Lit]
  }
floorCorridorDark = floorCorridorLit
  { tcolor   = BrYellow
  , tcolor2  = BrBlack
  , tfeature = [Walkable, Clear]
  }
floorRoomLit = floorCorridorLit
  { tfeature = Boring : tfeature floorCorridorLit
  }
floorRoomDark = floorCorridorDark
  { tfeature = Boring : tfeature floorCorridorDark
  }
floorRed = floorCorridorLit
  { tname    = "brick pavement"
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Special : tfeature floorCorridorLit
  }
floorBlue = floorRed
  { tname    = "granite cobblestones"
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreen = floorRed
  { tname    = "mossy stone path"
  , tcolor   = BrGreen
  , tcolor2  = Green
  }
floorBrown = floorRed
  { tname    = "rotting mahogany deck"
  , tcolor   = BrMagenta
  , tcolor2  = Magenta
  }
