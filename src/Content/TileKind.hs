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
      [wall, pillar, doorOpen, doorClosed, doorSecret, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen]
  }
wall,        pillar, doorOpen, doorClosed, doorSecret, stairsUp, stairsDown, unknown, floorCorridorLit, floorCorridorDark, floorRoomLit, floorRoomDark, floorRed, floorBlue, floorGreen :: TileKind

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
  , tfeature = [ Walkable, Clear, Exit, Closable
               , ChangeTo "closed door", ChangeFrom "open door"
               ]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tcolor   = Yellow
  , tcolor2  = BrBlack
  , tfreq    = 100
  , tfeature = [ Exit, Openable
               , ChangeTo "open door", ChangeFrom "closed door"
               ]
  }
doorSecret = wall
  { tfeature = [Hidden, Secret (RollDice 7 2), ChangeTo "closed door"]
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
  , tname    = "floor"
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
  { tname    = "emergency walkway"
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfreq    = 20
  , tfeature = Special : tfeature floorCorridorLit
  }
floorBlue = floorRed
  { tname    = "transport route"
  , tcolor   = BrBlue
  , tcolor2  = Blue
  , tfreq    = 100
  }
floorGreen = floorRed
  { tname    = "greenery path"
  , tcolor   = BrGreen
  , tcolor2  = Green
  , tfreq    = 100
  }
