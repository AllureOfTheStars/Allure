module Content.RoomKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.RoomKind

cdefs :: Content.CDefs RoomKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validate = rvalidate
  , content =
      [rect, oval, ovalFloor, ovalSquare, colonnade, colonnadeWide, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells]
  }
rect,        oval, ovalFloor, ovalSquare, colonnade, colonnadeWide, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells :: RoomKind

rect = RoomKind  -- room is valid for any nonempty area, hence low frequency
  { rsymbol  = 'r'
  , rname    = "room"
  , rfreq    = 100
  , rcover   = CTile
  , rfence   = FWall
  , rtopLeft = ["."]
  }
oval = RoomKind  -- needs a large area, hence high frequency
  { rsymbol  = 'o'
  , rname    = "oval room"
  , rfreq    = 1000
  , rcover   = CStretch
  , rfence   = FWall
  , rtopLeft = [ "####.."
               , "##...."
               , "#....."
               , "#....."
               , "......"
               , "......"
               ]
  }
ovalFloor = oval  -- without outer solid fence, the pattern visible from outside
  { rfence   = FFloor
  , rtopLeft = [ "....+#"
               , "..###."
               , ".##..."
               , ".#...."
               , "+#...."
               , "#....."
               ]
  }
ovalSquare = ovalFloor
  { rtopLeft = [ ".###+"
               , "##..."
               , "#...."
               , "#...."
               , "+...."
               ]
  }
colonnade = RoomKind
  { rsymbol  = 'c'
  , rname    = "colonnade"
  , rfreq    = 50
  , rcover   = CTile
  , rfence   = FFloor
  , rtopLeft = [ ".#"
               , "#."
               ]
  }
colonnadeWide = colonnade
  { rfence   = FWall
  , rtopLeft = [ "...."
               , ".#.#"
               , "...."
               , ".#.#"
               ]
  }
maze = RoomKind
  { rsymbol  = 'm'
  , rname    = "maze"
  , rfreq    = 50
  , rcover   = CStretch
  , rfence   = FNone
  , rtopLeft = [ "#.#.##"
               , "##.#.."
               , "#.##.#"
               , "#.#.#."
               ]
  }
maze2 = maze
  { rtopLeft = [ "###.##"
               , ".###.."
               , "..#..#"
               , ".#..#."
               ]
  }
maze3 = maze
  { rtopLeft = [ "###.##"
               , ".##.#."
               , "..##.#"
               , ".#..#."
               ]
  }
mazeBig = maze
  { rfreq    = 1000
  , rtopLeft = [ "#.#.##"
               , ".#.#.."
               , "#.#.##"
               , ".#.#.."
               , "#.#..#"
               , "#.#.#."
               ]
  }
mazeBig2 = mazeBig
  { rtopLeft = [ "##..##"
               , "#.##.."
               , ".#.###"
               , ".##.#."
               , "#.##.#"
               , "#.#.#."
               ]
  }
mazeBig3 = mazeBig
  { rtopLeft = [ "##..##"
               , "#.###."
               , ".#...#"
               , ".#.##."
               , "##.#.#"
               , "#.#.#."
               ]
  }
cells = RoomKind
  { rsymbol  = '#'
  , rname    = "cells"
  , rfreq    = 50
  , rcover   = CReflect
  , rfence   = FWall
  , rtopLeft = [ "..#"
               , "..#"
               , "##."
               ]
  }
-- TODO: obtain all the reet as rooms nested within rooms. 3 rooms are enough,
-- with 1 or 2 tiles between rooms, on all sides, only vertical, only horizontal,
