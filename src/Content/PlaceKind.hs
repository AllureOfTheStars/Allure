-- | Rooms, halls and passages for Allure of the Stars.
module Content.PlaceKind ( cdefs ) where

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Content.PlaceKind

cdefs :: Content.CDefs PlaceKind
cdefs = Content.CDefs
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validate = pvalidate
  , content =
      [rect, oval, ovalFloor, ovalSquare, colonnade, colonnadeWide, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells]
  }
rect,        oval, ovalFloor, ovalSquare, colonnade, colonnadeWide, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells :: PlaceKind

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100)]
  , pcover   = CTile
  , pfence   = FWall
  , ptopLeft = ["."]
  }
oval = PlaceKind  -- Needs a large area, hence high frequency.
  { psymbol  = 'o'
  , pname    = "oval room"
  , pfreq    = [("rogue", 1000)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "####.."
               , "##...."
               , "#....."
               , "#....."
               , "......"
               , "......"
               ]
  }
ovalFloor = oval  -- Without outer solid fence, visible from outside.
  { pfence   = FFloor
  , ptopLeft = [ "....+#"
               , "..###."
               , ".##..."
               , ".#...."
               , "+#...."
               , "#....."
               ]
  }
ovalSquare = ovalFloor
  { ptopLeft = [ ".###+"
               , "##..."
               , "#...."
               , "#...."
               , "+...."
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 50)]
  , pcover   = CTile
  , pfence   = FFloor
  , ptopLeft = [ ".#"
               , "#."
               ]
  }
colonnadeWide = colonnade
  { pfence   = FWall
  , ptopLeft = [ "...."
               , ".#.#"
               , "...."
               , ".#.#"
               ]
  }
maze = PlaceKind
  { psymbol  = 'm'
  , pname    = "maze"
  , pfreq    = [("rogue", 50)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#.#.##"
               , "##.#.."
               , "#.##.#"
               , "#.#.#."
               ]
  }
maze2 = maze
  { ptopLeft = [ "###.##"
               , ".###.."
               , "..#..#"
               , ".#..#."
               ]
  }
maze3 = maze
  { ptopLeft = [ "###.##"
               , ".##.#."
               , "..##.#"
               , ".#..#."
               ]
  }
mazeBig = maze
  { pfreq    = [("rogue", 1000)]
  , ptopLeft = [ "#.#.##"
               , ".#.#.."
               , "#.#.##"
               , ".#.#.."
               , "#.#..#"
               , "#.#.#."
               ]
  }
mazeBig2 = mazeBig
  { ptopLeft = [ "##..##"
               , "#.##.."
               , ".#.###"
               , ".##.#."
               , "#.##.#"
               , "#.#.#."
               ]
  }
mazeBig3 = mazeBig
  { ptopLeft = [ "##..##"
               , "#.###."
               , ".#...#"
               , ".#.##."
               , "##.#.#"
               , "#.#.#."
               ]
  }
cells = PlaceKind
  { psymbol  = '#'
  , pname    = "cells"
  , pfreq    = [("rogue", 50)]
  , pcover   = CReflect
  , pfence   = FWall
  , ptopLeft = [ "..#"
               , "..#"
               , "##."
               ]
  }
-- TODO: obtain all the reet as places nested within places.
-- 3 places are enough, with 1 or 2 tiles between places,
-- on all sides, only vertical, only horizontal,
