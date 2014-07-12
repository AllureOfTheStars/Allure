-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Rooms, halls and passages for Allure of the Stars.
module Content.PlaceKind ( cdefs ) where

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validate = validatePlaceKind
  , content =
      [rect, ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillarC, pillar3, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, oval, ovalFloor, ovalSquare, colonnade, colonnadeWide, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells]
  }
rect,        ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillarC, pillar3, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, oval, ovalFloor, ovalSquare, colonnade, colonnadeWide, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells :: PlaceKind

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100), ("ambush", 8)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["."]
  , poverride = []
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruin"
  , pfreq    = [("ambush", 17), ("battle", 100)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["X"]
  , poverride = []
  }
collapsed = PlaceKind
  { psymbol  = 'c'
  , pname    = "collapsed cavern"
  , pfreq    = [("noise", 1)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = ["#"]
  , poverride = [('#', "doorlessWallOver_#")]
  }
collapsed2 = collapsed
  { pfreq    = [("noise", 100), ("battle", 50)]
  , ptopLeft = [ "XX#"
               , "X##"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [("noise", 200), ("battle", 50)]
  , ptopLeft = [ "XXX#"
               , "X###"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [("noise", 400), ("battle", 200)]
  , ptopLeft = [ "XXX#"
               , "XXX#"
               , "X###"
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "pillar room"
  , pfreq    = [("rogue", 1000)]  -- larger rooms require support pillars
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "...."
               , ".#.."
               , "...."
               , "...."
               ]
  , poverride = []
  }
pillarC = pillar
  { ptopLeft = [ ".#.."
               , "#..."
               , "...."
               , "...."
               ]
  }
pillar3 = pillar
  { ptopLeft = [ "&.#."
               , "...."
               , "#..."
               , "...."
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 60)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ ".#"
               , "#."
               ]
  , poverride = []
  }
colonnadeWide = colonnade
  { pfence   = FWall
  , ptopLeft = [ ".."
               , ".#"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "lamp post"
  , pfreq    = [("ambush", 30), ("battle", 10)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "X.X"
               , ".O."
               , "X.X"
               ]
  , poverride = [('O', "lampPostOver_O")]
  }
lampPost2 = lampPost
  { ptopLeft = [ "..."
               , ".O."
               , "..."
               ]
  }
lampPost3 = lampPost
  { ptopLeft = [ "XX.XX"
               , "X...X"
               , "..O.."
               , "X...X"
               , "XX.XX"
               ]
  }
lampPost4 = lampPost
  { ptopLeft = [ "X...X"
               , "....."
               , "..O.."
               , "....."
               , "X...X"
               ]
  }
treeShade = PlaceKind
  { psymbol  = 't'
  , pname    = "tree shade"
  , pfreq    = [("skirmish", 100)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "sss"
               , "XOs"
               , "XXs"
               ]
  , poverride = [('O', "treeShadeOver_O"), ('s', "treeShadeOver_s")]
  }
treeShade2 = treeShade
  { ptopLeft = [ "sss"
               , "XOs"
               , "Xss"
               ]
  }
treeShade3 = treeShade
  { ptopLeft = [ "sss"
               , "sOs"
               , "XXs"
               ]
  }
oval = PlaceKind
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
  , poverride = []
  }
ovalFloor = oval  -- Without outer solid fence, visible from outside.
  { pfreq    = [("rogue", 10000)]
  , pfence   = FFloor
  , ptopLeft = [ "XXXX+#"
               , "XX###."
               , "X##..."
               , "X#...."
               , "+#...."
               , "#....."
               ]
  }
ovalSquare = ovalFloor
  { ptopLeft = [ "X###+"
               , "##..."
               , "#...."
               , "#...."
               , "+...."
               ]
  }
maze = PlaceKind
  { psymbol  = 'm'
  , pname    = "maze"
  , pfreq    = [("rogue", 20)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#.#.##"
               , "##.#.."
               , "#.##.#"
               , "#.#.#."
               ]
  , poverride = []
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
               , "#.&.##"
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
               , "#.#&.#"
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
  , pfreq    = [("rogue", 30)]
  , pcover   = CReflect
  , pfence   = FWall
  , ptopLeft = [ "..#"
               , "..#"
               , "##."
               ]
  , poverride = []
  }
-- TODO: obtain all the rest as places nested within places.
-- 3 places are enough, with 1 or 2 tiles between places,
-- on all sides, only vertical, only horizontal,
