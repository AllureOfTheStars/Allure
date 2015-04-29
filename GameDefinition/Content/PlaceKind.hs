-- Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Room, hall and passage definitions.
module Content.PlaceKind ( cdefs ) where

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validateSingle = validateSinglePlaceKind
  , validateAll = validateAllPlaceKind
  , content =
      [rect, ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnadeWide, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells]
  }
rect,        ruin, collapsed, collapsed2, collapsed3, collapsed4, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnadeWide, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells :: PlaceKind

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 70), ("ambush", 8), ("noise", 80)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["."]
  , poverride = []
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruin"
  , pfreq    = [("ambush", 17), ("battle", 100), ("noise", 40)]
  , prarity  = [(1, 10), (10, 20)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["X"]
  , poverride = []
  }
collapsed = PlaceKind
  { psymbol  = 'c'
  , pname    = "collapsed cavern"
  , pfreq    = [("noise", 1)]
  , prarity  = [(1, 10), (10, 10)]
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
  , pfreq    = [("rogue", 1000), ("noise", 50)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FWall
  -- Larger rooms require support pillars.
  , ptopLeft = [ "...."
               , ".O.."
               , "...."
               , "...."
               ]
  , poverride = []
  }
pillar2 = pillar
  { prarity  = [(1, 5), (10, 5)]
  , ptopLeft = [ ".#.."
               , "#..."
               , "...."
               , "...."
               ]
  }
pillar3 = pillar
  { prarity  = [(1, 5), (10, 5)]
  , ptopLeft = [ "#..."
               , "..#."
               , ".#.."
               , "...."
               ]
  }
pillar4 = pillar
  { prarity  = [(10, 7)]
  , ptopLeft = [ "&.#."
               , "...."
               , "#..."
               , "...."
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 70), ("noise", 2000)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ ".#"
               , "#."
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 2), (10, 4)]
  , pfence   = FGround
  , ptopLeft = [ ".."
               , ".O"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 4), (10, 6)]
  , ptopLeft = [ "#.."
               , "..#"
               ]
  }
colonnade4 = colonnade
  { ptopLeft = [ "#."
               , ".."
               , ".#"
               ]
  }
colonnadeWide = colonnade
  { prarity  = [(1, 3), (10, 3)]
  , pfence   = FWall
  , ptopLeft = [ ".."
               , ".#"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "lamp post"
  , pfreq    = [("ambush", 30), ("battle", 10)]
  , prarity  = [(1, 10), (10, 10)]
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
  , prarity  = [(1, 10), (10, 10)]
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
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "####."
               , "##..."
               , "#...."
               , "#...."
               , "....."
               ]
  , poverride = []
  }
ovalFloor = oval  -- Without outer solid fence, visible from outside.
  { pfreq    = [("rogue", 10000)]
  , pfence   = FGround
  , ptopLeft = [ "XXXX+#"
               , "XX###."
               , "X##..."
               , "X#...."
               , "+#...."
               , "#....."
               ]
  }
ovalSquare = ovalFloor
  { pfreq    = [("rogue", 3000)]
  , ptopLeft = [ "X###+"
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
  , prarity  = [(1, 10), (10, 10)]
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
  , pfreq    = [("rogue", 100), ("noise", 100)]
  , prarity  = [(1, 10), (10, 10)]
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
