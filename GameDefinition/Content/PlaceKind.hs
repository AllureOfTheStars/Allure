-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validateSingle = validateSinglePlaceKind
  , validateAll = validateAllPlaceKind
  , content = contentFromList $
      [rect, rectWindows, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, colonnadeWide, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells]
      ++ map makeStaircaseUp lstaircase
      ++ map makeStaircaseDown lstaircase
  }
rect,        rectWindows, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, colonnadeWide, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17]

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 70), ("arena", 70), ("empty", 70)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["."]
  , poverride = []
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "room"
  , pfreq    = [("ambush", 8), ("noise", 80)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#%"
               , "%."
               ]
  , poverride = [('%', "wallOrGlassOver_%_Lit")]
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruin"
  , pfreq    = [("ambush", 17), ("battle", 33), ("noise", 40)]
  , prarity  = [(1, 10), (10, 20)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["X"]
  , poverride = []
  }
collapsed = PlaceKind  -- in a dark cave, they have little lights --- that's OK
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
  { pfreq    = [("noise", 100), ("battle", 20)]
  , ptopLeft = [ "X#"
               , "##"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [("noise", 200), ("battle", 20)]
  , ptopLeft = [ "XX#"
               , "###"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [("noise", 200), ("battle", 20)]
  , ptopLeft = [ "XXX#"
               , "####"
               ]
  }
collapsed5 = collapsed
  { pfreq    = [("noise", 300), ("battle", 50)]
  , ptopLeft = [ "XX#"
               , "X##"
               , "###"
               ]
  }
collapsed6 = collapsed
  { pfreq    = [("noise", 400), ("battle", 100)]
  , ptopLeft = [ "XXX#"
               , "X###"
               , "####"
               ]
  }
collapsed7 = collapsed
  { pfreq    = [("noise", 400), ("battle", 100)]
  , ptopLeft = [ "XXX#"
               , "XX##"
               , "####"
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "pillar room"
  , pfreq    = [ ("rogue", 500), ("arena", 1000), ("empty", 1000)
               , ("noise", 50) ]
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
  { prarity  = [(10, 10)]
  , ptopLeft = [ "..#."
               , "..#."
               , "##.."
               , "...."
               ]
  }
pillar3 = pillar
  { prarity  = [(10, 5)]
  , ptopLeft = [ "&.#."
               , "...."
               , "#.#."
               , "...."
               ]
  }
pillar4 = pillar
  { prarity  = [(10, 5)]
  , ptopLeft = [ "&.#."
               , "...."
               , "#..."
               , "...."
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 30), ("arena", 70), ("noise", 2000)]
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
  , pfreq    = [("rogue", 100)]
  , ptopLeft = [ ".."
               , ".O"
               ]
  }
colonnade3 = colonnade
  { ptopLeft = [ "..#"
               , ".#."
               , "#.."
               ]
  }
colonnade4 = colonnade
  { ptopLeft = [ "#.."
               , ".#."
               , "..#"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 4), (10, 4)]
  , ptopLeft = [ "#.."
               , "..#"
               ]
  }
colonnade6 = colonnade
  { ptopLeft = [ "#."
               , ".."
               , ".#"
               ]
  }
colonnadeWide = colonnade
  { prarity  = [(1, 3), (10, 3)]
  , pfence   = FWall
  , ptopLeft = [ "#."
               , ".."
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
  , pfreq    = [("brawl", 100)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "sss"
               , "XOs"
               , "XXs"
               ]
  , poverride = [('O', "treeShadeOver_O"), ('s', "treeShadeOrFogOver_s")]
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
staircase = PlaceKind
  { psymbol  = '|'
  , pname    = "staircase"
  , pfreq    = [("staircase", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<.>"
               ]
  , poverride = [('<', "staircase up"), ('>', "staircase down")]
  }
staircase2 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O.O"
               , "..."
               , "<.>"
               , "..."
               , "O.O"
               ]
  }
staircase3 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.#.#"
               , "....."
               , ".<.>."
               , "....."
               , "#.#.#"
               ]
  }
staircase4 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.#.#.#"
               , "......."
               , "#.<.>.#"
               , "......."
               , "#.#.#.#"
               ]
  }
staircase5 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "O.<.>.O"
               ]
  }
staircase6 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "O..<.>..O"
               ]
  }
staircase7 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "O.O.<.>.O.O"
               ]
  }
staircase8 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O.....O"
               , "..<.>.."
               , "O.....O"
               ]
  }
staircase9 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O.......O"
               , ".O.<.>.O."
               , "O.......O"
               ]
  }
staircase10 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.#.....#.#"
               , ".#..<.>..#."
               , "#.#.....#.#"
               ]
  }
staircase11 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FGround
  , ptopLeft = [ "XX#.#XX"
               , "#.....#"
               , "..<.>.."
               , "#.....#"
               , "XX#.#XX"
               ]
  }
staircase12 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "....."
               , ".<.>."
               , "....."
               ]
  }
staircase13 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "......."
               , "#.<.>.#"
               , "......."
               ]
  }
staircase14 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "........."
               , ".O.<.>.O."
               , "........."
               ]
  }
staircase15 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "..........."
               , "#.O.<.>.O.#"
               , "..........."
               ]
  }
staircase16 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.....#"
               , "..<.>.."
               , "#.....#"
               ]
  }
staircase17 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.......#"
               , ".#.<.>.#."
               , "#.......#"
               ]
  }
escapeUp = PlaceKind
  { psymbol  = '<'
  , pname    = "escape up"
  , pfreq    = [("escape up", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<"
               ]
  , poverride = []
  }
escapeUp2 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O.O"
               , ".<."
               , "O.O"
               ]
  }
escapeUp3 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.#"
               , ".<."
               , "#.#"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 2000)]
  , pcover   = CMirror
  , pfence   = FWall
  , ptopLeft = [ "#.."
               , ".<."
               , "#.#"
               ]
  }
escapeUp5 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FGround
  , ptopLeft = [ "XX#XX"
               , "#...#"
               , "..<.."
               , "#...#"
               , "XX#XX"
               ]
  }
escapeDown = PlaceKind
  { psymbol  = '>'
  , pname    = "escape down"
  , pfreq    = [("escape down", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ ">"
               ]
  , poverride = []
  }
escapeDown2 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "O.O"
               , ".>."
               , "O.O"
               ]
  }
escapeDown3 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.#"
               , ".>."
               , "#.#"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 2000)]
  , pcover   = CMirror
  , pfence   = FWall
  , ptopLeft = [ "#.."
               , ".>."
               , "#.#"
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FGround
  , ptopLeft = [ "XX#XX"
               , "#...#"
               , "..>.."
               , "#...#"
               , "XX#XX"
               ]
  }
oval = PlaceKind
  { psymbol  = 'o'
  , pname    = "oval room"
  , pfreq    = [("rogue", 1000), ("arena", 1000), ("empty", 1000)]
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
  { pfreq    = [("rogue", 10000), ("arena", 10000), ("empty", 10000)]
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
  { pfreq    = [("rogue", 3000), ("arena", 3000), ("empty", 3000)]
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
  , pfreq    = [("rogue", 20), ("arena", 20), ("empty", 20)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#.#.##"
               , "##.#.."
               , ".###.#"
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
  { pfreq    = [("rogue", 1000), ("arena", 1000), ("empty", 1000)]
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
  , pfreq    = [("rogue", 100), ("arena", 100), ("empty", 100), ("noise", 100)]
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

makeStaircaseUp :: PlaceKind -> PlaceKind
makeStaircaseUp s = s
 { psymbol   = '<'
 , pname     = "staircase up"
 , pfreq     = map (\(_, k) -> ("staircase up", k)) $ pfreq s
 , poverride = [('>', "stair terminal"), ('<', "staircase up")]
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = "staircase down"
 , pfreq     = map (\(_, k) -> ("staircase down", k)) $ pfreq s
 , poverride = [('<', "stair terminal"), ('>', "staircase down")]
 }
