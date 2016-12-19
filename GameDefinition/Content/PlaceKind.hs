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
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.PlaceKind

cdefs :: ContentDef PlaceKind
cdefs = ContentDef
  { getSymbol = psymbol
  , getName = pname
  , getFreq = pfreq
  , validateSingle = validateSinglePlaceKind
  , validateAll = validateAllPlaceKind
  , content = contentFromList $
      [rect, rectWindows, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, colonnadeWide, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, staircase, staircaseLift, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells, cells2, cells3, cells4, cells5, cells6, cells7]
      ++ map makeStaircaseUp lstaircase
      ++ map makeStaircaseDown lstaircase
  }
rect,        rectWindows, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, colonnadeWide, lampPost, lampPost2, lampPost3, lampPost4, treeShade, treeShade2, treeShade3, staircase, staircaseLift, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells, cells2, cells3, cells4, cells5, cells6, cells7 :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircaseLift, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor]

rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 50), ("arena", 20), ("empty", 1)]
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
               , ".#.."
               , "...."
               , "...."
               ]
  , poverride = []
  }
pillar2 = pillar
  { ptopLeft = [ "..#."
               , "..#."
               , "##.."
               , "...."
               ]
  }
pillar3 = pillar
  { prarity  = [(10, 3)]
  , ptopLeft = [ "&.#."
               , "..#."
               , "##.."
               , "...."
               ]
  }
pillar4 = pillar
  { prarity  = [(10, 3)]
  , ptopLeft = [ "&#.."
               , "#.#."
               , ".##."
               , "...."
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [("rogue", 30), ("arena", 70), ("empty", 70), ("noise", 2000)]
  , prarity  = [(1, 5), (10, 5)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "#."
               , ".#"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { ptopLeft = [ "#."
               , ".."
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "..#"
               , ".#."
               , "#.."
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "#.."
               , ".#."
               , "..#"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 8), (10, 8)]
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
  , ptopLeft = [ ".."
               , "#."
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
staircaseLift = staircase
  { pname    = "staircase lift"
  , pfreq    = [("staircase lift", 1)]
  , poverride = [('<', "staircase lift up"), ('>', "staircase lift down")]
  }
staircase2 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "#.#"
               , "..."
               , "<.>"
               , "..."
               , "#.#"
               ]
  }
staircase3 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.#.#"
               , "....."
               , ".<.>."
               , "....."
               , "#.#.#"
               ]
  }
staircase4 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
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
  , ptopLeft = [ "#.<.>.#"
               ]
  }
staircase6 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "#..<.>..#"
               ]
  }
staircase7 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround

  , ptopLeft = [ "#.#.<.>.#.#"
               ]
  }
staircase8 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "#.....#"
               , "..<.>.."
               , "#.....#"
               ]
  }
staircase9 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.......#"
               , ".#.<.>.#."
               , "#.......#"
               ]
  }
staircase10 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ ".#.....#."
               , "#..<.>..#"
               , ".#.....#."
               ]
  }
staircase11 = staircase
  { pfreq    = [("staircase", 10000)]
  , pfence   = FGround
  , ptopLeft = [ "..#.#.."
               , "#.....#"
               , "..<.>.."
               , "#.....#"
               , "..#.#.."
               ]
  }
staircase12 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "....."
               , ".<.>."
               , "....."
               ]
  }
staircase13 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "......."
               , "#.<.>.#"
               , "......."
               ]
  }
staircase14 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "........."
               , ".#.<.>.#."
               , "........."
               ]
  }
staircase15 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "..........."
               , "#.#.<.>.#.#"
               , "..........."
               ]
  }
staircase16 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#.....#"
               , "..<.>.."
               , "#.....#"
               ]
  }
staircase17 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ ".#.....#."
               , "#..<.>..#"
               , ".#.....#."
               ]
  }
staircaseOutdoor = staircase
  { pname    = "staircase outdoor"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = [('<', "staircase outdoor up"), ('>', "staircase outdoor down")]
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
  , ptopLeft = [ "#.#"
               , ".<."
               , "#.#"
               ]
  }
escapeUp3 = escapeUp
  { pfreq    = [("escape down", 2000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "#.."
               , ".<."
               , "#.#"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FWall
  , ptopLeft = [ ".#."
               , "#<#"
               , ".#."
               ]
  }
escapeUp5 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pcover   = CMirror
  , pfence   = FWall
  , ptopLeft = [ "#.."
               , ".<."
               , "..#"
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
  , ptopLeft = [ "#.#"
               , ".>."
               , "#.#"
               ]
  }
escapeDown3 = escapeDown
  { pfreq    = [("escape down", 2000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "#.."
               , ".>."
               , "#.#"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FWall
  , ptopLeft = [ ".#."
               , "#>#"
               , ".#."
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pcover   = CMirror
  , pfence   = FWall
  , ptopLeft = [ "#.."
               , ".>."
               , "..#"
               ]
  }
escapeOutdoorDown = escapeDown
  { pfreq     = [("escape outdoor down", 1)]
  , poverride = [('>', "escape outdoor down")]
  }
escapeSpaceshipDown = escapeDown
  { pfreq     = [("escape spaceship down", 1)]
  , poverride = [('>', "escape spaceship down")]
  }
escapeSpaceshipDown2 = escapeDown2
  { pfreq     = [("escape spaceship down", 1000)]
  , poverride = [('>', "escape spaceship down")]
  }
escapeSpaceshipDown3 = escapeDown3
  { pfreq     = [("escape spaceship down", 2000)]
  , poverride = [('>', "escape spaceship down")]
  }
escapeSpaceshipDown4 = escapeDown4
  { pfreq     = [("escape spaceship down", 1000)]
  , poverride = [('>', "escape spaceship down")]
  }
escapeSpaceshipDown5 = escapeDown5
  { pfreq     = [("escape spaceship down", 1000)]
  , poverride = [('>', "escape spaceship down")]
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
  , ptopLeft = [ ".####"
               , "#.#.."
               , "##..#"
               , "#..#."
               ]
  , poverride = []
  }
maze2 = maze
  { pfreq    = [("rogue", 50), ("arena", 50), ("empty", 50)]
  , ptopLeft = [ "..####"
               , "##.##."
               , "#.#..#"
               , "#...#."
               ]
  }
maze3 = maze
  { pfreq    = [("rogue", 70), ("arena", 70), ("empty", 70)]
  , ptopLeft = [ ".######"
               , ".##.##."
               , "#..#..."
               , "#..#..#"
               ]
  }
mazeBig = maze
  { pfreq    = [("rogue", 500), ("arena", 500), ("empty", 500)]
  , ptopLeft = [ "..####"
               , ".#.#.."
               , "##&.##"
               , "##.##."
               , "#.#..#"
               ]
  }
mazeBig2 = mazeBig
  { ptopLeft = [ "..####"
               , "##.##."
               , "#.#&.."
               , "#.#..#"
               , "#...#."
               ]
  }
mazeBig3 = mazeBig
  { pfreq    = [("rogue", 800), ("arena", 800), ("empty", 800)]
  , ptopLeft = [ ".######"
               , "#.##.#."
               , "#.#.##."
               , "##.&#.#"
               , "#..#..."
               ]
  }
cells = PlaceKind
  { psymbol  = '#'
  , pname    = "cells"
  , pfreq    = [("rogue", 50), ("arena", 50), ("empty", 50), ("noise", 50)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CReflect
  , pfence   = FWall
  , ptopLeft = [ "..#"
               , "..#"
               , "##."
               ]
  , poverride = []
  }
cells2 = cells
  { ptopLeft = [ "..#"
               , ".#."
               , "#.."
               ]
  }
cells3 = cells
  { ptopLeft = [ "#.."
               , ".#."
               , "..#"
               ]
  }
cells4 = cells
  { ptopLeft = [ "..#"
               , ".##"
               , "#.."
               ]
  }
cells5 = cells
  { ptopLeft = [ "..#"
               , ".#."
               , "##."
               ]
  }
cells6 = cells
  { pfreq    = [("rogue", 1), ("arena", 2), ("empty", 10), ("noise", 1)]
  , ptopLeft = [ "..#"
               , "##."
               ]
  }
cells7 = cells
  { pfreq    = [("rogue", 1), ("arena", 2), ("empty", 10), ("noise", 1)]
  , pfence   = FFloor
  , ptopLeft = [ "#.#"
               , ".#."
               ]
  }

makeStaircaseUp :: PlaceKind -> PlaceKind
makeStaircaseUp s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "up", k)) $ pfreq s
 , poverride = [ ('>', toGroupName $ pname s <+> "terminal")
               , ('<', toGroupName $ pname s <+> "up") ]
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "down", k)) $ pfreq s
 , poverride = [ ('<', toGroupName $ pname s <+> "terminal")
               , ('>', toGroupName $ pname s <+> "down") ]
 }
