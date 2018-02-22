-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Room, hall and passage definitions.
module Content.PlaceKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.PlaceKind

content :: [PlaceKind]
content =
  [rect, rectWindows, glasshouse, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown]
  ++ map makeStaircaseUp lstaircase
  ++ map makeStaircaseDown lstaircase
  -- Allure-specific
  ++ [staircaseLift, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, colonnadeWide, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells, cells2, cells3, cells4, cells5, cells6, cells7]

rect,    rectWindows, glasshouse, pulpit, ruin, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown :: PlaceKind
-- Allure-specific
staircaseLift, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, colonnadeWide, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, mazeBig3, cells, cells2, cells3, cells4, cells5, cells6, cells7 :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated]
  -- Allure-specific
  ++ [staircaseLift]

-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "room"
  , pfreq    = [("rogue", 100), ("arena", 40), ("laboratory", 40), ("zoo", 9)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["·"]
  , poverride = []
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "room"
  , pfreq    = [("empty", 10), ("park", 6)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#%"
               , "%·"
               ]
  , poverride = [('%', "rectWindowsOver_%_Lit")]
      -- for now I need to specify 'Lit' or I'd be randomly getting lit and dark
      -- tiles, until ooverride is extended to take night/dark into account
  }
glasshouse = PlaceKind
  { psymbol  = 'g'
  , pname    = "glasshouse"
  , pfreq    = [("arena", 40), ("shootout", 8), ("zoo", 9), ("ambush", 7)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "%%"
               , "%·"
               ]
  , poverride = [('%', "glasshouseOver_%_Lit")]
  }
pulpit = PlaceKind
  { psymbol  = 'p'
  , pname    = "pulpit"
  , pfreq    = [("arena", 10), ("zoo", 30)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ "%%·"
               , "%··"
               , "··O"
               ]
  , poverride = [('%', "glasshouseOver_%_Lit"), ('O', "pulpit")]
      -- except for floor, this will all be lit, regardless of night/dark; OK
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruin"
  , pfreq    = [("battle", 33), ("noise", 50)]
  , prarity  = [(1, 10), (10, 20)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["X"]
  , poverride = []
  }
collapsed = PlaceKind  -- in a dark cave, they have little lights --- that's OK
  { psymbol  = 'c'
  , pname    = "hardware stack"
  , pfreq    = [("noise", 1)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#"
               ]
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
  , pname    = "commercial space"
  , pfreq    = [ ("rogue", 500), ("arena", 1000), ("laboratory", 1000)
               , ("empty", 300), ("noise", 1000) ]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FWall
  -- Larger rooms require support pillars.
  , ptopLeft = [ "····"
               , "·#··"
               , "····"
               , "····"
               ]
  , poverride = [('&', "cachable")]
  }
pillar2 = pillar
  { ptopLeft = [ "··#·"
               , "··#·"
               , "##··"
               , "····"
               ]
  }
pillar3 = pillar
  { prarity  = [(10, 5)]
  , ptopLeft = [ "&·#·"
               , "··#·"
               , "##··"
               , "····"
               ]
  }
pillar4 = pillar
  { prarity  = [(10, 5)]
  , ptopLeft = [ "&#··"
               , "#·#·"
               , "·##·"
               , "····"
               ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "colonnade"
  , pfreq    = [ ("rogue", 30), ("arena", 70), ("laboratory", 40)
               , ("empty", 100), ("mine", 10000), ("park", 4000) ]
  , prarity  = [(1, 3), (10, 3)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "#·"
               , "·#"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 2), (10, 2)]
  , ptopLeft = [ "#·"
               , "··"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "··#"
               , "·#·"
               , "#··"
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "#··"
               , "·#·"
               , "··#"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 7), (10, 7)]
  , ptopLeft = [ "#··"
               , "··#"
               ]
  }
colonnade6 = colonnade
  { ptopLeft = [ "#·"
               , "··"
               , "·#"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "lamp post"
  , pfreq    = [("park", 20), ("ambush", 20), ("zoo", 10), ("battle", 10)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "X·X"
               , "·O·"
               , "X·X"
               ]
  , poverride = [('O', "lampPostOver_O"), ('·', "floorActorLit")]
  }
lampPost2 = lampPost
  { ptopLeft = [ "···"
               , "·O·"
               , "···"
               ]
  }
lampPost3 = lampPost
  { pfreq    = [("park", 3000), ("zoo", 50), ("battle", 110)]
  , ptopLeft = [ "XX·XX"
               , "X···X"
               , "··O··"
               , "X···X"
               , "XX·XX"
               ]
  }
lampPost4 = lampPost
  { pfreq    = [("park", 3000), ("zoo", 50), ("battle", 60)]
  , ptopLeft = [ "X···X"
               , "·····"
               , "··O··"
               , "·····"
               , "X···X"
               ]
  }
treeShade = PlaceKind
  { psymbol  = 't'
  , pname    = "tree shade"
  , pfreq    = [("brawl", 300)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "··s"
               , "sO·"
               , "Xs·"
               ]
  , poverride = [ ('O', "treeShadeOver_O_Lit"), ('s', "treeShadeOver_s_Lit")
                , ('·', "shaded ground") ]
  }
fogClump = PlaceKind
  { psymbol  = 'f'
  , pname    = "foggy patch"
  , pfreq    = [("shootout", 170)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";f"
               ]
  , poverride = [('f', "fogClumpOver_f_Lit"), (';', "lit fog")]
  }
fogClump2 = fogClump
  { pfreq    = [("shootout", 400), ("empty", 7000)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "Xff"
               , "f;f"
               , ";;f"
               , "XfX"
               ]
  }
smokeClump = PlaceKind
  { psymbol  = 's'
  , pname    = "smoky patch"
  , pfreq    = [("zoo", 100), ("ambush", 50)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";f"
               ]
  , poverride = [ ('f', "smokeClumpOver_f_Lit"), (';', "lit smoke")
                , ('·', "floorActorLit") ]
  }
smokeClump2FGround = smokeClump
  { pfreq    = [("laboratory", 100), ("zoo", 1000), ("ambush", 500)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ ";f;"
               , "f·f"
               , ";·f"
               , ";f;"
               ]
  }
bushClump = PlaceKind
  { psymbol  = 'b'
  , pname    = "bushy patch"
  , pfreq    = [("shootout", 120)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";f"
               ]
  , poverride = [('f', "bushClumpOver_f_Lit"), (';', "bush Lit")]
  }
staircase = PlaceKind
  { psymbol  = '|'
  , pname    = "staircase"
  , pfreq    = [("staircase", 1)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<#>"
               ]
  , poverride = [ ('<', "staircase up"), ('>', "staircase down")
                , ('I', "signboard") ]
  }
staircase2 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#"
               , "···"
               , "<#>"
               , "···"
               , "#·#"
               ]
  }
staircase3 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#···#"
               , "·····"
               , "·<I>·"
               , "·····"
               , "#···#"
               ]
  }
staircase4 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#·#·#·#"
               , "·······"
               , "#·<#>·#"
               , "·······"
               , "#·#·#·#"
               ]
  }
staircase5 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "#·<#>·#"
               ]
  }
staircase6 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "#··<#>··#"
               ]
  }
staircase7 = staircase
  { pfreq    = [("staircase", 100)]
  , pfence   = FGround
  , ptopLeft = [ "#·#·<I>·#·#"
               ]
  }
staircase8 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·····#"
               , "··<#>··"
               , "#·····#"
               ]
  }
staircase9 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#·······#"
               , "·#·<#>·#·"
               , "#·······#"
               ]
  }
staircase10 = staircase
  { pfreq    = [("staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·····#·"
               , "#··<#>··#"
               , "·#·····#·"
               ]
  }
staircase11 = staircase
  { pfreq    = [("staircase", 10000)]
  , pfence   = FGround
  , ptopLeft = [ "··#·#··"
               , "#·····#"
               , "··<#>··"
               , "#·····#"
               , "··#·#··"
               ]
  }
staircase12 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·····"
               , "·<#>·"
               , "·····"
               ]
  }
staircase13 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·······"
               , "#·<#>·#"
               , "·······"
               ]
  }
staircase14 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·········"
               , "·#·<#>·#·"
               , "·········"
               ]
  }
staircase15 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "···········"
               , "#·#·<I>·#·#"
               , "···········"
               ]
  }
staircase16 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#·····#"
               , "··<#>··"
               , "#·····#"
               ]
  }
staircase17 = staircaseLift
  { pfreq    = [("staircase lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·#·····#·"
               , "#··<#>··#"
               , "·#·····#·"
               ]
  }
staircaseOutdoor = staircase
  { pname     = "staircase outdoor"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = [('<', "staircase outdoor up"), ('>', "staircase outdoor down")]
  }
staircaseGated = staircase
  { pname     = "gated staircase"
  , pfreq     = [("gated staircase", 1)]
  , poverride = [('<', "gated staircase up"), ('>', "gated staircase down")]
  }
escapeUp = PlaceKind
  { psymbol  = '<'
  , pname    = "escape airlock up"
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
  , ptopLeft = [ "#·#"
               , "·<·"
               , "#·#"
               ]
  }
escapeUp3 = escapeUp
  { pfreq    = [("escape down", 2000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "#··"
               , "·<·"
               , "#·#"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·#·"
               , "#<#"
               , "·#·"
               ]
  }
escapeUp5 = escapeUp
  { pfreq    = [("escape up", 2000)]
  , pcover   = CMirror
  , pfence   = FWall
  , ptopLeft = [ "#··"
               , "·<·"
               , "··#"
               ]
  }
escapeDown = PlaceKind
  { psymbol  = '>'
  , pname    = "escape airlock down"
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
  , ptopLeft = [ "#·#"
               , "·>·"
               , "#·#"
               ]
  }
escapeDown3 = escapeDown
  { pfreq    = [("escape down", 2000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "#··"
               , "·>·"
               , "#·#"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·#·"
               , "#>#"
               , "·#·"
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [("escape down", 2000)]
  , pcover   = CMirror
  , pfence   = FWall
  , ptopLeft = [ "#··"
               , "·>·"
               , "··#"
               ]
  }
escapeOutdoorDown = escapeDown
  { pfreq     = [("escape outdoor down", 1)]
  , poverride = [('>', "escape outdoor down")]
  }

-- * Allure-specific

staircaseLift = staircase
  { pname    = "lift"
  , pfreq    = [("staircase lift", 1)]
  , poverride = [ ('<', "staircase lift up"), ('>', "staircase lift down")
                , ('I', "signboard") ]
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
colonnadeWide = colonnade
  { prarity  = [(1, 2), (10, 2)]
  , pfence   = FWall
  , ptopLeft = [ "··"
               , "#·"
               ]
  }
oval = PlaceKind
  { psymbol  = 'o'
  , pname    = "oval room"
  , pfreq    = [ ("rogue", 1000), ("arena", 1000), ("empty", 1000)
               , ("zoo", 1000), ("ambush", 1000) ]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "####·"
               , "##···"
               , "#····"
               , "#····"
               , "·····"
               ]
  , poverride = []
  }
ovalFloor = oval  -- Without outer solid fence, visible from outside·
  { pfreq    = [ ("rogue", 10000), ("arena", 10000), ("empty", 10000)
               , ("zoo", 10000), ("ambush", 10000) ]
  , pfence   = FGround
  , ptopLeft = [ "XXXX+#"
               , "XX###·"
               , "X##···"
               , "X#····"
               , "+#····"
               , "#·····"
               ]
  }
ovalSquare = ovalFloor
  { pfreq    = [ ("rogue", 3000), ("arena", 3000), ("empty", 3000)
               , ("zoo", 3000) , ("ambush", 3000) ]
  , ptopLeft = [ "X###+"
               , "##···"
               , "#····"
               , "#····"
               , "+····"
               ]
  }
maze = PlaceKind
  { psymbol  = 'm'
  , pname    = "mysterious maze"
  , pfreq    = [("rogue", 20), ("arena", 20), ("empty", 20)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "·####"
               , "#·#··"
               , "##··#"
               , "#··#·"
               ]
  , poverride = [('&', "cachable")]
  }
maze2 = maze
  { pfreq    = [("rogue", 40), ("arena", 40), ("empty", 40)]
  , ptopLeft = [ "··####"
               , "##·##·"
               , "#·#··#"
               , "#···#·"
               ]
  }
maze3 = maze
  { pfreq    = [("rogue", 70), ("arena", 70), ("empty", 70)]
  , ptopLeft = [ "·######"
               , "·##·##·"
               , "#··#···"
               , "#··#··#"
               ]
  }
mazeBig = maze
  { pfreq    = [("rogue", 500), ("arena", 500), ("empty", 500)]
  , ptopLeft = [ "··####"
               , "·#·#··"
               , "##&·##"
               , "##·##·"
               , "#·#··#"
               ]
  }
mazeBig2 = mazeBig
  { ptopLeft = [ "··####"
               , "##·##·"
               , "#·#&··"
               , "#·#··#"
               , "#···#·"
               ]
  }
mazeBig3 = mazeBig
  { pfreq    = [("rogue", 800), ("arena", 800), ("empty", 800)]
  , ptopLeft = [ "·######"
               , "#·##·#·"
               , "#·#·#··"
               , "##·##·#"
               , "#··#···"
               ]
  }
cells = PlaceKind
  { psymbol  = '#'
  , pname    = "broken holding pens"
  , pfreq    = [ ("rogue", 20), ("arena", 20), (" laboratory", 20)
               , ("empty", 20), ("noise", 20), ("zoo", 200) ]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CReflect
  , pfence   = FWall
  , ptopLeft = [ "··#"
               , "·#·"
               , "#··"
               ]
  , poverride = []
  }
cells2 = cells  -- this one is distinct enough from others
  { pfreq    = [ ("rogue", 50), ("arena", 50), (" laboratory", 50)
               , ("empty", 50), ("noise", 50), ("zoo", 500) ]
  , ptopLeft = [ "··#"
               , "··#"
               , "##·"
               ]
  }
cells3 = cells
  { ptopLeft = [ "#··"
               , "·#·"
               , "··#"
               ]
  }
cells4 = cells
  { ptopLeft = [ "··#"
               , "·##"
               , "#··"
               ]
  }
cells5 = cells
  { ptopLeft = [ "··#"
               , "·#·"
               , "##·"
               ]
  }
cells6 = cells
  { pfreq    = [ ("rogue", 1), ("arena", 2), ("laboratory", 2)
               , ("empty", 2), ("noise", 1), ("zoo", 100) ]
  , ptopLeft = [ "··#"
               , "##·"
               ]
  }
cells7 = cells
  { pfreq    = [ ("rogue", 1), ("arena", 2), (" laboratory", 2)
               , ("empty", 2), ("noise", 1) ]
  , pfence   = FFloor
  , ptopLeft = [ "#·#"
               , "·#·"
               ]
  }

-- * Helper functions

makeStaircaseUp :: PlaceKind -> PlaceKind
makeStaircaseUp s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "up", k)) $ pfreq s
 , poverride = ('>', "stair terminal") : filter ((/= '>') . fst) (poverride s)
 }

makeStaircaseDown :: PlaceKind -> PlaceKind
makeStaircaseDown s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "down", k)) $ pfreq s
 , poverride = ('<', "stair terminal") : filter ((/= '<') . fst) (poverride s)
 }
