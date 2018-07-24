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
  [deadEnd, rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown]
  ++ map makeStaircaseUp lstaircase
  ++ map makeStaircaseDown lstaircase
  -- Allure-specific
  ++ [staircaseLift, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, colonnadeWide, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7]

deadEnd,    rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown :: PlaceKind
-- Allure-specific
staircaseLift, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, colonnadeWide, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7 :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircaseOutdoor, staircaseGated]
  -- Allure-specific
  ++ [staircaseLift]

deadEnd = PlaceKind  -- needs to have index 0
  { psymbol  = 'd'
  , pname    = "a dead end"
  , pfreq    = []
  , prarity  = []
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = ["·"]
  , poverride = []
  }
-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "a room"
  , pfreq    = [("rogue", 100), ("arena", 40), ("laboratory", 40)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["·"]
  , poverride = []
  }
rect2 = rect
  { pname    = "a pen"
  , pfreq    = [("zoo", 10)]
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "a shed"
  , pfreq    = [("empty", 10), ("park", 10)]
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
  , pname    = "a glasshouse"
  , pfreq    = [("shootout", 6)]
  , prarity  = [(1, 10), (10, 8)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "%%"
               , "%·"
               ]
  , poverride = [('%', "glasshouseOver_%_Lit")]
  }
glasshouse2 = glasshouse
  { pname    = "a glass cage"
  , pfreq    = [("zoo", 10)]
  }
glasshouse3 = glasshouse
  { pname    = "a display platform"
  , pfreq    = [("arena", 40), ("ambush", 7)]
  }
pulpit = PlaceKind
  { psymbol  = 'p'
  , pname    = "a stand"
  , pfreq    = [("arena", 10), ("zoo", 20)]
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
  , pname    = "ruins"
  , pfreq    = [("battle", 33)]
  , prarity  = [(1, 10), (10, 20)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["X"]
  , poverride = []
  }
ruin2 = ruin
  { pname    = "a scaffolding"
  , pfreq    = [("noise", 50)]
  }
collapsed = PlaceKind  -- in a dark cave, they have little lights --- that's OK
  { psymbol  = 'c'
  , pname    = "a hardware stack"
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
  , pname    = "a commercial space"
  , pfreq    = [ ("rogue", 500), ("arena", 1000), ("laboratory", 1000)
               , ("empty", 300), ("noise", 1000) ]
  , prarity  = [(1, 5), (10, 5)]
  , pcover   = CStretch
  , pfence   = FWall
  -- Larger rooms require support pillars.
  , ptopLeft = [ "····"
               , "·#··"
               , "····"
               , "····"
               ]
  , poverride = []
  }
pillar2 = pillar
  { ptopLeft = [ "#···"
               , "····"
               , "····"
               , "····"
               ]
  }
pillar3 = pillar
  { ptopLeft = [ "#·#·"
               , "····"
               , "#···"
               , "····"
               ]
  }
pillar4 = pillar
  { pname    = "a bank outlet"
  , prarity  = [(10, 3)]
  , ptopLeft = [ "&·#·"
               , "··#·"
               , "##+·"
               , "····"
               ]
  , poverride = [('&', "cache deposit")]
  }
pillar5 = pillar
  { pname    = "a jewelry store"
  , prarity  = [(10, 3)]
  , ptopLeft = [ "&#··"
               , "#·#·"
               , "·##·"
               , "····"
               ]
  , poverride = [('&', "cache jewelry")]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
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
  , pname    = "a lamp post"
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
  , pname    = "a tree shade"
  , pfreq    = [("brawl", 100)]
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
  , pname    = "a foggy patch"
  , pfreq    = [("park", 100), ("shootout", 150), ("empty", 2000)]
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
  { pfreq    = [("park", 100), ("shootout", 500), ("empty", 5000)]
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
  , pname    = "a smoky patch"
  , pfreq    = [("zoo", 50), ("ambush", 50), ("empty", 1000)]
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
  { pname    = "a burned out area"
  , pfreq    = [ ("laboratory", 100), ("zoo", 500), ("ambush", 500)
               , ("empty", 2000) ]
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
  , pname    = "a bushy patch"
  , pfreq    = [("shootout", 100)]
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
  , pname    = "a staircase"
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
  , ptopLeft = [ "I·#·<#>·#·I"
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
               , "#·I·<#>·I·#"
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
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = [('<', "staircase outdoor up"), ('>', "staircase outdoor down")]
  }
staircaseGated = staircase
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 1)]
  , poverride = [('<', "gated staircase up"), ('>', "gated staircase down")]
  }
escapeUp = PlaceKind
  { psymbol  = '<'
  , pname    = "an escape airlock up"
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
  , pfence   = FWall
  , ptopLeft = [ "#··"
               , "·<·"
               , "#·#"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FFloor
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
  , pname    = "an escape airlock down"
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
  , pfence   = FWall
  , ptopLeft = [ "#··"
               , "·>·"
               , "#·#"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FFloor
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
  { pname    = "a lift"
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
  { pname    = "water pumps"
  , prarity  = [(1, 2), (10, 2)]
  , pfence   = FWall
  , ptopLeft = [ "··"
               , "#·"
               ]
  }
oval = PlaceKind
  { psymbol  = 'o'
  , pname    = "a dome"
  , pfreq    = [ ("rogue", 1000), ("arena", 500), ("empty", 250)
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
  { pfreq    = [ ("rogue", 3000), ("arena", 10000), ("empty", 5000)
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
  { pfreq    = [ ("rogue", 1000), ("arena", 3000), ("empty", 1000)
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
  , pname    = "an intricate maze"
  , pfreq    = [("rogue", 20), ("arena", 40), ("empty", 20)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "##··"
               , "#··#"
               , "··#·"
               ]
  , poverride = [('&', "cache")]
  }
maze2 = maze
  { pfreq    = [("rogue", 25), ("arena", 50), ("empty", 30)]
  , ptopLeft = [ "#·##·"
               , "·#··#"
               , "···#·"
               ]
  }
maze3 = maze
  { pfreq    = [("rogue", 30), ("arena", 60), ("empty", 40)]
  , ptopLeft = [ "##·##·"
               , "#·#··#"
               , "··#···"
               ]
  }
mazeBig = maze
  { pfreq    = [("rogue", 200), ("arena", 400), ("empty", 300)]
  , pfence   = FNone
  , ptopLeft = [ "X####"
               , "#·##·"
               , "##···"
               , "##·+#"
               , "#··#·"
               ]
  }
mazeBig2 = maze
  { pfreq    = [("rogue", 200), ("arena", 500), ("empty", 500)]
  , pfence   = FNone
  , ptopLeft = [ "X#####"
               , "#·##··"
               , "##··&#"
               , "##·##·"
               , "#·#···"
               ]
  }
cells = PlaceKind
  { psymbol  = '#'
  , pname    = "air filters"
  , pfreq    = [ ("rogue", 20), ("arena", 20), (" laboratory", 20)
               , ("empty", 20), ("noise", 20), ("zoo", 200) ]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CReflect
  , pfence   = FWall
  , ptopLeft = [ "··#"
               , "·#·"
               , "#··"
               ]
  , poverride = [('%', "doorlessWallOver_#")]
  }
cells2 = cells
  { pname    = "humidity equalizers"
  , ptopLeft = [ "#··"
               , "·#·"
               , "··#"
               ]
  }
cells3 = cells
  { pname    = "thermostat units"
  , ptopLeft = [ "··#"
               , "·#%"
               , "#··"
               ]
  }
cells4 = cells
  { pname    = "a power node"
  , ptopLeft = [ "··#"
               , "·#·"
               , "#%·"
               ]
  }
cells5 = cells  -- this one is distinct enough from others, so needs a boost
  { pname    = "broken robot holds"
  , pfreq    = [ ("rogue", 50), ("arena", 50), (" laboratory", 50)
               , ("empty", 50), ("noise", 50) ]
  , ptopLeft = [ "··#"
               , "··#"
               , "##·"
               ]
  }
cells6 = cells
  { pname    = "animal holding pens"
  , pfreq    = [ ("rogue", 1), ("arena", 2), ("laboratory", 2)
               , ("empty", 2), ("noise", 1), ("zoo", 10) ]
  , ptopLeft = [ "··#"
               , "##'"
               ]
  }
cells7 = cells
  { pname    = "a defunct control room"
  , pfreq    = [ ("rogue", 1), ("arena", 2), (" laboratory", 2)
               , ("empty", 2), ("noise", 1) ]
  , pfence   = FFloor
  , ptopLeft = [ "#·#"
               , "·%·"
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
