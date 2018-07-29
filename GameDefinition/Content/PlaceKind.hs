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
import Game.LambdaHack.Content.TileKind (TileKind)

content :: [PlaceKind]
content =
  [deadEnd, rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, pillar6, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase, staircase2, staircaseLift, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37, staircaseOutdoor, staircaseOutdoor16, staircaseOutdoor25, staircaseGated, staircaseGated16, staircaseGated25, staircase15up, staircase16down, staircase21up, staircase21down, staircase25up, staircase25down]
  -- Allure-specific
  ++ [staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25, staircaseLift15up, staircaseLift16down, staircaseLift21up, staircaseLift21down, staircaseLift25up, staircaseLift25down]
  -- generated
  ++ map (makeStaircaseUp "stair terminal") (fst stairsAndLifts)
  ++ map (makeStaircaseUp "lift terminal") (snd stairsAndLifts)
  ++ map (makeStaircaseDown "stair terminal") (fst stairsAndLifts)
  ++ map (makeStaircaseDown "lift terminal") (snd stairsAndLifts)
  -- Allure-specific, continued
  ++ [escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, colonnadeWide, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7]

deadEnd,    rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, pillar6, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase, staircase2, staircaseLift, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37, staircaseOutdoor, staircaseOutdoor16, staircaseOutdoor25, staircaseGated, staircaseGated16, staircaseGated25, staircase15up, staircase16down, staircase21up, staircase21down, staircase25up, staircase25down :: PlaceKind
-- Allure-specific
staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25, staircaseLift15up, staircaseLift16down, staircaseLift21up, staircaseLift21down, staircaseLift25up, staircaseLift25down, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, colonnadeWide, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7 :: PlaceKind

lstaircase :: [PlaceKind]
lstaircase = [staircase, staircase2, staircaseLift, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37, staircaseOutdoor, staircaseOutdoor16, staircaseOutdoor25, staircaseGated, staircaseGated16, staircaseGated25]
  -- Allure-specific
  ++ [staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25]

stairsAndLifts :: ([PlaceKind], [PlaceKind])
stairsAndLifts = partition ((/= "a lift") . pname) lstaircase

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
  , pfreq    = [("rogue", 100), ("arena", 35), ("laboratory", 30)]
  , prarity  = [(1, 10), (10, 7)]
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
  , pfreq    = [("escape", 10)]
  , prarity  = [(1, 10), (10, 7)]
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
  , prarity  = [(1, 10), (10, 7)]
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
  , pfreq    = [("arena", 23), ("zoo", 15)]
  , prarity  = [(1, 10), (10, 10)]
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
  , pfreq    = [("noise", 120)]
  }
collapsed = PlaceKind  -- in a dark cave, they have little lights --- that's OK
  { psymbol  = 'c'
  , pname    = "a hardware stack"
  , pfreq    = [("noise", 1)]
  , prarity  = [(1, 11), (10, 11)]
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
  , pfreq    = [ ("rogue", 600), ("arena", 1200), ("laboratory", 1200)
               , ("empty", 300), ("noise", 1200) ]
  , prarity  = [(1, 5), (10, 5)]
  , pcover   = CStretch
  , pfence   = FWall
  -- Larger rooms require support pillars.
  , ptopLeft = [ "····"
               , "·O··"
               , "····"
               , "····"
               ]
  , poverride = []
  }
pillar2 = pillar
  { prarity  = [(1, 10), (10, 10)]
  , ptopLeft = [ "O····"
               , "·····"
               , "·····"
               , "···O·"
               , "·····"
               ]
  }
pillar3 = pillar
  { pfreq    = [ ("rogue", 50), ("arena", 100), ("laboratory", 100)
               , ("empty", 30), ("noise", 300) ]
  , ptopLeft = [ "#··"
               , "···"
               , "···"
               ]
  }
pillar4 = pillar
  { ptopLeft = [ "#·#·"
               , "····"
               , "#···"
               , "····"
               ]
  }
pillar5 = pillar
  { pname    = "a bank outlet"
  , pfreq    = [ ("rogue", 300), ("arena", 1000), ("empty", 300) ]
  , prarity  = [(10, 3)]
  , ptopLeft = [ "&·#·"
               , "··#·"
               , "##+·"
               , "····"
               ]
  , poverride = [('&', "cache deposit")]
  }
pillar6 = pillar
  { pname    = "a jewelry store"
  , pfreq    = [ ("rogue", 300), ("arena", 1000), ("empty", 300) ]
  , prarity  = [(10, 3)]
  , ptopLeft = [ "&#··"
               , "#·%·"
               , "·%%·"
               , "····"
               ]
  , poverride = [('&', "cache jewelry")]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
  , pfreq    = [ ("rogue", 4), ("arena", 9), ("laboratory", 5)
               , ("empty", 12), ("mine", 1200), ("escape", 40) ]
  , prarity  = [(1, 20), (10, 20)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "#·"
               , "·#"
               ]
  , poverride = []
  }
colonnade2 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "#·"
               , "··"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 100), (10, 100)]
  , ptopLeft = [ "··#"
               , "·#·"
               , "#··"
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 50), (10, 50)]
  , ptopLeft = [ "#··"
               , "·#·"
               , "··#"
               ]
  }
colonnade5 = colonnade
  { ptopLeft = [ "#··"
               , "··#"
               ]
  }
colonnade6 = colonnade
  { prarity  = [(1, 12), (10, 12)]
  , ptopLeft = [ "#·"
               , "··"
               , "·#"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "a lamp post"
  , pfreq    = [("escape", 20), ("ambush", 20), ("zoo", 10), ("battle", 10)]
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
  { pfreq    = [("escape", 3000), ("zoo", 50), ("battle", 110)]
  , ptopLeft = [ "XX·XX"
               , "X···X"
               , "··O··"
               , "X···X"
               , "XX·XX"
               ]
  }
lampPost4 = lampPost
  { pfreq    = [("escape", 3000), ("zoo", 50), ("battle", 60)]
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
  , pfreq    = [("escape", 100), ("shootout", 150), ("empty", 1000)]
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
  { pfreq    = [("escape", 100), ("shootout", 500), ("empty", 5000)]
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
  , pfreq    = [("zoo", 50), ("ambush", 50), ("empty", 100)]
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
  , pfreq    = [ ("laboratory", 80), ("zoo", 500), ("ambush", 500)
               , ("empty", 200) ]
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
  { pfreq    = [("escape up", 2000)]
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
staircase = PlaceKind
  { psymbol  = '/'
  , pname    = "a staircase"
  , pfreq    = [ ("open staircase", 1), ("closed staircase", 1)
               , ("walled staircase", 1) ]
  , prarity  = [(1, 1)]  -- no cover when arriving, so low rarity
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<S>"
               ]
  , poverride = [ ('<', "staircase up"), ('>', "staircase down")
                , ('I', "signboard"), ('S', "fillerWall") ]
  }
staircase2 = staircase
  { pfreq    = [ ("open staircase", 3), ("closed staircase", 3)
               , ("walled staircase", 3) ]
  , pfence   = FGround
  , ptopLeft = [ "·<S>·"
               ]
  }
-- Allure-specific:
staircaseLift = PlaceKind
  { psymbol  = '|'
  , pname     = "a lift"
  , pfreq     = [ ("open lift", 1), ("closed lift", 1)
                , ("walled lift", 1) ]
  , prarity  = [(1, 1)]  -- no cover when arriving, so low rarity
  , pcover   = CVerbatim
  , pfence   = FFloor
  , ptopLeft = [ "<S>"
               ]
  , poverride = overrideLift
  }
staircase4 = staircaseLift
  { pfreq    = [ ("open lift", 3), ("closed lift", 3)
               , ("walled lift", 3) ]
  , ptopLeft = [ "·<S>·"
               ]
  }
staircase5 = staircase
  { pfreq    = [("open staircase", 200)]  -- no cover, open
  , pfence   = FGround
  , ptopLeft = [ "#·#"
               , "···"
               , "<S>"
               , "···"
               , "#·#"
               ]
  }
staircase6 = staircaseLift
  { pfreq    = [("open lift", 300)]
  , pfence   = FGround
  , ptopLeft = [ "#·#·#"
               , "·····"
               , "·<S>·"
               , "·····"
               , "#·#·#"
               ]
  }
staircase7 = staircase
  { pfreq    = [("open staircase", 500)]
  , pfence   = FGround
  , ptopLeft = [ "#·#·#·#"
               , "·······"
               , "#·<S>·#"
               , "·······"
               , "#·#·#·#"
               ]
  }
staircase8 = staircaseLift
  { pfreq    = [("open lift", 2000)]
  , pfence   = FGround
  , ptopLeft = [ "·#·#·#·"
               , "#·····#"
               , "··<S>··"
               , "#·····#"
               , "·#·#·#·"
               ]
  }
staircase9 = staircase
  { pfreq    = [("open staircase", 500)]
  , pfence   = FGround
  , ptopLeft = [ "#·······#"
               , "···<S>···"
               , "#·······#"
               ]
  }
staircase10 = staircaseLift
  { pfreq    = [("open lift", 500)]
  , pfence   = FGround
  , ptopLeft = [ "O·····O"
               , "··<S>··"
               , "O·····O"
               ]
  }
staircase11 = staircase
  { pfreq    = [("closed staircase", 2000)]  -- weak cover, low freq
  , pfence   = FFloor
  , ptopLeft = [ "·#·"
               , "#·#"
               , "···"
               , "<S>"
               , "···"
               , "#·#"
               , "·#·"
               ]
  }
staircase12 = staircase
  { pfreq    = [("closed staircase", 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·#·"
               , "#·#·#"
               , "·····"
               , "·<S>·"
               , "·····"
               , "#·#·#"
               , "·#·#·"
               ]
  }
staircase13 = staircase
  { pfreq    = [("closed staircase", 10000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·#·#·"
               , "#·#·#·#"
               , "·······"
               , "#·<S>·#"
               , "·······"
               , "#·#·#·#"
               , "·#·#·#·"
               ]
  }
staircase14 = staircase
  { pfreq    = [("closed staircase", 20000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·#·#"
               , "·#·#·#·"
               , "#·····#"
               , "··<S>··"
               , "#·····#"
               , "·#·#·#·"
               , "#·#·#·#"
               ]
  }
staircase15 = staircase
  { pfreq    = [("closed staircase", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·#·#·#·"
               , "#·#·#·#·#"
               , "·#·····#·"
               , "#··<S>··#"
               , "·#·····#·"
               , "#·#·#·#·#"
               , "·#·#·#·#·"
               ]
  }
staircase16 = staircase
  { pfreq    = [("closed staircase", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·#·#·#"
               , "·#·#·#·#·"
               , "#·······#"
               , "·#·<S>·#·"
               , "#·······#"
               , "·#·#·#·#·"
               , "#·#·#·#·#"
               ]
  }
staircase17 = staircase
  { pfreq    = [("closed staircase", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·#·#·#·#"
               , "·#·#·#·#·#·"
               , "#·#·····#·#"
               , "·#··<S>··#·"
               , "#·#·····#·#"
               , "·#·#·#·#·#·"
               , "#·#·#·#·#·#"
               ]
  }
staircase18 = staircase
  { pfreq    = [("closed staircase", 500000)]
  , pfence   = FFloor
  , ptopLeft = [ "··#·#·#·#··"
               , "·#·#·#·#·#·"
               , "#·#·····#·#"
               , "·#··<S>··#·"
               , "#·#·····#·#"
               , "·#·#·#·#·#·"
               , "··#·#·#·#··"
               ]
  }
staircase19 = staircase
  { pfreq    = [("closed staircase", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·#·#·#·#·"
               , "#·#·#·#·#·#"
               , "·#·······#·"
               , "#·#·<S>·#·#"
               , "·#·······#·"
               , "#·#·#·#·#·#"
               , "·#·#·#·#·#·"
               ]
  }
staircase20 = staircase
  { pfreq    = [("closed staircase", 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·#·O·#·#·"
               , "#·#·····#·#"
               , "·#··<S>··#·"
               , "#·#·····#·#"
               , "·#·#·I·#·#·"
               ]
  }
staircase21 = staircase
  { pfreq    = [("closed staircase", 5000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·I·#·#"
               , "·#·····#·"
               , "#··<S>··#"
               , "·#·····#·"
               , "#·#·O·#·#"
               ]
  }
staircase22 = staircase
  { pfreq    = [("closed staircase", 2000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·····#·#"
               , "·#··<S>··#·"
               , "#·#·····#·#"
               ]
  }
staircase23 = staircase
  { pfreq    = [("closed staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·······#·"
               , "#·#·<S>·#·#"
               , "·#·······#·"
               ]
  }
staircase24 = staircase
  { pfreq    = [("closed staircase", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·····#·"
               , "#··<S>··#"
               , "·#·····#·"
               ]
  }
staircase25 = staircase
  { pfreq    = [("walled staircase", 100)]
  , pfence   = FWall
  , ptopLeft = [ "·····"
               , "·<S>·"
               , "·····"
               ]
  }
staircase26 = staircase
  { pfreq    = [("walled staircase", 200)]
  , pfence   = FWall
  , ptopLeft = [ "·······"
               , "··<S>··"
               , "·······"
               ]
  }
staircase27 = staircaseLift
  { pfreq    = [("walled lift", 500)]
  , pfence   = FWall
  , ptopLeft = [ "#·····#"
               , "··<S>··"
               , "#·····#"
               ]
  }
staircase28 = staircaseLift
  { pfreq    = [("walled lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·····"
               , "·····"
               , "·<S>·"
               , "·····"
               , "·····"
               ]
  }
staircase29 = staircase
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#···#"
               , "·····"
               , "·<S>·"
               , "·····"
               , "#···#"
               ]
  }
staircase30 = staircaseLift
  { pfreq    = [("walled lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "O···O"
               , "·····"
               , "·<S>·"
               , "·····"
               , "O···O"
               ]
  }
staircase31 = staircase
  { pfreq    = [("walled staircase", 2000)]
  , pfence   = FWall
  , ptopLeft = [ "·······"
               , "·······"
               , "··<S>··"
               , "·······"
               , "·······"
               ]
  }
staircase32 = staircaseLift
  { pfreq    = [("walled lift", 5000)]
  , pfence   = FWall
  , ptopLeft = [ "#·····#"
               , "·······"
               , "··<S>··"
               , "·······"
               , "#·····#"
               ]
  }
staircase33 = staircase
  { pfreq    = [("walled staircase", 5000)]
  , pfence   = FWall
  , ptopLeft = [ "#·#·#·#"
               , "·······"
               , "#·<S>·#"
               , "·······"
               , "#·#·#·#"
               ]
  }
staircase34 = staircaseLift
  { pfreq    = [("walled lift", 5000)]
  , pfence   = FWall
  , ptopLeft = [ "·#·#·#·"
               , "#·····#"
               , "··<S>··"
               , "#·····#"
               , "·#·#·#·"
               ]
  }
staircase35 = staircase
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·········"
               , "···<S>···"
               , "·········"
               ]
  }
staircase36 = staircaseLift
  { pfreq    = [("walled lift", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "·#·····#·"
               , "#··<S>··#"
               , "·#·····#·"
               ]
  }
staircase37 = staircase
  { pfreq    = [("walled staircase", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#·······#"
               , "·#·<S>·#·"
               , "#·······#"
               ]
  }
overrideOutdoor :: [(Char, GroupName TileKind)]
overrideOutdoor =
  [ ('<', "staircase outdoor up"), ('>', "staircase outdoor down")
  , ('I', "signboard"), ('S', "fillerWall") ]
staircaseOutdoor = staircase
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 1)]
  , poverride = overrideOutdoor
  }
staircaseOutdoor16 = staircase16
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 10000)]
  , poverride = overrideOutdoor
  }
staircaseOutdoor25 = staircase25
  { pname     = "an outdoor area exit"
  , pfreq     = [("staircase outdoor", 1000)]
  , poverride = overrideOutdoor
  }
overrideGated :: [(Char, GroupName TileKind)]
overrideGated =
  [ ('<', "gated staircase up"), ('>', "gated staircase down")
  , ('I', "signboard"), ('S', "fillerWall") ]
staircaseGated = staircase
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 1)]
  , poverride = overrideGated
  }
staircaseGated16 = staircase16
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 10000)]
  , poverride = overrideGated
  }
staircaseGated25 = staircase25
  { pname     = "a gated staircase"
  , pfreq     = [("gated staircase", 1000)]
  , poverride = overrideGated
  }
staircase15up = staircase
  { pname    = "a staircase up"
  , pfreq    = [("closed staircase up", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·#·#·"
               , "#·#·#·#"
               , "·#···#·"
               , "#··<··#"
               , "·#···#·"
               , "#·#·#·#"
               , "·#·#·#·"
               ]
  }
staircase16down = staircase
  { pname    = "a staircase down"
  , pfreq    = [("closed staircase down", 100000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·#·#"
               , "·#·#·#·"
               , "#·····#"
               , "·#·>·#·"
               , "#·····#"
               , "·#·#·#·"
               , "#·#·#·#"
               ]
  }
staircase21up = staircase
  { pname    = "a staircase up"
  , pfreq    = [("closed staircase up", 10000), ("open staircase up", 10000)]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·#·#"
               , "·#···#·"
               , "#··<··#"
               , "·#···#·"
               , "#·#·#·#"
               ]
  }
staircase21down = staircase
  { pname    = "a staircase down"
  , pfreq    = [ ("closed staircase down", 10000)
               , ("open staircase down", 10000) ]
  , pfence   = FFloor
  , ptopLeft = [ "#·#·#·#"
               , "·#···#·"
               , "#··>··#"
               , "·#···#·"
               , "#·#·#·#"
               ]
  }
staircase25up = staircase
  { pname    = "a staircase up"
  , pfreq    = [("walled staircase up", 100)]
  , pfence   = FWall
  , ptopLeft = [ "···"
               , "·<·"
               , "···"
               ]
  }
staircase25down = staircase
  { pname    = "a staircase down"
  , pfreq    = [("walled staircase down", 100)]
  , pfence   = FWall
  , ptopLeft = [ "···"
               , "·>·"
               , "···"
               ]
  }

-- * Allure-specific

overrideLift :: [(Char, GroupName TileKind)]
overrideLift = [ ('<', "staircase lift up"), ('>', "staircase lift down")
               , ('I', "signboard"), ('S', "lift shaft") ]

staircaseLift11 = staircase11
  { pname     = "a lift"
  , pfreq     = [("closed lift", 2000)]  -- weak cover, low freq
  , poverride = overrideLift
  }
staircaseLift12 = staircase12
  { pname     = "a lift"
  , pfreq     = [("closed lift", 5000)]
  }
staircaseLift13 = staircase13
  { pname     = "a lift"
  , pfreq     = [("closed lift", 10000)]
  , poverride = overrideLift
  }
staircaseLift14 = staircase14
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverride = overrideLift
  }
staircaseLift15 = staircase15
  { pname     = "a lift"
  , pfreq     = [("closed lift", 100000)]
  , poverride = overrideLift
  }
staircaseLift16 = staircase16
  { pname     = "a lift"
  , pfreq     = [("closed lift", 100000)]
  , poverride = overrideLift
  }
staircaseLift17 = staircase17
  { pname     = "a lift"
  , pfreq     = [("closed lift", 100000)]
  , poverride = overrideLift
  }
staircaseLift18 = staircase18
  { pname     = "a lift"
  , pfreq     = [("closed lift", 500000)]
  , poverride = overrideLift
  }
staircaseLift19 = staircase19
  { pname     = "a lift"
  , pfreq     = [("closed lift", 100000)]
  , poverride = overrideLift
  }
staircaseLift20 = staircase20
  { pname     = "a lift"
  , pfreq     = [("closed lift", 5000)]
  , poverride = overrideLift
  }
staircaseLift21 = staircase21
  { pname     = "a lift"
  , pfreq     = [("closed lift", 5000)]
  , poverride = overrideLift
  }
staircaseLift22 = staircase22
  { pname     = "a lift"
  , pfreq     = [("closed lift", 2000)]
  , poverride = overrideLift
  }
staircaseLift23 = staircase23
  { pname     = "a lift"
  , poverride = overrideLift
  , pfreq     = [("closed lift", 1000)]
  }
staircaseLift24 = staircase24
  { pname     = "a lift"
  , pfreq     = [("closed lift", 1000)]
  , poverride = overrideLift
  }
staircaseLift25 = staircase25
  { pname     = "a lift"
  , pfreq     = [("walled lift", 100)]
  , poverride = overrideLift
  }
staircaseLift15up = staircaseLift
  { pname    = "a lift up"
  , pfreq    = [("closed lift up", 200000)]
  , ptopLeft = [ "·#·##·#·"
               , "#·#··#·#"
               , "·#····#·"
               , "#··<I··#"
               , "·#····#·"
               , "#·#··#·#"
               , "·#·##·#·"
               ]
  }
staircaseLift16down = staircaseLift
  { pname    = "a lift down"
  , pfreq    = [("closed lift down", 200000)]
  , ptopLeft = [ "#·#··#·#"
               , "·#·##·#·"
               , "#······#"
               , "·#·I>·#·"
               , "#······#"
               , "·#·##·#·"
               , "#·#··#·#"
               ]
 }
staircaseLift21up = staircaseLift
  { pname    = "a lift up"
  , pfreq    = [("closed lift up", 20000), ("open lift up", 20000)]
  , ptopLeft = [ "#·#··#·#"
               , "·#····#·"
               , "#··<S··#"
               , "·#····#·"
               , "#·#··#·#"
               ]
  }
staircaseLift21down = staircaseLift
  { pname    = "a lift down"
  , pfreq    = [("closed lift down", 20000), ("open lift down", 20000)]
  , ptopLeft = [ "·#·##·#·"
               , "#······#"
               , "·#·S>·#·"
               , "#······#"
               , "·#·##·#·"
               ]
  }
staircaseLift25up = staircaseLift
  { pname    = "a lift up"
  , pfreq    = [("walled lift up", 200)]
  , pfence   = FWall
  , ptopLeft = [ "····"
               , "·<S·"
               , "····"
               ]
  }
staircaseLift25down = staircaseLift
  { pname    = "a lift down"
  , pfreq    = [("walled lift down", 200)]
  , pfence   = FWall
  , ptopLeft = [ "····"
               , "·S>·"
               , "····"
               ]
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
  , prarity  = [(1, 10), (10, 10)]
  , pfence   = FWall
  , ptopLeft = [ "··"
               , "%·"
               ]
  , poverride = [('%', "doorlessWallOver_#")]
  }
oval = PlaceKind
  { psymbol  = 'o'
  , pname    = "a dome"
  , pfreq    = [ ("rogue", 2000), ("arena", 1000), ("empty", 500)
               , ("zoo", 2000), ("ambush", 2000) ]
  , prarity  = [(1, 13), (10, 13)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "####·"
               , "##···"
               , "#··tt"
               , "#·t··"
               , "··t··"
               ]
  , poverride = [('t', "trailLit"), ('a', "alarmingTrailLit")]
  }
ovalFloor = oval  -- Without outer solid fence, visible from outside·
  { pfreq    = [ ("rogue", 3000), ("arena", 10000), ("empty", 5000)
               , ("zoo", 10000), ("ambush", 10000) ]
  , pfence   = FGround
  , ptopLeft = [ "XXXX+#"
               , "XX###·"
               , "X##···"
               , "X#···a"
               , "+#··a·"
               , "#··a··"
               ]
  }
ovalSquare = oval
  { pfence   = FGround
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
  , pfreq    = [("rogue", 10), ("arena", 20), ("empty", 10)]
  , prarity  = [(1, 12), (10, 12)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "##··"
               , "#··#"
               , "··#·"
               ]
  , poverride = [('&', "cache")]
  }
maze2 = maze
  { pfreq    = [("rogue", 20), ("arena", 40), ("empty", 20)]
  , ptopLeft = [ "#·##·"
               , "·#··#"
               , "···#·"
               ]
  }
maze3 = maze
  { pfreq    = [("rogue", 30), ("arena", 60), ("empty", 30)]
  , ptopLeft = [ "##·##·"
               , "#·#··#"
               , "··#···"
               ]
  }
mazeBig = maze
  { pfreq    = [("rogue", 50), ("arena", 150), ("empty", 150)]
  , pfence   = FNone
  , ptopLeft = [ "X####"
               , "#·##·"
               , "##···"
               , "##·+#"
               , "#··#·"
               ]
  }
mazeBig2 = maze
  { pfreq    = [("rogue", 50), ("arena", 150), ("empty", 150)]
  , prarity  = [(1, 10), (10, 20)]
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
  , pfreq    = [ ("rogue", 12), ("arena", 12), (" laboratory", 12)
               , ("empty", 12), ("noise", 12), ("zoo", 120) ]
  , prarity  = [(1, 4), (10, 4)]
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
               , "·#^"
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
  , pfreq    = [ ("rogue", 15), ("arena", 20), (" laboratory", 20)
               , ("empty", 20), ("noise", 20) ]
  , ptopLeft = [ "··#"
               , "··#"
               , "##·"
               ]
  }
cells6 = cells
  { pname    = "animal holding pens"
  , pfreq    = [ ("rogue", 3), ("arena", 5), ("laboratory", 5)
               , ("empty", 5), ("noise", 3), ("zoo", 30) ]
  , ptopLeft = [ "··#"
               , "##'"
               ]
  }
cells7 = cells
  { pname    = "a defunct control room"
  , pfreq    = [ ("rogue", 3), ("arena", 5), (" laboratory", 5)
               , ("empty", 5), ("noise", 3) ]
  , pfence   = FFloor
  , ptopLeft = [ "#·#"
               , "·%·"
               ]
  }

-- * Helper functions

makeStaircaseUp :: GroupName TileKind -> PlaceKind -> PlaceKind
makeStaircaseUp terminal s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "up", k)) $ pfreq s
 , poverride = ('>', terminal) : filter ((/= '>') . fst) (poverride s)
 }

makeStaircaseDown :: GroupName TileKind -> PlaceKind -> PlaceKind
makeStaircaseDown terminal s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ tshow t <+> "down", k)) $ pfreq s
 , poverride = ('<', terminal) : filter ((/= '<') . fst) (poverride s)
 }
