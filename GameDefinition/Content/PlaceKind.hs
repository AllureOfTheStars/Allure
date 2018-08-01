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

import qualified Data.Text as T

import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.PlaceKind
import Game.LambdaHack.Content.TileKind (TileKind)

content :: [PlaceKind]
content =
  [deadEnd, rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, pillar6, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]
  -- Allure-specific
  ++ [staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25]
  -- automatically generated
  ++ generatedStairs
  -- Allure-specific, continued
  ++ [escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, pumps, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7]

deadEnd,    rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, pillar6, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2FGround, bushClump, escapeUp, escapeUp2, escapeUp3, escapeUp4, escapeUp5, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeOutdoorDown, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37 :: PlaceKind
-- Allure-specific
staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25, escapeSpaceshipDown, escapeSpaceshipDown2, escapeSpaceshipDown3, escapeSpaceshipDown4, escapeSpaceshipDown5, pumps, oval, ovalFloor, ovalSquare, maze,  maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7 :: PlaceKind

staircase, staircaseLift :: PlaceKind  -- templates

staircaseBasic :: [PlaceKind]
staircaseBasic = [staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]
  -- Allure-specific
  ++ [staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25]

generatedStairs :: [PlaceKind]
generatedStairs =
  let (stairs, lifts) = partition ((/= "a lift") . pname) staircaseBasic
      gatedStairs = map makeGatedStaircase stairs
      gatedLifts = map makeGatedLift lifts
      outdoorStairs = map makeOutdoor stairs
      stairsAll = stairs ++ gatedStairs ++ outdoorStairs
      liftsAll = lifts ++ gatedLifts
  in gatedStairs ++ gatedLifts ++ outdoorStairs
     ++ map (makeStaircaseUp "stair terminal") stairsAll
     ++ map (makeStaircaseUp "lift terminal") liftsAll
     ++ map (makeStaircaseDown "stair terminal") stairsAll
     ++ map (makeStaircaseDown "lift terminal") liftsAll

-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
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
  , pfreq    = [("shootout", 3)]
  , prarity  = [(1, 10), (10, 7)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "%%"
               , "%·"
               ]
  , poverride = []
  }
glasshouse2 = glasshouse
  { pname    = "a glass cage"
  , pfreq    = [("zoo", 10)]
  }
glasshouse3 = glasshouse
  { pname    = "an entertainment center"
  , pfreq    = [("arena", 40), ("ambush", 3)]
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
  , poverride = [('O', "pulpit")]
      -- except for floor, this will all be lit, regardless of night/dark; OK
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruins"
  , pfreq    = [("battle", 33), ("ambush", 5)]
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
  , pname    = "a market"
  , pfreq    = [ ("rogue", 600), ("arena", 1200)
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
  { prarity  = [(1, 15), (10, 15)]
  , ptopLeft = [ "O····"
               , "·····"
               , "·····"
               , "···O·"
               , "·····"
               ]
  }
pillar3 = pillar
  { pname    = "a plaza"
  , pfreq    = [ ("rogue", 50), ("arena", 100), ("laboratory", 100)
               , ("empty", 30), ("noise", 300) ]
  , ptopLeft = [ "#··"
               , "···"
               , "···"
               ]
  }
pillar4 = pillar
  { pname    = "a mall"
  , pfreq    = [ ("rogue", 300), ("arena", 600)
               , ("empty", 300), ("noise", 1200) ]
  , ptopLeft = [ "#·#·"
               , "····"
               , "#···"
               , "····"
               ]
  }
pillar5 = pillar
  { pname    = "a bank outlet"
  , pfreq    = [ ("rogue", 300), ("arena", 1000), ("empty", 300) ]
  , prarity  = [(10, 3)]
  , ptopLeft = [ "&·%·"
               , "··#·"
               , "%#+·"
               , "····"
               ]
  , poverride = [('&', "cache deposit"), ('+', "trapped door")]
  }
pillar6 = pillar
  { pname    = "a jewelry store"
  , pfreq    = [ ("rogue", 300), ("arena", 1000), ("empty", 300) ]
  , prarity  = [(10, 3)]
  , ptopLeft = [ "&#··"
               , "#·%·"
               , "·%h·"
               , "····"
               ]
  , poverride = [('&', "cache jewelry"), ('h', "hardware rack")]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
  , pfreq    = [ ("rogue", 4), ("arena", 9), ("empty", 12), ("mine", 1200)
               , ("escape", 40) ]
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
  , pname    = "an escape up"
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
  , pfence   = FWall
  , ptopLeft = [ "·#·"
               , "#<#"
               , "·#·"
               ]
  }
escapeUp4 = escapeUp
  { pfreq    = [("escape up", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·^·"
               , "^<^"
               , "·^·"
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
  , pname    = "an escape down"
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
  , pfence   = FWall
  , ptopLeft = [ "·#·"
               , "#>#"
               , "·#·"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·^·"
               , "^>^"
               , "·^·"
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
  , pfreq    = [("tiny staircase", 1)]  -- no cover when arriving; low freq
  , prarity  = [(1, 100)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<S>"
               ]
  , poverride = [ ('<', "staircase up"), ('>', "staircase down")
                , ('I', "signboard"), ('S', "fillerWall") ]
  }
staircase1 = staircase
  { prarity  = [(1, 1)]  -- no cover when arriving; so low rarity
  }
staircase2 = staircase
  { pfreq    = [("tiny staircase", 3)]
  , prarity  = [(1, 1)]
  , pfence   = FGround
  , ptopLeft = [ "·<S>·"
               ]
  }
-- Allure-specific:
staircaseLift = PlaceKind
  { psymbol  = '|'
  , pname    = "a lift"
  , pfreq    = [("tiny lift", 1)]
  , prarity  = [(1, 100)]
  , pcover   = CVerbatim
  , pfence   = FFloor
  , ptopLeft = [ "<S>"
               ]
  , poverride = overrideLift
  }
staircase3 = staircaseLift
  { prarity  = [(1, 1)]
  }
staircase4 = staircaseLift
  { pfreq    = [("tiny lift", 3)]
  , prarity  = [(1, 1)]
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
  { pfreq    = [("closed staircase", 4000)]
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
  { pfreq    = [("closed staircase", 6000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·#·#·"
               , "#·#·#·#"
               , "·······"
               , "O·<S>·O"
               , "·······"
               , "#·#·#·#"
               , "·#·#·#·"
               ]
  }
staircase14 = staircase
  { pfreq    = [("closed staircase", 10000)]
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
  { pfreq    = [("closed staircase", 20000)]
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
  { pfreq    = [("closed staircase", 20000)]
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
  { pfreq    = [("closed staircase", 20000)]
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
  { pfreq    = [("closed staircase", 80000)]
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
  { pfreq    = [("closed staircase", 20000)]
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
  , ptopLeft = [ "#···#"
               , "·····"
               , "·<S>·"
               , "·····"
               , "#···#"
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
  , ptopLeft = [ "·········"
               , "·O·<S>·O·"
               , "·········"
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
  , pfreq     = [("closed lift", 4000)]
  , poverride = overrideLift
  }
staircaseLift13 = staircase13
  { pname     = "a lift"
  , pfreq     = [("closed lift", 6000)]
  , poverride = overrideLift
  }
staircaseLift14 = staircase14
  { pname     = "a lift"
  , pfreq     = [("closed lift", 10000)]
  , poverride = overrideLift
  }
staircaseLift15 = staircase15
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverride = overrideLift
  }
staircaseLift16 = staircase16
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverride = overrideLift
  }
staircaseLift17 = staircase17
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverride = overrideLift
  }
staircaseLift18 = staircase18
  { pname     = "a lift"
  , pfreq     = [("closed lift", 80000)]
  , poverride = overrideLift
  }
staircaseLift19 = staircase19
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
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
  , pfreq     = [("closed lift", 1000)]
  , poverride = overrideLift
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
pumps = PlaceKind
  { psymbol  = 'w'
  , pname    = "water pumps"
  , pfreq    = [ ("rogue", 4), ("laboratory", 5), ("empty", 12), ("mine", 1200)
               , ("shootout", 3), ("escape", 40) ]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CAlternate
  , pfence   = FWall
  , ptopLeft = [ "·f"
               , "%·"
               ]
  , poverride = [('%', "doorlessWallOver_#"), ('f', "bushClumpOver_f_Lit")]
  }
oval = PlaceKind
  { psymbol  = 'o'
  , pname    = "a dome"
  , pfreq    = [ ("rogue", 2000), ("arena", 1000), ("laboratory", 1200)
               , ("empty", 500), ("zoo", 2000), ("ambush", 2000) ]
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
  { pfreq    = [ ("rogue", 3000), ("arena", 10000), ("laboratory", 1200)
               , ("empty", 5000), ("zoo", 10000), ("ambush", 10000) ]
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
  , poverride = [('&', "cache"), ('+', "trapped door"), ('i', "floorActorItem")]
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
               , "··%···"
               ]
  }
mazeBig = maze
  { pfreq    = [("rogue", 50), ("arena", 150), ("empty", 150)]
  , pfence   = FNone
  , ptopLeft = [ "X####"
               , "#·##·"
               , "##···"
               , "##·+%"
               , "#··%i"
               ]
  }
mazeBig2 = maze
  { pfreq    = [("rogue", 50), ("arena", 150), ("empty", 150)]
  , prarity  = [(1, 10), (10, 20)]
  , pfence   = FNone
  , ptopLeft = [ "X#####"
               , "#·##··"
               , "##···#"
               , "##··&·"
               , "#··#··"
               ]
  }
cells = PlaceKind
  { psymbol  = '#'
  , pname    = "air filters"
  , pfreq    = [ ("rogue", 12), (" laboratory", 12), ("empty", 12)
               , ("noise", 12), ("zoo", 120), ("shootout", 3), ("ambush", 4) ]
  , prarity  = [(1, 4), (10, 4)]
  , pcover   = CReflect
  , pfence   = FWall
  , ptopLeft = [ "··#"
               , "·#·"
               , "#··"
               ]
  , poverride = [('%', "doorlessWallOver_#"), ('f', "bushClumpOver_f_Lit")]
  }
cells2 = cells
  { pname    = "humidity equalizers"
  , ptopLeft = [ "#··"
               , "·#·"
               , "f·#"
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
  , pfreq    = [ ("rogue", 15), (" laboratory", 20), ("empty", 20)
               , ("noise", 20) ]
  , ptopLeft = [ "··#"
               , "··#"
               , "##·"
               ]
  }
cells6 = cells
  { pname    = "animal holding pens"
  , pfreq    = [("laboratory", 20), ("empty", 20), ("noise", 20), ("zoo", 30)]
  , ptopLeft = [ "··#"
               , "##'"
               ]
  }
cells7 = cells
  { pname    = "a defunct control room"
  , pfreq    = [ ("rogue", 5), (" laboratory", 10), ("empty", 8)
               , ("noise", 5) ]
  , pfence   = FFloor
  , ptopLeft = [ "#··"
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

overrideGatedStaircase :: [(Char, GroupName TileKind)]
overrideGatedStaircase =
  [ ('<', "gated staircase up"), ('>', "gated staircase down")
  , ('I', "signboard"), ('S', "fillerWall") ]

makeGatedStaircase :: PlaceKind -> PlaceKind
makeGatedStaircase s = s
 { psymbol   = 'g'
 , pname     = T.unwords $ "a gated" : tail (T.words (pname s))
 , pfreq     = map (first (\t -> toGroupName $ "gated" <+> tshow t)) $ pfreq s
 , poverride = overrideGatedStaircase
 }

overrideGatedLift :: [(Char, GroupName TileKind)]
overrideGatedLift =
  [ ('<', "gated lift up"), ('>', "gated lift down")
  , ('I', "signboard"), ('S', "lift shaft") ]

makeGatedLift :: PlaceKind -> PlaceKind
makeGatedLift s = s
 { psymbol   = 'g'
 , pname     = T.unwords $ "a gated" : tail (T.words (pname s))
 , pfreq     = map (first (\t -> toGroupName $ "gated" <+> tshow t)) $ pfreq s
 , poverride = overrideGatedLift
 }

overrideOutdoor :: [(Char, GroupName TileKind)]
overrideOutdoor =
  [ ('<', "staircase outdoor up"), ('>', "staircase outdoor down")
  , ('I', "signboard"), ('S', "fillerWall") ]

makeOutdoor :: PlaceKind -> PlaceKind
makeOutdoor s = s
 { psymbol   = 'o'
 , pname     = "an outdoor area exit"
 , pfreq     = map (first (\t -> toGroupName $ "outdoor" <+> tshow t)) $ pfreq s
 , poverride = overrideOutdoor
 }
