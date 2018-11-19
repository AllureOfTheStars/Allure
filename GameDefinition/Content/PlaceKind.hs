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
  [deadEnd, rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, glasshouse4, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, pillar6, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, colonnade7, colonnade8, colonnade9, colonnade10, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2, smokeClump3FGround, bushClump, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeDown6, escapeDown7, escapeDown8, escapeDown9, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]
  -- Allure-specific
  ++ [staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25]
  -- automatically generated
  ++ generatedStairs ++ generatedEscapes
  -- Allure-specific, continued
  ++ [ pumps, oval, ovalFloor, ovalSquare, ovalBasin, ovalBasin2, squareBasin, squareBasin2, floodedRoom, maze, maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7, tank, tank2, tank3, tank4, tank5, shuttleHusk, shuttleHusk2, shuttleHusk3, shuttleHusk4, shuttleHusk5, shuttleHusk6]

deadEnd,    rect, rect2, rectWindows, glasshouse, glasshouse2, glasshouse3, glasshouse4, pulpit, ruin, ruin2, collapsed, collapsed2, collapsed3, collapsed4, collapsed5, collapsed6, collapsed7, pillar, pillar2, pillar3, pillar4, pillar5, pillar6, colonnade, colonnade2, colonnade3, colonnade4, colonnade5, colonnade6, colonnade7, colonnade8, colonnade9, colonnade10, lampPost, lampPost2, lampPost3, lampPost4, treeShade, fogClump, fogClump2, smokeClump, smokeClump2, smokeClump3FGround, bushClump, escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeDown6, escapeDown7, escapeDown8, escapeDown9, staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37 :: PlaceKind
-- Allure-specific
staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25, pumps, oval, ovalFloor, ovalSquare, ovalBasin, ovalBasin2, squareBasin, squareBasin2, floodedRoom, maze, maze2, maze3, mazeBig, mazeBig2, cells, cells2, cells3, cells4, cells5, cells6, cells7, tank, tank2, tank3, tank4, tank5, shuttleHusk, shuttleHusk2, shuttleHusk3, shuttleHusk4, shuttleHusk5, shuttleHusk6 :: PlaceKind

staircase, staircaseLift :: PlaceKind  -- templates

staircaseBasic :: [PlaceKind]
staircaseBasic = [staircase1, staircase2, staircase3, staircase4, staircase5, staircase6, staircase7, staircase8, staircase9, staircase10, staircase11, staircase12, staircase13, staircase14, staircase15, staircase16, staircase17, staircase18, staircase19, staircase20, staircase21, staircase22, staircase23, staircase24, staircase25, staircase26, staircase27, staircase28, staircase29, staircase30, staircase31, staircase32, staircase33, staircase34, staircase35, staircase36, staircase37]
  -- Allure-specific
  ++ [staircaseLift11, staircaseLift12, staircaseLift13, staircaseLift14, staircaseLift15, staircaseLift16, staircaseLift17, staircaseLift18, staircaseLift19, staircaseLift20, staircaseLift21, staircaseLift22, staircaseLift23, staircaseLift24, staircaseLift25]

generatedStairs :: [PlaceKind]
generatedStairs =
  let (stairs, lifts) = partition ((/= "a lift") . pname) staircaseBasic
      gatedStairs = map switchStaircaseToGated stairs
      gatedLifts = map switchLiftToGated lifts
      weldedStairs = map switchStaircaseToWelded stairs
      weldedLifts = map switchLiftToWelded lifts
      outdoorStairs = map switchStaircaseToOutdoor stairs
      stairsAll = stairs ++ gatedStairs ++ weldedStairs ++ outdoorStairs
      liftsAll = lifts ++ gatedLifts ++ weldedLifts
  in gatedStairs ++ gatedLifts
     ++ weldedStairs ++ weldedLifts
     ++ outdoorStairs
     ++ map (switchExitToUp "stair terminal") stairsAll
     ++ map (switchExitToUp "lift terminal") liftsAll
     ++ map (switchExitToDown "stair terminal") stairsAll
     ++ map (switchExitToDown "lift terminal") liftsAll

escapeDownBasic :: [PlaceKind]
escapeDownBasic =
  [ escapeDown, escapeDown2, escapeDown3, escapeDown4, escapeDown5, escapeDown6
  , escapeDown7, escapeDown8, escapeDown9 ]

generatedEscapes :: [PlaceKind]
generatedEscapes =
  let upEscapes = map switchEscapeToUp escapeDownBasic
      outdoorEscapes = map switchEscapeToOutdoorDown escapeDownBasic
      spaceshipEscapes = map switchEscapeToSpaceshipDown escapeDownBasic
  in upEscapes ++ outdoorEscapes ++ spaceshipEscapes

-- The dots below are @Char.chr 183@, as defined in @TileKind.floorSymbol@.
deadEnd = PlaceKind  -- needs to have index 0
  { psymbol  = 'd'
  , pname    = "a dead end"
  , pfreq    = []
  , prarity  = []
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = ["·"]
  , poverrideDark = []
  , poverrideLit = []
  }
rect = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'r'
  , pname    = "a room"
  , pfreq    = [("rogue", 100), ("laboratory", 10)]
  , prarity  = [(1, 10), (10, 6)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["·"]
  , poverrideDark = []
  , poverrideLit = []
  }
rect2 = rect
  { pname    = "a pen"
  , pfreq    = [("shootout", 1), ("zoo", 10)]
  }
rectWindows = PlaceKind
  { psymbol  = 'w'
  , pname    = "a shed"
  , pfreq    = [("brawl", 12), ("escape", 20)]
  , prarity  = [(1, 10), (10, 10)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#%"
               , "%·"
               ]
  , poverrideDark = [('%', "rectWindowsOver_%")]
  , poverrideLit = [('%', "rectWindowsOver_%")]
  }
glasshouse = PlaceKind
  { psymbol  = 'g'
  , pname    = "a glasshouse"
  , pfreq    = [("shootout", 8)]
  , prarity  = [(1, 10), (10, 7)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "%%"
               , "%·"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
glasshouse2 = glasshouse
  { pname    = "a glass cage"
  , pfreq    = [("laboratory", 2), ("zoo", 30)]
  }
glasshouse3 = glasshouse
  { pname    = "an entertainment center"
  , pfreq    = [("arena", 1), ("ambush", 10)]
  }
glasshouse4 = glasshouse
  { pname    = "an exhibition area"
  , pfreq    = [("arena", 1), ("museum", 1)]
  }
pulpit = PlaceKind
  { psymbol  = 'p'
  , pname    = "a stand podium"
  , pfreq    = [("arena", 15), ("museum", 15), ("zoo", 80)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ "%%·"
               , "%··"
               , "··O"
               ]
  , poverrideDark = [('O', "pulpit")]
  , poverrideLit = [('O', "pulpit")]
      -- except for floor, this will all be lit, regardless of night/dark; OK
  }
ruin = PlaceKind
  { psymbol  = 'R'
  , pname    = "ruins"
  , pfreq    = [("battle", 660), ("ambush", 70)]
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["X"]
  , poverrideDark = []
  , poverrideLit = []
  }
ruin2 = ruin
  { pname    = "a scaffolding"
  , pfreq    = [("noise", 2000), ("exit", 5), ("museum", 1)]
  }
collapsed = PlaceKind
  { psymbol  = 'c'
  , pname    = "a hardware stack"
  , pfreq    = [("noise", 1)]
      -- no point taking up space if very little space taken,
      -- but if no other place can be generated, a failsafe is useful
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#"
               ]
  , poverrideDark = [('#', "doorlessMachineryOver_#")]
  , poverrideLit = [('#', "doorlessMachineryOver_#")]
  }
collapsed2 = collapsed
  { pfreq    = [("noise", 1000), ("battle", 200)]
  , ptopLeft = [ "X#"
               , "##"
               ]
  }
collapsed3 = collapsed
  { pfreq    = [("noise", 2000), ("battle", 200)]
  , ptopLeft = [ "XX#"
               , "###"
               ]
  }
collapsed4 = collapsed
  { pfreq    = [("noise", 2200), ("battle", 200)]
  , ptopLeft = [ "XXX#"
               , "####"
               ]
  }
collapsed5 = collapsed
  { pfreq    = [("noise", 3000), ("battle", 500)]
  , ptopLeft = [ "XX#"
               , "X##"
               , "###"
               ]
  }
collapsed6 = collapsed
  { pfreq    = [("noise", 4000), ("battle", 1000)]
  , ptopLeft = [ "XXX#"
               , "X###"
               , "####"
               ]
  }
collapsed7 = collapsed
  { pfreq    = [("noise", 4000), ("battle", 1000)]
  , ptopLeft = [ "XXX#"
               , "XX##"
               , "####"
               ]
  }
pillar = PlaceKind
  { psymbol  = 'p'
  , pname    = "a market"
  , pfreq    = [("rogue", 300), ("arena", 10000), ("empty", 400)]
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FWall
  -- Larger rooms require support pillars.
  , ptopLeft = [ "····"
               , "·O··"
               , "····"
               , "····"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
pillar2 = pillar
  { pname    = "a mall"
  , pfreq    = [("rogue", 10000), ("arena", 100000), ("empty", 5000)]
  , ptopLeft = [ "O····"
               , "·····"
               , "·····"
               , "···O·"
               , "····~"
               ]
  , poverrideDark = [('~', "poolOver_~_Dark")]
  , poverrideLit = [('~', "poolOver_~_Lit")]
  }
pillar3 = pillar
  { pname    = "a court"
  , pfreq    = [ ("rogue", 250), ("arena", 15), ("museum", 10)
               , ("laboratory", 200) ]
  , ptopLeft = [ "#··"
               , "···"
               , "···"
               ]
  }
pillar4 = pillar
  { pname    = "a plaza"
  , pfreq    = [ ("rogue", 1500), ("arena", 5000)
               , ("museum", 4000), ("laboratory", 1500) ]
  , ptopLeft = [ "#·#·"
               , "····"
               , "#···"
               , "····"
               ]
  }
pillar5 = pillar
  { pname    = "a bank outlet"
  , pfreq    = [ ("rogue", 1200), ("arena", 6000)
               , ("empty", 600), ("exit", 600) ]
  , ptopLeft = [ "&i%·"
               , "ii#·"
               , "%#+·"
               , "····"
               ]
  , poverrideDark = [ ('&', "cache deposit"), ('+', "trapped door")
                    , ('i', "floorActorItem") ]  -- lit or not, randomly
  , poverrideLit = [ ('&', "cache deposit"), ('+', "trapped door")
                   , ('i', "floorActorItem") ]  -- lit or not, randomly
  }
pillar6 = pillar
  { pname    = "a jewelry store"
  , pfreq    = [ ("rogue", 1200), ("arena", 6000)
               , ("museum", 7000), ("empty", 500) ]
  , ptopLeft = [ "Of··"
               , "ff%·"
               , "·%&·"
               , "····"
               ]
  , poverrideDark = [ ('&', "cache jewelry"), ('O', "lampPostOver_O")
                    , ('f', "floorActorLit") ]
  , poverrideLit = [ ('&', "cache jewelry"), ('O', "lampPostOver_O")
                   , ('f', "floorActorLit") ]
  }
colonnade = PlaceKind
  { psymbol  = 'c'
  , pname    = "a colonnade"
  , pfreq    = [ ("rogue", 12), ("noise", 1000), ("escape", 200)
               , ("exit", 200) ]
  , prarity  = [(1, 12), (10, 12)]
  , pcover   = CAlternate
  , pfence   = FFloor
  , ptopLeft = [ "#·"
               , "··"
               ]
  , poverrideDark = []
  , poverrideLit = []
  }
colonnade2 = colonnade
  { pfreq    = [("rogue", 300)]
  , prarity  = [(1, 1)]
  , pfence   = FWall
  , ptopLeft = [ "#·"
               , "·#"
               ]
  }
colonnade3 = colonnade
  { prarity  = [(1, 120), (10, 120)]
  , ptopLeft = [ "··#"
               , "·#·"
               , "#··"
               ]
  }
colonnade4 = colonnade
  { prarity  = [(1, 1)]
  , pfreq    = [("rogue", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "#··"
               , "·#·"
               , "··#"
               ]
  }
colonnade5 = colonnade
  { prarity  = [(1, 25), (10, 25)]
  , ptopLeft = [ "#··"
               , "··#"
               ]
  }
colonnade6 = colonnade
  { prarity  = [(1, 14), (10, 14)]
  , ptopLeft = [ "#·"
               , "··"
               , "·#"
               ]
  }
colonnade7 = colonnade
  { pfreq    = [("arena", 50), ("museum", 30), ("empty", 800)]
  , prarity  = [(1, 7), (10, 7)]
  , ptopLeft = [ "O·"
               , "··"
               ]
  }
colonnade8 = colonnade7
  { prarity  = [(1, 50), (10, 50)]
  , ptopLeft = [ "··O"
               , "·O·"
               , "O··"
               ]
  }
colonnade9 = colonnade7
  { prarity  = [(1, 20), (10, 20)]
  , ptopLeft = [ "O··"
               , "··O"
               ]
  }
colonnade10 = colonnade7
  { prarity  = [(1, 10), (10, 10)]
  , ptopLeft = [ "O·"
               , "··"
               , "·O"
               ]
  }
lampPost = PlaceKind
  { psymbol  = 'l'
  , pname    = "a lamp-lit area"
  , pfreq    = [ ("escape", 200), ("zoo", 100), ("ambush", 1000)
               , ("battle", 100) ]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FNone
  , ptopLeft = [ "X·X"
               , "·O·"
               , "X·X"
               ]
  , poverrideDark = [('O', "lampPostOver_O"), ('·', "floorActorLit")]
  , poverrideLit = [('O', "lampPostOver_O"), ('·', "floorActorLit")]
  }
lampPost2 = lampPost
  { ptopLeft = [ "···"
               , "·O·"
               , "···"
               ]
  }
lampPost3 = lampPost
  { pfreq    = [("escape", 3000), ("zoo", 500), ("battle", 1100)]
  , ptopLeft = [ "XX·XX"
               , "X···X"
               , "··O··"
               , "X···X"
               , "XX·XX"
               ]
  }
lampPost4 = lampPost
  { pfreq    = [("escape", 3000), ("zoo", 500), ("battle", 600)]
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
  , pfreq    = [("brawl", 500)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "··s"
               , "sO·"
               , "Xs·"
               ]
  , poverrideDark = [ ('O', "treeShadeOver_O_Dark")
                    , ('s', "treeShadeOver_s_Dark")
                    , ('·', "shaded ground") ]
  , poverrideLit = [ ('O', "treeShadeOver_O_Lit")
                   , ('s', "treeShadeOver_s_Lit")
                   , ('·', "shaded ground") ]
  }
fogClump = PlaceKind
  { psymbol  = 'f'
  , pname    = "a foggy patch"
  , pfreq    = [("empty", 400), ("shootout", 70), ("escape", 60)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";X"
               ]
  , poverrideDark = [('f', "fogClumpOver_f_Dark"), (';', "fog Lit")]
  , poverrideLit = [('f', "fogClumpOver_f_Lit"), (';', "fog Lit")]
  }
fogClump2 = fogClump
  { pfreq    = [("empty", 3000), ("shootout", 400), ("escape", 100)]
  , ptopLeft = [ "X;f"
               , "f;f"
               , ";;f"
               , "Xff"
               ]
  }
smokeClump = PlaceKind
  { psymbol  = 's'
  , pname    = "a smoky patch"
  , pfreq    = [("exit", 50), ("zoo", 40), ("ambush", 50)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";f"
               , ";X"
               ]
  , poverrideDark = [ ('f', "smokeClumpOver_f_Dark"), (';', "smoke Lit")
                    , ('·', "floorActorDark") ]
  , poverrideLit = [ ('f', "smokeClumpOver_f_Lit"), (';', "smoke Lit")
                   , ('·', "floorActorLit") ]
  }
smokeClump2 = smokeClump
  { pfreq    = [("exit", 300), ("zoo", 200), ("ambush", 150)]
  , ptopLeft = [ "X;f"
               , "f;f"
               , ";;f"
               , "Xff"
               ]
  }
smokeClump3FGround = smokeClump
  { pname    = "a burned out area"
  , pfreq    = [("laboratory", 25)]
  , prarity  = [(1, 1)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ ";f;"
               , "f·f"
               , "f·f"
               , ";f;"
               ]
      -- should not be used in caves with trails, because bushes should
      -- not grow over such artificial trails
  }
bushClump = PlaceKind
  { psymbol  = 'b'
  , pname    = "a bushy patch"
  , pfreq    = [("shootout", 100)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FNone
  , ptopLeft = [ "f;"
               , ";X"
               , ";f"
               ]
  , poverrideDark = [('f', "bushClumpOver_f_Dark"), (';', "bush Lit")]
  , poverrideLit = [('f', "bushClumpOver_f_Lit"), (';', "bush Lit")]
      -- should not be used in caves with trails, because bushes can't
      -- grow over such artificial trails
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
  , poverrideDark = [ ('*', "oil spill"), ('g', "frozen path")
                    , ('O', "lampPostOver_O")
                    , ('f', "floorActorLit"), ('r', "rubbleOrWaste_Dark") ]
  , poverrideLit = [ ('*', "oil spill"), ('g', "frozen path")
                   , ('O', "lampPostOver_O")
                   , ('f', "floorActorLit"), ('r', "rubbleOrWaste_Lit") ]
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
  { pfreq    = [("escape down", 1000)]
  , pfence   = FFloor
  , ptopLeft = [ "·#·"
               , "#>#"
               , "·#·"
               ]
  }
escapeDown4 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pfence   = FWall
  , ptopLeft = [ "^·^"
               , "·>·"
               , "^·^"
               ]
  }
escapeDown5 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "r#·"
               , "r>#"
               , "rrr"
               ]
  }
escapeDown6 = escapeDown
  { pfreq    = [("escape down", 500)]
  , pfence   = FWall
  , ptopLeft = [ "··#··"
               , "·#*#·"
               , "#*>*#"
               , "·#*#·"
               , "··#··"
               ]
  }
escapeDown7 = escapeDown
  { pfreq    = [("escape down", 500)]
  , pfence   = FFloor
  , ptopLeft = [ "·*#*·"
               , "*#*#*"
               , "#*>*#"
               , "*#*#*"
               , "·*#*·"
               ]
  }
escapeDown8 = escapeDown
  { pfreq    = [("escape down", 500)]
  , pcover   = CMirror
  , pfence   = FWall
  , ptopLeft = [ "··#g·"
               , "·#gg·"
               , "·#>g#"
               , "·gg#·"
               , "g·#··"
               ]
  }
escapeDown9 = escapeDown
  { pfreq    = [("escape down", 1000)]
  , pcover   = CMirror
  , pfence   = FFloor
  , ptopLeft = [ "·f·#"
               , "%f>·"
               , "%Of·"
               , "ff%%"
               ]
  }
staircase = PlaceKind
  { psymbol  = '/'
  , pname    = "a staircase"
  , pfreq    = [("tiny staircase", 1)]  -- no cover when arriving; low freq
  , prarity  = [(1, 100), (10, 100)]
  , pcover   = CVerbatim
  , pfence   = FGround
  , ptopLeft = [ "<S>"
               ]
  , poverrideDark = [ ('<', "staircase up"), ('>', "staircase down")
                    , ('I', "signboard"), ('S', "fillerWall") ]
  , poverrideLit = [ ('<', "staircase up"), ('>', "staircase down")
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
  , prarity  = [(1, 100), (10, 100)]
  , pcover   = CVerbatim
  , pfence   = FFloor
  , ptopLeft = [ "<S>"
               ]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
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
               , "·#~~~~~#·"
               , "#~~<S>~~#"
               , "·#~~~~~#·"
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
  , ptopLeft = [ "XX#·#·#·#XX"
               , "X#·#·#·#·#X"
               , "#·#·····#·#"
               , "·#··<S>··#·"
               , "#·#·····#·#"
               , "X#·#·#·#·#X"
               , "XX#·#·#·#XX"
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
               , "·~~~~~·"
               , "·~<S>~·"
               , "·~~~~~·"
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
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift12 = staircase12
  { pname     = "a lift"
  , pfreq     = [("closed lift", 4000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift13 = staircase13
  { pname     = "a lift"
  , pfreq     = [("closed lift", 6000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift14 = staircase14
  { pname     = "a lift"
  , pfreq     = [("closed lift", 10000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift15 = staircase15
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift16 = staircase16
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift17 = staircase17
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift18 = staircase18
  { pname     = "a lift"
  , pfreq     = [("closed lift", 80000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift19 = staircase19
  { pname     = "a lift"
  , pfreq     = [("closed lift", 20000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift20 = staircase20
  { pname     = "a lift"
  , pfreq     = [("closed lift", 5000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift21 = staircase21
  { pname     = "a lift"
  , pfreq     = [("closed lift", 5000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift22 = staircase22
  { pname     = "a lift"
  , pfreq     = [("closed lift", 2000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift23 = staircase23
  { pname     = "a lift"
  , pfreq     = [("closed lift", 1000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift24 = staircase24
  { pname     = "a lift"
  , pfreq     = [("closed lift", 1000)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
staircaseLift25 = staircase25
  { pname     = "a lift"
  , pfreq     = [("walled lift", 100)]
  , poverrideDark = overrideLift
  , poverrideLit = overrideLift
  }
pumps = PlaceKind
  { psymbol  = 'w'
  , pname    = "water pumps"
  , pfreq    = [ ("rogue", 200), ("laboratory", 100), ("empty", 2000)
               , ("brawl", 80), ("shootout", 50) ]
  , prarity  = [(1, 1)]
  , pcover   = CAlternate
  , pfence   = FWall
  , ptopLeft = [ "·f"
               , "%·"
               ]
  , poverrideDark = [ ('%', "doorlessMachineryOver_#")
                    , ('f', "pumpsOver_f_Dark") ]
  , poverrideLit = [ ('%', "doorlessMachineryOver_#")
                   , ('f', "pumpsOver_f_Lit") ]
  }
oval = PlaceKind
  { psymbol  = 'o'
  , pname    = "a dome"
  , pfreq    = [ ("rogue", 20000), ("arena", 30000), ("museum", 30000)
               , ("laboratory", 50000), ("empty", 5000), ("exit", 5000)
               , ("ambush", 20000) ]
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "####·"
               , "##···"
               , "#··tt"
               , "#·t··"
               , "··t··"
               ]
  , poverrideDark = [ ('t', "trailLit"), ('a', "alarmingTrailLit")
                    , ('~', "poolOver_~_Dark") ]
  , poverrideLit = [ ('t', "trailLit"), ('a', "alarmingTrailLit")
                   , ('~', "poolOver_~_Lit") ]
  }
ovalFloor = oval
  { pfreq    = [ ("rogue", 50000), ("arena", 60000), ("museum", 60000)
               , ("laboratory", 100000), ("empty", 10000), ("exit", 5000)
               , ("ambush", 100000) ]
  , pfence   = FGround
  , ptopLeft = [ "aXXX##"
               , "X+###·"
               , "X#a···"
               , "X#·a·a"
               , "##··a·"
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
ovalBasin = oval
  { pname    = "a water basin"
  , pfreq    = [ ("rogue", 40000), ("arena", 200000), ("laboratory", 200000)
               , ("empty", 20000) ]
  , pfence   = FGround
  , ptopLeft = [ "XXX+##"
               , "X###··"
               , "X#····"
               , "+#··~~"
               , "#··~~~"
               , "#··~~~"
               ]
  }
ovalBasin2 = oval
  { pname    = "a water basin"
  , pfreq    = [ ("rogue", 600), ("arena", 10000), ("laboratory", 3000)
               , ("empty", 1800) ]
  , pfence   = FWall
  , ptopLeft = [ "#···"
               , "··~~"
               , "·~~~"
               , "·~~~"
               ]
  }
squareBasin = oval
  { pname    = "a water basin"
  , pfreq    = [("arena", 15000), ("laboratory", 3000), ("empty", 4000)]
  , pfence   = FNone
  , ptopLeft = [ "OttOt"
               , "t~~~~"
               , "t~O~~"
               , "O~~~~"
               , "t~~~~"
               ]
  }
squareBasin2 = oval
  { pname    = "a water basin"
  , pfreq    = [("arena", 100000), ("laboratory", 50000), ("empty", 15000)]
  , pfence   = FNone
  , ptopLeft = [ "OtOttt"
               , "t~~~~~"
               , "O~~~~~"
               , "t~~O~~"
               , "t~~~~~"
               , "t~~~~~"
               ]
  }
floodedRoom = PlaceKind  -- Valid for any nonempty area, hence low frequency.
  { psymbol  = 'f'
  , pname    = "a flooded room"
  , pfreq    = [("rogue", 10), ("laboratory", 12), ("brawl", 40), ("zoo", 50)]
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = ["~"]
  , poverrideDark = []
  , poverrideLit = []
  }
maze = PlaceKind
  { psymbol  = 'm'
  , pname    = "an intricate maze"
  , pfreq    = [ ("rogue", 60), ("laboratory", 1500), ("arena", 3)
               , ("museum", 3), ("exit", 300) ]
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FWall
  , ptopLeft = [ "##··"
               , "#··#"
               , "··#·"
               ]
  , poverrideDark = [ ('&', "cache maze"), ('+', "trapped door")
                    , ('i', "floorActorItem")  -- lit or not, randomly
                    , ('$', "trappableWall") ]
  , poverrideLit = [ ('&', "cache maze"), ('+', "trapped door")
                   , ('i', "floorActorItem")  -- lit or not, randomly
                   , ('$', "trappableWall") ]
  }
maze2 = maze
  { pfreq    = [ ("rogue", 180), ("laboratory", 12000), ("arena", 6)
               , ("museum", 6), ("exit", 360) ]
  , ptopLeft = [ "#·##·"
               , "·#··#"
               , "···#·"
               ]
  }
maze3 = maze
  { pfreq    = [ ("rogue", 300), ("laboratory", 15000), ("arena", 9)
               , ("exit", 400) ]
  , ptopLeft = [ "##·##·"
               , "#·#··#"
               , "~·%···"
               ]
  }
mazeBig = maze
  { pfreq    = [ ("rogue", 600), ("laboratory", 3000), ("arena", 5000)
               , ("exit", 500) ]
  , pfence   = FNone
  , ptopLeft = [ "X$$$$"
               , "$·##·"
               , "$#···"
               , "$#·+%"
               , "$··%i"
               ]
  }
mazeBig2 = maze
  { pfreq    = [ ("rogue", 1500), ("laboratory", 8000), ("arena", 10000)
               , ("exit", 700) ]
  , pfence   = FNone
  , ptopLeft = [ "XX$$$~"
               , "X#···%"
               , "$·###·"
               , "$·+&%%"
               , "$·#iii"
               ]
  }
cells = PlaceKind
  { psymbol  = '#'
  , pname    = "air filters"
  , pfreq    = [ ("rogue", 48), ("laboratory", 48), ("museum", 10)
               , ("exit", 280), ("noise", 480)
               , ("zoo", 700), ("ambush", 100) ]
  , prarity  = [(1, 1)]
  , pcover   = CReflect
  , pfence   = FWall
  , ptopLeft = [ "#··"
               , "·%·"
               , "··#"
               ]
  , poverrideDark = [ ('%', "doorlessMachineryOver_#")
                    , ('f', "bushClumpOver_f_Dark"), ('o', "oilOver_o_Dark") ]
  , poverrideLit = [ ('%', "doorlessMachineryOver_#")
                   , ('f', "bushClumpOver_f_Lit"), ('o', "oilOver_o_Lit") ]
  }
cells2 = cells
  { pname    = "humidity equalizers"
  , prarity  = [(1, 3), (10, 3)]
  , ptopLeft = [ "f·#·"  -- extra column to avoid blocked exits
               , "·#··"
               , "·#··"
               ]
  }
cells3 = cells
  { pname    = "thermostat units"
  , ptopLeft = [ "·^#"
               , "·#~"
               , "··#"
               ]
  }
cells4 = cells
  { pname    = "a power node"
  , ptopLeft = [ "·o#"
               , "o#o"
               , "#o·"
               ]
  }
cells5 = cells  -- this one is distinct enough from others, so needs a boost
  { pname    = "broken robot holds"
  , pfreq    = [ ("rogue", 20), ("laboratory", 15)
               , ("empty", 80), ("exit", 90), ("noise", 150) ]
  , ptopLeft = [ "··#"
               , "··#"
               , "##o"
               ]
  }
cells6 = cells
  { pname    = "animal holding pens"
  , pfreq    = [ ("arena", 2), ("laboratory", 8), ("zoo", 80)]
  , ptopLeft = [ "··#"
               , "##'"
               ]
  }
cells7 = cells
  { pname    = "a defunct control room"
  , pfreq    = [ ("rogue", 5), ("laboratory", 10)
               , ("empty", 80), ("exit", 20), ("noise", 80) ]
  , pfence   = FFloor
  , ptopLeft = [ "%·o"
               , "·#o"
               ]
  }
tank = PlaceKind
  { psymbol  = 'c'
  , pname    = "a tank"
  , pfreq    = [("empty", 1)]
      -- no point taking up space if very little space taken,
      -- but if no other place can be generated, a failsafe is useful
  , prarity  = [(1, 1)]
  , pcover   = CStretch
  , pfence   = FNone
  , ptopLeft = [ "#"
               ]
  , poverrideDark = [('#', "doorlessWallOver_#"), ('r', "reinforced wall")]
  , poverrideLit = [('#', "doorlessWallOver_#"), ('r', "reinforced wall")]
  }
tank2 = tank
  { pfreq    = [ ("empty", 500), ("exit", 15), ("noise", 100)
               , ("battle", 50) ]
  , ptopLeft = [ "O#"
               , "##"
               ]
  }
tank3 = tank
  { pfreq    = [ ("empty", 600), ("exit", 30), ("noise", 200)
               , ("battle", 100) ]
  , ptopLeft = [ "rr#"
               , "r##"
               , "###"
               ]
  }
tank4 = tank
  { pfreq    = [ ("empty", 800), ("exit", 120), ("noise", 300)
               , ("battle", 300) ]
  , ptopLeft = [ "XXO#"
               , "Xrr#"
               , "Or##"
               , "####"
               ]
  }
tank5 = tank
  { pname    = "a cistern"
  , pfreq    = [ ("empty", 1000), ("exit", 150), ("noise", 300)
               , ("battle", 300) ]
  , ptopLeft = [ "XXr#"
               , "Xr##"
               , "r###"
               , "####"
               ]
  }
shuttleHusk = PlaceKind
  { psymbol  = 's'
  , pname    = "a shuttle husk"
  , pfreq    = [("empty", 1000), ("exit", 15000), ("ambush", 15000)]
  , prarity  = [(1, 1)]
  , pcover   = CMirror
  , pfence   = FGround
  , ptopLeft = [ "X·###·X"  -- 7 x 9
               , "X%#w#%X"
               , "#%···%#"
               , "#··h··#"
               , "#w··rw#"
               , "···rr&c"
               , "###&###"
               , "XhhchhX"
               , "hh#w#hh"
               ]
  , poverrideDark = [ ('·', "oily floor Dark")
                    , ('r', "rubbleOrWaste_Dark")
                    , ('#', "shuttle hull")
                    , ('c', "cache shuttle")
                    , ('h', "hardware rack")
                    , ('w', "reinforced wall") ]
  , poverrideLit = [ ('·', "oily floor Lit")
                   , ('r', "rubbleOrWaste_Lit")
                   , ('#', "shuttle hull")
                   , ('c', "cache shuttle")
                   , ('h', "hardware rack")
                   , ('w', "reinforced wall") ]
  }
shuttleHusk2 = shuttleHusk
  { pfreq    = [("empty", 1000), ("exit", 15000), ("ambush", 15000)]
  , ptopLeft = map (T.cons 'X' . flip T.snoc 'X')
               $ ptopLeft shuttleHusk  -- 9 x 9
  }
shuttleHusk3 = shuttleHusk
  { pfreq    = [("empty", 300), ("exit", 5000), ("ambush", 5000)]
  , ptopLeft = [ "X··##··X"  -- 8 x 8
               , "X#%ww%#X"
               , "#w····w#"
               , "····h·r#"
               , "#·rrrrr#"
               , "###&&###"
               , "XhhcchhX"
               , "hh#ww#hh"
               ]
  }
shuttleHusk4 = shuttleHusk3
  { pfreq    = [("empty", 300), ("exit", 5000), ("ambush", 5000)]
  , ptopLeft = map (T.cons 'X' . flip T.snoc 'X')
               $ ptopLeft shuttleHusk3  -- 10 x 8
  }
shuttleHusk5 = shuttleHusk
  { pfreq    = [("empty", 1600), ("exit", 80000), ("ambush", 80000)]
  , pfence   = FGround
  , ptopLeft = [ "···##···"  -- 8 x 10
               , "w#%ww%#w"
               , "X#····#X"
               , "X···h·#X"
               , "#w····w#"
               , "%rr····%"
               , "##rrrr##"
               , "X##&&##X"
               , "XhhcchhX"
               , "hh#ww#hh"
               ]
  }
shuttleHusk6 = shuttleHusk
  { pfreq    = [("empty", 2000), ("exit", 120000), ("ambush", 120000)]
  , ptopLeft = [ "X··###··X"  -- 9 x 10
               , "X#%#w#%#X"
               , "##·h·h·##"
               , "········%"
               , "#w·····w#"
               , "%·····rr%"
               , "##·rrrr##"
               , "X###&###X"
               , "XXhhchhXX"
               , "Xhh#w#hhX"
               ]
  }

-- * Helper functions

switchExitToUp :: Text -> PlaceKind -> PlaceKind
switchExitToUp terminal s = s
 { psymbol   = '<'
 , pname     = pname s <+> "up"
 , pfreq     = map (\(t, k) -> (toGroupName $ fromGroupName t <+> "up", k))
               $ pfreq s
 , poverrideDark = ('>', toGroupName $ terminal <+> "Dark")
                   : filter ((/= '>') . fst) (poverrideDark s)
 , poverrideLit = ('>', toGroupName $ terminal <+> "Lit")
                  : filter ((/= '>') . fst) (poverrideLit s)
 }

switchExitToDown :: Text -> PlaceKind -> PlaceKind
switchExitToDown terminal s = s
 { psymbol   = '>'
 , pname     = pname s <+> "down"
 , pfreq     = map (\(t, k) -> (toGroupName $ fromGroupName t <+> "down", k))
               $ pfreq s
 , poverrideDark = ('<', toGroupName $ terminal <+> "Dark")
                   : filter ((/= '<') . fst) (poverrideDark s)
 , poverrideLit = ('<', toGroupName $ terminal <+> "Lit")
                  : filter ((/= '<') . fst) (poverrideLit s)
 }

overrideGatedStaircase :: [(Char, GroupName TileKind)]
overrideGatedStaircase =
  [ ('<', "gated staircase up"), ('>', "gated staircase down")
  , ('I', "signboard"), ('S', "fillerWall") ]

switchStaircaseToGated :: PlaceKind -> PlaceKind
switchStaircaseToGated s = s
 { psymbol   = 'g'
 , pname     = T.unwords $ "a gated" : tail (T.words (pname s))
 , pfreq     = map (first (\t -> toGroupName $ "gated" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideGatedStaircase
 , poverrideLit = overrideGatedStaircase
 }

overrideGatedLift :: [(Char, GroupName TileKind)]
overrideGatedLift =
  [ ('<', "gated lift up"), ('>', "gated lift down")
  , ('I', "signboard"), ('S', "lift shaft") ]

switchLiftToGated :: PlaceKind -> PlaceKind
switchLiftToGated s = s
 { psymbol   = 'g'
 , pname     = T.unwords $ "a gated" : tail (T.words (pname s))
 , pfreq     = map (first (\t -> toGroupName $ "gated" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideGatedLift
 , poverrideLit = overrideGatedLift
 }

overrideWeldedStaircase :: [(Char, GroupName TileKind)]
overrideWeldedStaircase =
  [ ('<', "welded staircase up"), ('>', "ordinary staircase down")
  , ('I', "signboard"), ('S', "fillerWall") ]

switchStaircaseToWelded :: PlaceKind -> PlaceKind
switchStaircaseToWelded s = s
 { psymbol   = 'w'
 , pfreq     = map (first (\t -> toGroupName $ "welded" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideWeldedStaircase
 , poverrideLit = overrideWeldedStaircase
 }

overrideWeldedLift :: [(Char, GroupName TileKind)]
overrideWeldedLift =
  [ ('<', "welded lift up"), ('>', "ordinary lift down")
  , ('I', "signboard"), ('S', "lift shaft") ]

switchLiftToWelded :: PlaceKind -> PlaceKind
switchLiftToWelded s = s
 { psymbol   = 'w'
 , pfreq     = map (first (\t -> toGroupName $ "welded" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideWeldedLift
 , poverrideLit = overrideWeldedLift
 }

overrideOutdoor :: [(Char, GroupName TileKind)]
overrideOutdoor =
  [ ('<', "staircase outdoor up"), ('>', "staircase outdoor down")
  , ('I', "signboard"), ('S', "fillerWall") ]

switchStaircaseToOutdoor :: PlaceKind -> PlaceKind
switchStaircaseToOutdoor s = s
 { psymbol   = 'o'
 , pname     = "an outdoor area exit"
 , pfreq     = map (first (\t -> toGroupName $ "outdoor" <+> fromGroupName t))
               $ pfreq s
 , poverrideDark = overrideOutdoor
 , poverrideLit = overrideOutdoor
 }

switchEscapeToUp :: PlaceKind -> PlaceKind
switchEscapeToUp s = s
 { psymbol   = '<'
 , pname     = "an escape up"
 , pfreq     = map (\(_, n) -> ("escape up", n)) $ pfreq s
 , poverrideDark = ('>', "escape up") : poverrideDark s
 , poverrideLit = ('>', "escape up") : poverrideLit s
 }

switchEscapeToOutdoorDown :: PlaceKind -> PlaceKind
switchEscapeToOutdoorDown s = s
 { pname     = "outdoor escape route"
 , pfreq     = map (\(_, n) -> ("escape outdoor down", n)) $ pfreq s
 , poverrideDark = ('>', "escape outdoor down") : poverrideDark s
 , poverrideLit = ('>', "escape outdoor down") : poverrideLit s
 }

switchEscapeToSpaceshipDown :: PlaceKind -> PlaceKind
switchEscapeToSpaceshipDown s = s
 { pname     = "escape from spaceship"
 , pfreq     = map (\(_, n) -> ("escape spaceship down", n)) $ pfreq s
 , poverrideDark = ('>', "escape spaceship down") : poverrideDark s
 , poverrideLit = ('>', "escape spaceship down") : poverrideLit s
 }
