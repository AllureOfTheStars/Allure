-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The default game key-command mapping to be used for UI. Can be overriden
-- via macros in the config file.
module Client.UI.Content.KeyKind ( standardKeys ) where

import Control.Arrow (first)

import qualified Game.LambdaHack.Client.Key as K
import Game.LambdaHack.Client.UI.Content.KeyKind
import Game.LambdaHack.Client.UI.HumanCmd
import Game.LambdaHack.Common.Misc
import qualified Game.LambdaHack.Content.ItemKind as IK
import qualified Game.LambdaHack.Content.TileKind as TK

standardKeys :: KeyKind
standardKeys = KeyKind
  { rhumanCommands = map (first K.mkKM)
      -- All commands are defined here, except some movement and leader picking
      -- commands. All commands are shown on help screens except debug commands
      -- and macros with empty descriptions.
      -- The order below determines the order on the help screens.
      -- Remember to put commands that show information (e.g., enter targeting
      -- mode) first.

      -- Main Menu, which apart of these includes a few extra commands
      [ ("CTRL-x", ([CmdMenu], GameExit))
      , ("CTRL-u", ([CmdMenu], GameRestart "duel"))
      , ("CTRL-k", ([CmdMenu], GameRestart "skirmish"))
      , ("CTRL-m", ([CmdMenu], GameRestart "ambush"))
      , ("CTRL-b", ([CmdMenu], GameRestart "battle"))
      , ("CTRL-a", ([CmdMenu], GameRestart "campaign"))
      , ("CTRL-d", ([CmdMenu], GameDifficultyCycle))

      -- Movement and terrain alteration
      , ("less", ([CmdMove, CmdMinimal], TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "a level"
                            , feature = TK.Cause (IK.Ascend 1) }
           , TriggerFeature { verb = "escape"
                            , object = "spaceship"
                            , feature = TK.Cause (IK.Escape 1) } ]))
      , ("CTRL-less", ([CmdMove], TriggerTile
           [ TriggerFeature { verb = "ascend"
                            , object = "10 levels"
                            , feature = TK.Cause (IK.Ascend 10) } ]))
      , ("greater", ([CmdMove, CmdMinimal], TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "a level"
                            , feature = TK.Cause (IK.Ascend (-1)) }
           , TriggerFeature { verb = "escape"
                            , object = "spaceship"
                            , feature = TK.Cause (IK.Escape (-1)) } ]))
      , ("CTRL-greater", ([CmdMove], TriggerTile
           [ TriggerFeature { verb = "descend"
                            , object = "10 levels"
                            , feature = TK.Cause (IK.Ascend (-10)) } ]))
      , ("semicolon", ([CmdMove], MoveOnceToCursor))
      , ("colon",
         ( [CmdMove]
         , Macro "go to cursor for 100 steps"
                 ["semicolon", "CTRL-colon", "V"] ))
      , ("x",
         ( [CmdMove]
         , Macro "explore the closest unknown spot"
                 [ "CTRL-question"  -- no semicolon
                 , "CTRL-colon", "V" ] ))
      , ("X",
         ( [CmdMove]
         , Macro "autoexplore 100 times"
                 ["'", "CTRL-question", "CTRL-colon", "'", "V"] ))
      , ("CTRL-X",
         ( [CmdMove]
         , Macro "autoexplore 25 times"
                 ["'", "CTRL-question", "CTRL-colon", "'", "CTRL-V"] ))
      , ("R", ([CmdMove], Macro "rest (wait 100 times)"
                                ["KP_Begin", "V"]))
      , ("CTRL-R", ([CmdMove], Macro "rest (wait 25 times)"
                                     ["KP_Begin", "CTRL-V"]))
      , ("c", ([CmdMove], AlterDir
           [ AlterFeature { verb = "close"
                          , object = "door"
                          , feature = TK.CloseTo "closed door" }
           ]))
      , ("CTRL-KP_Begin", ([CmdMove], Macro "" ["KP_Begin"]))
      , ("period", ([CmdMove], Macro "" ["KP_Begin"]))
      , ("KP_5", ([CmdMove], Macro "" ["KP_Begin"]))
      , ("CTRL-KP_5", ([CmdMove], Macro "" ["KP_Begin"]))
      , ("i", ([CmdMove], Macro "" ["KP_Begin"]))

      -- Item use
      --
      -- For later:
      -- ApplyItem {verb = "eat", object = "food", symbol = ','}
      -- ApplyItem {verb = "activate", object = "emitter", symbol = '_'}
      -- ApplyItem {verb = "use", object = "tool", symbol = '~'}
      --
      , ("E", ([CmdItem], DescribeItem CEqp))
      , ("P", ([CmdItem], DescribeItem CInv))
      , ("S", ([CmdItem], DescribeItem CSha))
      , ("G", ([CmdItem], DescribeItem CGround))
      , ("A", ([CmdItem], AllOwned))
      , ("g", ([CmdItem, CmdMinimal],
               MoveItem [CGround] CEqp (Just "get") "an item" True))
      , ("d", ([CmdItem], MoveItem [CEqp, CInv, CSha] CGround
                                   Nothing "an item" False))
      , ("e", ([CmdItem], MoveItem [CGround, CInv, CSha] CEqp
                                   Nothing "an item" False))
      , ("p", ([CmdItem], MoveItem [CGround, CEqp, CSha] CInv
                                   Nothing "an item into inventory backpack"
                                   False))
      , ("s", ([CmdItem], MoveItem [CGround, CInv, CEqp] CSha
                                   Nothing "and share an item" False))
      , ("a", ([CmdItem, CmdMinimal], Apply
           [ ApplyItem { verb = "activate"
                       , object = "applicable item"
                       , symbol = ' ' }
           , ApplyItem { verb = "quaff"
                       , object = "drink"
                       , symbol = '!' }
           , ApplyItem { verb = "read"
                       , object = "chip"
                       , symbol = '?' }
           ]))
      , ("q", ([CmdItem], Apply [ApplyItem { verb = "quaff"
                                           , object = "drink"
                                           , symbol = '!' }]))
      , ("r", ([CmdItem], Apply [ApplyItem { verb = "read"
                                           , object = "chip"
                                           , symbol = '?' }]))
      , ("f", ([CmdItem, CmdMinimal], Project
           [ApplyItem { verb = "fling"
                      , object = "projectable item"
                      , symbol = ' ' }]))
      , ("t", ([CmdItem], Project [ ApplyItem { verb = "throw"
                                              , object = "missile"
                                              , symbol = '}' }
                                  , ApplyItem { verb = "throw"
                                              , object = "missile"
                                              , symbol = '{' } ]))
      , ("z", ([CmdItem], Project [ApplyItem { verb = "zap"
                                             , object = "mechanism"
                                             , symbol = '-' }]))

      -- Targeting
      , ("KP_Multiply", ([CmdTgt, CmdMinimal], TgtEnemy))
      , ("backslash", ([CmdTgt], Macro "" ["KP_Multiply"]))
      , ("slash", ([CmdTgt], TgtFloor))
      , ("plus", ([CmdTgt], EpsIncr True))
      , ("minus", ([CmdTgt], EpsIncr False))
      , ("BackSpace", ([CmdTgt], TgtClear))
      , ("CTRL-question", ([CmdTgt], CursorUnknown))
      , ("CTRL-I", ([CmdTgt], CursorItem))
      , ("CTRL-braceleft", ([CmdTgt], CursorStair True))
      , ("CTRL-braceright", ([CmdTgt], CursorStair False))

      -- Automation
      , ("equal", ([CmdAuto], SelectActor))
      , ("underscore", ([CmdAuto], SelectNone))
      , ("v", ([CmdAuto], Repeat 1))
      , ("V", ([CmdAuto], Repeat 100))
      , ("CTRL-v", ([CmdAuto], Repeat 1000))
      , ("CTRL-V", ([CmdAuto], Repeat 25))
      , ("apostrophe", ([CmdAuto], Record))
      , ("CTRL-T", ([CmdAuto], Tactic))
      , ("CTRL-A", ([CmdAuto], Automate))

      -- Assorted
      , ("question", ([CmdMeta], Help))
      , ("D", ([CmdMeta], History))
      , ("T", ([CmdMeta], MarkSuspect))
      , ("Z", ([CmdMeta], MarkVision))
      , ("C", ([CmdMeta], MarkSmell))
      , ("Tab", ([CmdMeta], MemberCycle))
      , ("ISO_Left_Tab", ([CmdMeta], MemberBack))
      , ("space", ([CmdMeta], Clear))
      , ("Escape", ([CmdMeta, CmdMinimal], Cancel))
      , ("Return", ([CmdMeta], Accept))

      -- Mouse
      , ("LeftButtonPress",
         ( [CmdMouse]
         , Macro "go to pointer for 100 steps"
                 [ "SHIFT-MiddleButtonPress", "semicolon"
                 , "CTRL-colon", "V" ] ))
      , ("SHIFT-LeftButtonPress",
         ( [CmdMouse]
         , Macro "run collectively to pointer for 100 steps"
                 [ "SHIFT-MiddleButtonPress", "CTRL-semicolon"
                 , "CTRL-colon", "V" ] ))
      , ("MiddleButtonPress", ([CmdMouse], CursorPointerEnemy))
      , ("SHIFT-MiddleButtonPress", ([CmdMouse], CursorPointerFloor))
      , ("CTRL-MiddleButtonPress",
         ([CmdInternal], Macro "" ["SHIFT-MiddleButtonPress"]))
      , ("RightButtonPress", ([CmdMouse], TgtPointerEnemy))

      -- Debug and others not to display in help screens
      , ("CTRL-s", ([CmdDebug], GameSave))
      , ("CTRL-f", ([CmdDebug], GameRestart "safari"))
      , ("CTRL-e", ([CmdDebug], GameRestart "defense"))
      , ("CTRL-O", ([CmdDebug], DescribeItem COrgan))
      , ("CTRL-period", ([CmdInternal], RunOnceAhead))
      , ("CTRL-semicolon", ([CmdInternal], RunOnceToCursor))
      , ("CTRL-colon", ([CmdInternal], ContinueToCursor))
      , ("CTRL-LeftButtonPress",
         ([CmdInternal], Macro "" ["SHIFT-LeftButtonPress"]))
      , ("CTRL-MiddleButtonPress",
         ([CmdInternal], Macro "" ["SHIFT-MiddleButtonPress"]))
     ]
  }
