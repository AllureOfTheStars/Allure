-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Monsters and heroes for Allure of the Stars.
module Content.ActorKind ( cdefs ) where

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Common.Time
import Game.LambdaHack.Content.ActorKind

cdefs :: ContentDef ActorKind
cdefs = ContentDef
  { getSymbol = asymbol
  , getName = aname
  , getFreq = afreq
  , validate = validateActorKind
  , content =
      [hero, projectile, eye, fastEye, nose]
  }
hero,        projectile, eye, fastEye, nose :: ActorKind

hero = ActorKind
  { asymbol = '@'
  , aname   = "hero"
  , afreq   = [("hero", 1)]
  , acolor  = BrWhite  -- modified if many hero factions
  , ahp     = 50
  , acalm   = 50
  , aspeed  = toSpeed 2
  , asight  = True
  , asmell  = False
  , aiq     = 15  -- higher that that leads to looping movement
  , aregen  = 500
  , acanDo  = [minBound..maxBound]
  , aitems  = [("fist", CBody), ("foot", CBody)]
  }

projectile = ActorKind  -- includes homing missiles
  { asymbol = '*'
  , aname   = "projectile"
  , afreq   = [("projectile", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite
  , ahp     = 0
  , acalm   = 50
  , aspeed  = toSpeed 0
  , asight  = False
  , asmell  = False
  , aiq     = 0
  , aregen  = maxBound
  , acanDo  = []
  , aitems  = []
  }

eye = ActorKind
  { asymbol = 'r'
  , aname   = "deranged home robot"
  , afreq   = [("robot", 60), ("horror", 60)]
  , acolor  = BrYellow
  , ahp     = 7 * d 4
  , acalm   = 50
  , aspeed  = toSpeed 2
  , asight  = True
  , asmell  = False
  , aiq     = 16  -- leads to robotic, repetitious, looping movement
  , aregen  = 100
  , acanDo  = [minBound..maxBound]
  , aitems  = [("fist", CBody), ("foot", CBody)]
  }
fastEye = ActorKind
  { asymbol = 'm'
  , aname   = "deformed monkey"
  , afreq   = [("animal", 15), ("horror", 15)]
  , acolor  = BrMagenta
  , ahp     = d 6
  , acalm   = 50
  , aspeed  = toSpeed 4
  , asight  = True
  , asmell  = False
  , aiq     = 12
  , aregen  = 10  -- Regenerates fast (at max HP most of the time!).
  , acanDo  = [minBound..maxBound]
  , aitems  = [("fist", CBody), ("foot", CBody)]
  }
nose = ActorKind
  { asymbol = 'h'
  , aname   = "tentacled horror"
  , afreq   = [("alien", 20), ("horror", 20)]
  , acolor  = Green
  , ahp     = 17 * d 2
  , acalm   = 50
  , aspeed  = toSpeed 1.8
  , asight  = False
  , asmell  = True
  , aiq     = 0
  , aregen  = 100
  , acanDo  = [minBound..maxBound]
  , aitems  = [("fist", CBody), ("foot", CBody)]
  }
