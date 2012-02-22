-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Monsters and heroes for Allure of the Stars.
module Content.ActorKind ( cdefs ) where

import Game.LambdaHack.Color
import qualified Game.LambdaHack.Content as Content
import Game.LambdaHack.Content.ActorKind
import Game.LambdaHack.Random

cdefs :: Content.CDefs ActorKind
cdefs = Content.CDefs
  { getSymbol = asymbol
  , getName = aname
  , getFreq = afreq
  , validate = avalidate
  , content =
      [hero, projectile, eye, fastEye, nose]
  }
hero,        projectile, eye, fastEye, nose :: ActorKind

hero = ActorKind
  { asymbol = '@'
  , aname   = "hero"
  , afreq   = [("hero", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite  -- Heroes white, monsters colorful.
  , ahp     = RollDice 60 1
  , aspeed  = 10
  , asight  = True
  , asmell  = False
  , aiq     = 13  -- Can see hidden doors, when he is under alien control.
  , aregen  = 5000
  }

projectile = ActorKind  -- includes homing missiles
  { asymbol = '*'
  , aname   = "projectile"
  , afreq   = [("projectile", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite
  , ahp     = RollDice 0 0
  , aspeed  = 4
  , asight  = False
  , asmell  = False
  , aiq     = 0
  , aregen  = maxBound
  }

eye = ActorKind
  { asymbol = 'r'
  , aname   = "deranged household robot"
  , afreq   = [("monster", 60), ("summon", 50)]
  , acolor  = BrYellow
  , ahp     = RollDice 3 4
  , aspeed  = 10
  , asight  = True
  , asmell  = False
  , aiq     = 8
  , aregen  = 1000
  }
fastEye = ActorKind
  { asymbol = 'm'
  , aname   = "deformed monkey"
  , afreq   = [("monster", 15)]
  , acolor  = BrMagenta
  , ahp     = RollDice 1 4
  , aspeed  = 5
  , asight  = True
  , asmell  = False
  , aiq     = 12
  , aregen  = 50  -- Regenerates fast (at max HP most of the time!).
  }
nose = ActorKind
  { asymbol = 'h'
  , aname   = "tentacled horror"
  , afreq   = [("monster", 20), ("summon", 100)]
  , acolor  = Green
  , ahp     = RollDice 7 2
  , aspeed  = 11
  , asight  = False
  , asmell  = True
  , aiq     = 0
  , aregen  = 1000
  }
