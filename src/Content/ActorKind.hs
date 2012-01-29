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
      [hero, eye, fastEye, nose]
  }
hero,        eye, fastEye, nose :: ActorKind

hero = ActorKind
  { asymbol = '@'
  , aname   = "hero"
  , afreq   = [("hero", 1)]  -- Does not appear randomly in the dungeon.
  , acolor  = BrWhite  -- Heroes white, monsters colorful.
  , ahp     = RollDice 50 1
  , aspeed  = 10
  , asight  = True
  , asmell  = False
  , aiq     = 13  -- Can see hidden doors, when he is under alien control.
  , aregen  = 1500
  }

eye = ActorKind
  { asymbol = 'r'
  , aname   = "deranged household robot"
  , afreq   = [("monster", 60), ("summon", 50)]
  , acolor  = BrBlue
  , ahp     = RollDice 1 12  -- Falls in 1--4 unarmed rounds.
  , aspeed  = 10
  , asight  = True
  , asmell  = False
  , aiq     = 8
  , aregen  = 1500
  }
fastEye = ActorKind
  { asymbol = 'm'
  , aname   = "deformed monkey"
  , afreq   = [("monster", 10)]
  , acolor  = BrRed
  , ahp     = RollDice 1 6  -- Falls in 1--2 unarmed rounds.
  , aspeed  = 4
  , asight  = True
  , asmell  = False
  , aiq     = 12
  , aregen  = 1500
  }
nose = ActorKind
  { asymbol = 'h'
  , aname   = "tentacled horror"
  , afreq   = [("monster", 20), ("summon", 100)]
  , acolor  = Green
  , ahp     = RollDice 6 2
  , aspeed  = 11
  , asight  = False
  , asmell  = True
  , aiq     = 0
  , aregen  = 1500
  }
