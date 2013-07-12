{-# LANGUAGE OverloadedStrings #-}
-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | AI strategies for Allure of the Stars.
module Content.StrategyKind ( cdefs ) where

import Data.List

import Game.LambdaHack.Common.Ability
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Content.StrategyKind

cdefs :: ContentDef StrategyKind
cdefs = ContentDef
  { getSymbol = ssymbol
  , getName = sname
  , getFreq = sfreq
  , validate = svalidate
  , content =
      [noAbility, onlyFollowTrack, meleeAdjacent, meleeAndRanged, animalAbility, robotAbility, fullAbility]
  }
noAbility,        onlyFollowTrack, meleeAdjacent, meleeAndRanged, animalAbility, robotAbility, fullAbility :: StrategyKind

noAbility = StrategyKind  -- not even projectiles will fly
  { ssymbol    = '@'
  , sname      = "noAbility"
  , sfreq      = [("noAbility", 1)]
  , sabilities = []
  }

onlyFollowTrack = StrategyKind  -- projectiles enabled
  { ssymbol    = '@'
  , sname      = "onlyFollowTrack"
  , sfreq      = [("onlyFollowTrack", 1)]
  , sabilities = [Track]
  }

meleeAdjacent = StrategyKind
  { ssymbol    = '@'
  , sname      = "meleeAdjacent"
  , sfreq      = [("meleeAdjacent", 1)]
  , sabilities = [Track, Melee]
  }

meleeAndRanged = StrategyKind  -- melee and reaction fire
  { ssymbol    = '@'
  , sname      = "meleeAndRanged"
  , sfreq      = [("meleeAndRanged", 1)]
  , sabilities = [Track, Melee, Ranged]
  }

animalAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "animalAbility"
  , sfreq      = [("animalAbility", 1)]
  , sabilities = [Track, Flee, Melee, Chase, Wander]
  }

robotAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "robotAbility"
  , sfreq      = [("robotAbility", 1)]
  , sabilities = delete Flee [minBound..maxBound]
  }

fullAbility = StrategyKind
  { ssymbol    = '@'
  , sname      = "fullAbility"
  , sfreq      = [("fullAbility", 1)]
  , sabilities = [minBound..maxBound]
  }
