-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Terrain tiles for Allure of the Stars.
module Content.TileKind ( cdefs ) where

import Control.Arrow (first)
import Data.Text (Text)
import qualified Data.Text as T

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.ContentDef
import qualified Game.LambdaHack.Common.Effect as Effect
import Game.LambdaHack.Common.Feature
import Game.LambdaHack.Common.Msg
import Game.LambdaHack.Content.TileKind

cdefs :: ContentDef TileKind
cdefs = ContentDef
  { getSymbol = tsymbol
  , getName = tname
  , getFreq = tfreq
  , validate = validateTileKind
  , content =
      [wall, wallCache, hardRock, oriel, pillar, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit]
      ++ map makeDarkColor [floorCorridorLit, floorActorLit, floorItemLit, floorActorItemLit]
  }
wall,        wallCache, hardRock, oriel, pillar, tree, wallSuspect, doorClosed, doorOpen, stairsUp, stairsDown, escapeUp, escapeDown, liftUp, lift, liftDown, unknown, floorCorridorLit, floorActorLit, floorItemLit, floorActorItemLit, floorRedLit, floorBlueLit, floorGreenLit :: TileKind

wall = TileKind
  { tsymbol  = '#'
  , tname    = "granite wall"
  , tfreq    = [ ("fillerWall", 1), ("cachable", 70)
               , ("legendLit", 100), ("legendDark", 100)
               , ("noiseSet", 55) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [HideAs "suspect wall"]
  }
wallCache = TileKind
  { tsymbol  = '&'
  , tname    = "cache"
  , tfreq    = [ ("cachable", 30)
               , ("legendLit", 100), ("legendDark", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Suspect, Cause $ Effect.CreateItem 1, ChangeTo "cachable"]
  }
hardRock = TileKind
  { tsymbol  = '#'
  , tname    = "outer hull"
  , tfreq    = [("basic outer fence", 100), ("oriels fence", 98)]
  , tcolor   = BrBlack
  , tcolor2  = BrBlack
  , tfeature = [Impenetrable]
  }
oriel = TileKind
  { tsymbol  = '\''
  , tname    = "oriel"
  , tfreq    = [("oriels fence", 2)]
  , tcolor   = White
  , tcolor2  = Black
  , tfeature = [Dark, Impenetrable]
  }
pillar = TileKind
  { tsymbol  = 'O'
  , tname    = "rock"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("combatSet", 3)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = []
  }
tree = TileKind
  { tsymbol  = 'O'
  , tname    = "tree"
  , tfreq    = [("combatSet", 8)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  , tfeature = []
  }
wallSuspect = TileKind
  { tsymbol  = '#'
  , tname    = "moldy wall"
  , tfreq    = [("suspect wall", 1)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Suspect, RevealAs "closed door"]
  }
doorClosed = TileKind
  { tsymbol  = '+'
  , tname    = "closed door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("closed door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [OpenTo "open door", HideAs "suspect wall"]
  }
doorOpen = TileKind
  { tsymbol  = '\''
  , tname    = "open door"
  , tfreq    = [("legendLit", 100), ("legendDark", 100), ("open door", 1)]
  , tcolor   = Brown
  , tcolor2  = BrBlack
  , tfeature = [Dark, Walkable, Clear, CloseTo "closed door"]
  }
stairsUp = TileKind
  { tsymbol  = '<'
  , tname    = "staircase up"
  , tfreq    = []  -- TODO: [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Cause $ Effect.Ascend 1]
  }
stairsDown = TileKind
  { tsymbol  = '>'
  , tname    = "staircase down"
  , tfreq    = []  -- TODO: [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear, Cause $ Effect.Ascend (-1)]
  }
escapeUp = TileKind
  { tsymbol  = '<'
  , tname    = "exit airlock up"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Cause $ Effect.Escape 1]
  }
escapeDown = TileKind
  { tsymbol  = '>'
  , tname    = "exit trapdoor down"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrYellow
  , tcolor2  = BrYellow
  , tfeature = [Walkable, Clear, Cause $ Effect.Escape (-1)]
  }
liftUp = TileKind
  { tsymbol  = '<'
  , tname    = "lift up"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrCyan
  , tcolor2  = BrCyan
  , tfeature = [Walkable, Clear, Cause $ Effect.Ascend 1]
  }
lift = TileKind
  { tsymbol  = '<'
  , tname    = "lift"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrBlue
  , tcolor2  = BrBlue
  , tfeature = [ Walkable, Clear
               , Cause $ Effect.Ascend 1
               , Cause $ Effect.Ascend (-1) ]
  }
liftDown = TileKind
  { tsymbol  = '>'
  , tname    = "lift down"
  , tfreq    = [("legendLit", 100), ("legendDark", 100)]
  , tcolor   = BrCyan
  , tcolor2  = BrCyan
  , tfeature = [Walkable, Clear, Cause $ Effect.Ascend (-1)]
  }
unknown = TileKind
  { tsymbol  = ' '
  , tname    = "unknown space"
  , tfreq    = [("unknown space", 1)]
  , tcolor   = defFG
  , tcolor2  = defFG
  , tfeature = [Dark]
  }
floorCorridorLit = TileKind
  { tsymbol  = '.'
  , tname    = "floor"
  , tfreq    = [ ("floorCorridorLit", 1), ("floorArenaLit", 1)
               , ("arenaSet", 1), ("noiseSet", 100), ("combatSet", 100) ]
  , tcolor   = BrWhite
  , tcolor2  = defFG
  , tfeature = [Walkable, Clear]
  }
floorActorLit = floorCorridorLit
  { tfreq    = [("floorActorLit", 1)]
  , tfeature = CanActor : tfeature floorCorridorLit
  }
floorItemLit = floorCorridorLit
  { tfreq    = []
  , tfeature = CanItem : tfeature floorCorridorLit
  }
floorActorItemLit = floorItemLit
  { tfreq    = [("legendLit", 100), ("emptySet", 1)]
  , tfeature = CanActor : tfeature floorItemLit
  }
floorRedLit = floorCorridorLit
  { tname    = "emergency walkway"
  , tfreq    = [("trailLit", 20)]
  , tcolor   = BrRed
  , tcolor2  = Red
  , tfeature = Trail : tfeature floorCorridorLit
  }
floorBlueLit = floorRedLit
  { tname    = "transport route"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrBlue
  , tcolor2  = Blue
  }
floorGreenLit = floorRedLit
  { tname    = "greenery trail"
  , tfreq    = [("trailLit", 100)]
  , tcolor   = BrGreen
  , tcolor2  = Green
  }


makeDark :: TileKind -> TileKind
makeDark k = let textLit :: Text -> Text
                 textLit t = maybe t (<> "Dark") $ T.stripSuffix "Lit" t
                 litFreq = map (first textLit) $ tfreq k
             in k { tfreq    = litFreq
                  , tfeature = Dark : tfeature k
                  }

makeDarkColor :: TileKind -> TileKind
makeDarkColor k = (makeDark k) { tcolor  = BrYellow
                               , tcolor2 = BrBlack
                               }
