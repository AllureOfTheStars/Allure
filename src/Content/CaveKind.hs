module Content.CaveKind ( cdefs ) where

import Data.Ratio

import qualified Game.LambdaHack.Content.Content as Content
import Game.LambdaHack.Geometry
import Game.LambdaHack.Random as Random
import Game.LambdaHack.Content.CaveKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Tile as Tile

cdefs :: Content.CDefs CaveKind
cdefs = Content.CDefs
  { getSymbol = csymbol
  , getName = cname
  , getFreq = cfreq
  , validate = cvalidate
  , content =
      [rogue, arena, empty, noise, largeNoise]
  }
rogue,        arena, empty, noise, largeNoise :: CaveKind

rogue = CaveKind
  { csymbol           = '$'
  , cname             = "caveRogue"
  , cdesc             = "Storage area"
  , cfreq             = 20
  , cxsize            = fst normalLevelBound + 1
  , cysize            = snd normalLevelBound + 1
  , levelGrid         = (RollDice 3 2, RollDice 2 2)
  , minRoomSize       = (RollDice 2 2, RollDice 2 1)
  , darkRoomChance    = \ d -> Random.chance $ 1%((22 - (2 * fromIntegral d)) `max` 2)
  , extraConnects     = \ (x, y) -> (x * y) `div` 3
  , noRooms           = \ (x, y) -> Random.randomR (0, (x * y) `div` 3)
  , minStairsDistance = 30
  , doorChance        = Random.chance $ 2%3
  , doorOpenChance    = Random.chance $ 1%10
  , doorSecretChance  = Random.chance $ 1%4
  , citemNum          = RollDice 5 2
  , defTile           = Tile.wallP
  , corTile           = Tile.floorCorridorDarkP
  }
arena = rogue
  { csymbol           = 'A'
  , cname             = "caveArena"
  , cdesc             = "Recreational deck"
  , cfreq             = 20
  , levelGrid         = (RollDice 2 2, RollDice 2 2)
  , minRoomSize       = (RollDice 3 2, RollDice 2 1)
  , noRooms           = \ (x, y) -> Random.randomR (0, (x * y) `div` 2)
  , defTile           = Tile.floorCorridorLitP
  , corTile           = Tile.floorSpecialP
  }
empty = rogue
  { csymbol           = '.'
  , cname             = "caveEmpty"
  , cdesc             = "Construction site"
  , cfreq             = 20
  , levelGrid         = (RollDice 2 2, RollDice 1 2)
  , minRoomSize       = (RollDice 4 3, RollDice 4 1)
  , noRooms           = \ (x, y) -> Random.randomR (max 0 (x * y - 3),
                                                    max 0 (x * y - 1))
  , defTile           = Tile.floorRoomLitP
  , corTile           = Tile.floorRoomLitP
  }
noise = rogue
  { csymbol           = '!'
  , cname             = "caveNoise"
  , cdesc             = "Machine rooms"
  , cfreq             = 20
  , levelGrid         = (RollDice 2 2, RollDice 1 2)
  , minRoomSize       = (RollDice 4 2, RollDice 4 1)
  , darkRoomChance    = \ _ -> return True
  , noRooms           = \ _ -> return 0
  , defTile           = \ t -> Tile.wallP t ||
                               Tile.floorCorridorLitP t
  , corTile           = Tile.floorSpecialP
  }
largeNoise = noise
  { csymbol           = 'L'
  , cname             = "caveLargeNoise"
  , cdesc             = ""
  , cfreq             = 0  -- experimental
  , cxsize            = 231
  , cysize            = 77
  , defTile           = \ t -> tfeature t == [F.Special] ||
                               Tile.floorCorridorLitP t
  }
