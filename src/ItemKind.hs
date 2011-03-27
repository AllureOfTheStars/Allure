module ItemKind where

import Data.Binary
import qualified Data.List as L
import qualified Data.IntMap as IM

import Color
import Effect

data ItemKind = ItemKind
  { jsymbol  :: !Char
  , jflavour :: [Flavour]
  , jname    :: String
  , jeffect  :: Effect
  , jquant   :: Roll
  , jfreq    :: !Int
  }
  deriving (Show, Eq, Ord)

-- a + b * lvl + roll(c + d * lvl)
type Roll = (Word8, Word8, Word8, Word8)

type Flavour = (Color, Bool)  -- the flag tells to use fancy color names

rollOne = (1, 0, 0, 0)

zipPlain cs = L.zip cs (repeat False)
zipFancy cs = L.zip cs (repeat True)
darkCol    = [Red .. Cyan]
brightCol  = [BrRed .. BrCyan]  -- BrBlack is not really that bright
stdCol     = darkCol ++ brightCol
stdFlav    = zipPlain stdCol ++ zipFancy stdCol

flavourToName :: Flavour -> String
flavourToName (c, False) = colorToName c
flavourToName (c, True) = colorToName' c

flavourToColor :: Flavour -> Color
flavourToColor (c, _) = c

dungeonLoot :: IM.IntMap ItemKind
dungeonLoot = IM.fromDistinctAscList (L.zip [0..] loot)

getIK ik = dungeonLoot IM.! ik

loot :: [ItemKind]
loot =
  [amulet, dart, gem, gem1, gem2, gem3, gold,
   potion_water, potion_healing,
   ring, scroll, sword,
   wand_domination]

amulet, dart, gem, gem1, gem2, gem3, gold :: ItemKind
potion, potion_water, potion_healing :: ItemKind
ring, scroll, sword :: ItemKind
wand, wand_domination :: ItemKind
amulet = ItemKind
  { jsymbol  = '"'
  , jflavour = [(BrWhite, True)]
  , jname    = "amulet"
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 10
  }
dart = ItemKind
  { jsymbol  = ')'
  , jflavour = [(Yellow, False)]
  , jname    = "dart"
  , jeffect  = Wound 1
  , jquant   = (3, 0, 6, 0)
  , jfreq    = 40
  }
gem = ItemKind
  { jsymbol  = '*'
  , jflavour = zipPlain brightCol  -- natural, so not fancy
  , jname    = "gem"
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 5  -- x4, below
  }
gem1 = gem
gem2 = gem
gem3 = gem
gold = ItemKind
  { jsymbol  = '$'
  , jflavour = [(BrYellow, False)]
  , jname    = "gold piece"
  , jeffect  = NoEffect
  , jquant   = (0, 3, 0, 10)
  , jfreq    = 80
  }
potion = ItemKind
  { jsymbol  = '!'
  , jflavour = zipFancy stdCol
  , jname    = "potion"
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 20
  }
potion_water = potion
  { jeffect  = ApplyWater
  }
potion_healing = potion
  { jeffect  = Heal 20
  }
ring = ItemKind
  { jsymbol  = '='
  , jflavour = [(BrWhite, False)]
  , jname    = "ring"
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 10
  }
scroll = ItemKind
  { jsymbol  = '?'
  , jflavour = zipFancy darkCol  -- arcane and old
  , jname    = "scroll"
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 10
  }
sword = ItemKind
  { jsymbol  = ')'
  , jflavour = [(BrCyan, False)]
  , jname    = "sword"
  , jeffect  = Wound 3
  , jquant   = rollOne
  , jfreq    = 70
  }
wand = ItemKind
  { jsymbol  = '/'
  , jflavour = [(BrRed, True)]
  , jname    = "wand"
  , jeffect  = NoEffect
  , jquant   = rollOne
  , jfreq    = 30
  }
wand_domination = wand
  { jeffect  = Dominate
  }
