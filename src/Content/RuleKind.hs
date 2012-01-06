module Content.RuleKind ( cdefs ) where

-- Cabal
import qualified Paths_Allure as Self (getDataFileName, version)

import Game.LambdaHack.Content.RuleKind
import Game.LambdaHack.Content.TileKind
import qualified Game.LambdaHack.Feature as F
import qualified Game.LambdaHack.Content.Content as Content

cdefs :: Content.CDefs RuleKind
cdefs = Content.CDefs
  { getSymbol = rsymbol
  , getName = rname
  , getFreq = rfreq
  , validate = ruvalidate
  , content =
      [standard]
  }
standard :: RuleKind

standard = RuleKind
  { rsymbol        = 's'
  , rname          = "standard Allure of the Stars ruleset"
  , rfreq          = [("standard", 100)]
    -- Check whether one location is accessible from another.
    -- Precondition: the two locations are next to each other.
    -- TODO: in the future check flying for chasms, swimming for water, etc.
  , raccessible    = \ _lxsize _sloc _src _tloc tgt ->
      F.Walkable `elem` tfeature tgt
  , rtitle         = "Allure of the Stars"
  , rpathsDataFile = Self.getDataFileName
  , rpathsVersion  = Self.version
  }
