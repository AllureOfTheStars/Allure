-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Definitions of items embedded in map tiles.
module Content.ItemKindEmbed
  ( embeds
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Game.LambdaHack.Common.Color
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Flavour
import Game.LambdaHack.Common.ItemAspect (Aspect (..))
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.ItemKind

embeds :: [ItemKind]
embeds =
  [scratchOnWall, obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signboardExit, signboardMap, fireSmall, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, staircaseTrapUp, staircaseTrapDown, pulpit]
  -- Allure-specific
  ++ [blackStarrySky, ruinedFirstAidKit, wall3dBillboard, jewelryDisplay, liftUp, liftDown, liftTrap, liftTrap2]
scratchOnWall,    obscenePictogram, subtleFresco, treasureCache, treasureCacheTrap, signboardExit, signboardMap, fireSmall, fireBig, frost, rubble, doorwayTrapTemplate, doorwayTrap1, doorwayTrap2, doorwayTrap3, stairsUp, stairsDown, escape, staircaseTrapUp, staircaseTrapDown, pulpit :: ItemKind
-- Allure-specific
blackStarrySky,       ruinedFirstAidKit, wall3dBillboard, jewelryDisplay, liftUp, liftDown, liftTrap, liftTrap2 :: ItemKind

-- Make sure very few walls are substantially useful, e.g., caches,
-- and none that are secret. Otherwise the player will spend a lot of time
-- bumping walls, which is boring compare to fights or dialogues
-- and ever worse, the player will bump all secret walls, wasting time
-- and foregoing the fun of guessing how to find entrance to a disjoint part
-- of the level by bumping the least number of secret walls.
scratchOnWall = ItemKind
  { isymbol  = '?'
  , iname    = "scratch on wall"
  , ifreq    = [("scratch on wall", 1)]
  , iflavour = zipPlain [BrBlack]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "scratch"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []
  , ieffects = [Temporary "start making sense of the scratches", DetectHidden 3]
  , ifeature = [Durable]
  , idesc    = "A seemingly random series of scratches, carved deep into the wall."
  , ikit     = []
  }
obscenePictogram = ItemKind
  { isymbol  = '*'
  , iname    = "repulsing graffiti"
  , ifreq    = [("obscene pictogram", 1)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "infuriate"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 7]
  , ieffects = [ Recharging $ Temporary "enter unexplainable rage at a glimpse of the inscrutable graffiti"
               , Recharging $ RefillCalm (-20)
               , Recharging $ OneOf
                   [ toOrganGood "strengthened" (3 + 1 `d` 2)
                   , CreateItem CInv "sandstone rock" timerNone ] ]
  , ifeature = [Durable]
  , idesc    = ""
  , ikit     = []
  }
subtleFresco = ItemKind
  { isymbol  = '*'
  , iname    = "subtle mural"
  , ifreq    = [("subtle fresco", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "sooth"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 7]
  , ieffects = [ Temporary "be entranced by the subtle mural"
               , RefillCalm 2
               , Recharging $ toOrganGood "far-sighted" (3 + 1 `d` 2)
               , Recharging $ toOrganGood "keen-smelling" (3 + 1 `d` 2) ]
  , ifeature = [Durable]
  , idesc    = "Expensive yet tasteful."
  , ikit     = []
  }
treasureCache = stairsUp
  { isymbol  = 'O'
  , iname    = "intact deposit box"
  , ifreq    = [("treasure cache", 1)]
  , iflavour = zipPlain [BrBlue]
  , ieffects = [CreateItem CGround "common item" timerNone]
  , idesc    = ""
  }
treasureCacheTrap = ItemKind
  { isymbol  = '^'
  , iname    = "anti-theft protection"
  , ifreq    = [("treasure cache trap", 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "taint"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []
  , ieffects = [OneOf [ toOrganNoTimer "poisoned", Explode "glue"
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1)
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1)
                      , RefillCalm (-1), RefillCalm (-1) ]]
  , ifeature = []  -- not Durable, springs at most once
  , idesc    = ""
  , ikit     = []
  }
signboardExit = ItemKind
  { isymbol  = '?'
  , iname    = "signboard with exits"
  , ifreq    = [("signboard", 80)]
  , iflavour = zipPlain [BrMagenta]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "whack"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []
  , ieffects = [DetectExit 100]  -- low tech, hence fully operational
  , ifeature = [Durable]
  , idesc    = "Mandatory emergency exit information in low-tech form."
  , ikit     = []
  }
signboardMap = signboardExit
  { iname    = "signboard with a map"
  , ifreq    = [("signboard", 20)]
  , ieffects = [DetectEmbed 12]  -- low tech, hence fully operational
  , idesc    = "Detailed schematics for the maintenance crew."
  }
fireSmall = ItemKind
  { isymbol  = '%'
  , iname    = "small fire"
  , ifreq    = [("small fire", 1)]
  , iflavour = zipPlain [BrRed]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []
  , ieffects = [Burn 1, Explode "single spark"]
  , ifeature = [Durable]
  , idesc    = "A few small logs, burning brightly."
  , ikit     = []
  }
fireBig = fireSmall
  { isymbol  = 'O'
  , iname    = "big fire"
  , ifreq    = [("big fire", 1)]
  , ieffects = [ Burn 2, Explode "spark"
               , CreateItem CInv "wooden torch" timerNone ]
  , ifeature = [Durable]
  , idesc    = "Glowing with light and warmth."
  , ikit     = []
  }
frost = ItemKind
  { isymbol  = '^'
  , iname    = "frost"
  , ifreq    = [("frost", 1)]
  , iflavour = zipPlain [BrBlue]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "burn"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []
  , ieffects = [ Burn 1  -- sensory ambiguity between hot and cold
               , RefillCalm 20  -- cold reason
               , PushActor (ThrowMod 100 50) ]  -- slippery ice, 1 step, slow
  , ifeature = [Durable]
  , idesc    = "Intricate patterns of shining ice."
  , ikit     = []
  }
rubble = ItemKind
  { isymbol  = '&'
  , iname    = "rubble"
  , ifreq    = [("rubble", 1)]
  , iflavour = zipPlain [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "bury"
  , iweight  = 100000
  , idamage  = 0
  , iaspects = []
  , ieffects = [OneOf [ Explode "glass piece", Explode "waste"
                      , Summon "animal" $ 1 `dL` 2, toOrganNoTimer "poisoned"
                      , CreateItem CGround "common item" timerNone
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1)
                      , RefillCalm (-1), RefillCalm (-1), RefillCalm (-1) ]]
  , ifeature = [Durable]
  , idesc    = "Broken chunks of foam concrete and glass."
  , ikit     = []
  }
doorwayTrapTemplate = ItemKind
  { isymbol  = '+'
  , iname    = "doorway trap"
  , ifreq    = [("doorway trap unknown", 1)]
  , iflavour = zipPlain brightCol
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "cripple"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []
  , ieffects = []
  , ifeature = [HideAs "doorway trap unknown"]
      -- not Durable, springs at most once
  , idesc    = "Just turn the handle..."
  , ikit     = []
  }
doorwayTrap1 = doorwayTrapTemplate
  { ifreq    = [("doorway trap", 50)]
  , ieffects = [toOrganBad "blind" $ (1 `dL` 4) * 10]
  -- , idesc    = ""
  }
doorwayTrap2 = doorwayTrapTemplate
  { ifreq    = [("doorway trap", 25)]
  , ieffects = [toOrganBad "slowed" $ (1 `dL` 4) * 10]
  -- , idesc    = ""
  }
doorwayTrap3 = doorwayTrapTemplate
  { ifreq    = [("doorway trap", 25)]
  , ieffects = [toOrganBad "weakened" $ (1 `dL` 4) * 10 ]
  -- , idesc    = ""
  }
stairsUp = ItemKind
  { isymbol  = '<'
  , iname    = "staircase up"
  , ifreq    = [("staircase up", 1)]
  , iflavour = zipPlain [BrWhite]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "crash"  -- the verb is only used when the item hits,
                        -- not when it's applied otherwise, e.g., from tile
  , iweight  = 100000
  , idamage  = 0
  , iaspects = []
  , ieffects = [Ascend True]
  , ifeature = [Durable]
  , idesc    = "Stairs that rise towards the spaceship core."
  , ikit     = []
  }
stairsDown = stairsUp
  { isymbol  = '>'
  , iname    = "staircase down"
  , ifreq    = [("staircase down", 1)]
  , ieffects = [Ascend False]
  , idesc    = "Stairs that decend towards the outer ring."
  }
escape = stairsUp
  { isymbol  = 'E'
  , iname    = "escape"
  , ifreq    = [("escape", 1)]
  , iflavour = zipPlain [BrYellow]
  , ieffects = [Escape]
  , idesc    = ""  -- generic: moon outdoors, in spaceship, everywhere
  }
staircaseTrapUp = ItemKind
  { isymbol  = '^'
  , iname    = "staircase trap"
  , ifreq    = [("staircase trap up", 1)]
  , iflavour = zipPlain [Red]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "buffet"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []
  , ieffects = [ Temporary "be caught in decompression blast"
               , Teleport $ 3 + 1 `dL` 10 ]
  , ifeature = []  -- not Durable, springs at most once
  , idesc    = ""
  , ikit     = []
  }
-- Needs to be separate from staircaseTrapUp, to make sure the item is
-- registered after up staircase (not only after down staircase)
-- so that effects are invoked in the proper order and, e.g., teleport works.
staircaseTrapDown = staircaseTrapUp
  { ifreq    = [("staircase trap down", 1)]
  , iverbHit = "open up under"
  , ieffects = [ Temporary "tumble down the stairwell"
               , toOrganGood "drunk" (20 + 1 `d` 5) ]
  , idesc    = "A treacherous slab, to teach those who are too proud."
  }
pulpit = ItemKind
  { isymbol  = '?'
  , iname    = "VR harness"
  , ifreq    = [("pulpit", 1)]
  , iflavour = zipFancy [BrYellow]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "immerse"
  , iweight  = 10000
  , idamage  = 0
  , iaspects = []
  , ieffects = [ OneOf [ CreateItem CGround "any scroll" timerNone
                       , Detect 20
                       , Paralyze $ (1 `dL` 6) * 10
                       , toOrganGood "drunk" (20 + 1 `d` 5) ]
               , Explode "story-telling" ]
  , ifeature = []  -- not Durable, springs at most once
  , idesc    = ""
  , ikit     = []
  }

-- * Allure-specific

blackStarrySky = ItemKind
  { isymbol  = ' '
  , iname    = "black starry sky"
  , ifreq    = [("black starry sky", 1)]
  , iflavour = zipPlain [Black]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "awe"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []
  , ieffects = [ Temporary "look into the void and it looks back"
               , OneOf [RefillCalm 5, RefillCalm (-5)] ]
  , ifeature = [Durable]
  , idesc    = "The black starscape is constantly rotating. Occasionally a planet zips by, but is unable to disperse the blackness. The frantic dance is completely silent, muted, indifferent. There is not even a hint of vibration, just the sense of heaviness and dizziness. At the outermost deck, the curvature of the floor is unnoticeable, but artificial gravity apparently stronger than on Earth."  -- appears only on 100% flavour tiles, useless and trivial to notice, so the writeup can be longer
  , ikit     = []
  }
ruinedFirstAidKit = ItemKind
  { isymbol  = '?'
  , iname    = "ruined first aid kit"
  , ifreq    = [("ruined first aid kit", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "prick"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = []
  , ieffects = [ Temporary "inspect a tattered CPR instruction soaked in a residue of oily drugs"
               , OneOf [ toOrganNoTimer "poison resistant"
                       , toOrganNoTimer "slow resistant"
                       , toOrganGood "drunk" (20 + 1 `d` 5) ]
               , CreateItem CInv "needle" timerNone ]
  , ifeature = []  -- not Durable, springs at most once
  , idesc    = ""  -- regulations require
  , ikit     = []
  }
wall3dBillboard = ItemKind
  { isymbol  = '*'
  , iname    = "3D billboard"
  , ifreq    = [("3D billboard", 1)]
  , iflavour = zipPlain [BrGreen]
  , icount   = 1
  , irarity  = [(1, 1)]
  , iverbHit = "push"
  , iweight  = 1000
  , idamage  = 0
  , iaspects = [Timeout 3]
  , ieffects = [ Recharging $ Temporary "make it cough up a wobbly standalone hologram once more"
               , Recharging $ OneOf [ Explode "advertisement"
                                    , Explode "story-telling" ] ]
  , ifeature = [Durable]
  , idesc    = "One can still make out excited moves of bleached shapes."
  , ikit     = []
  }
jewelryDisplay = treasureCache
  { iname    = "jewelry display"
  , ifreq    = [("jewelry display", 1)]
  , ieffects = [CreateItem CGround "any jewelry" timerNone]
  , idesc    = ""
  }
liftUp = stairsUp
  { iname    = "lift up"
  , ifreq    = [("lift up", 1)]
  , idesc    = ""
  }
liftDown = stairsDown
  { iname    = "lift down"
  , ifreq    = [("lift down", 1)]
  , idesc    = ""
  }
liftTrap = staircaseTrapUp
  { iname    = "elevator trap"  -- hat tip to US heroes
  , ifreq    = [("lift trap", 100)]
  , iverbHit = "squeeze"
  , ieffects = [ Temporary "be crushed by the sliding doors"
               , DropBestWeapon, Paralyze 10 ]
  , idesc    = ""
  }
liftTrap2 = liftTrap
  { ifreq    = [("lift trap", 50)]
  , iverbHit = "choke"
  , ieffects = [ Temporary "inhale the gas lingering inside the cab"
               , toOrganBad "slowed" $ (1 `dL` 4) * 10 ]
  , idesc    = ""
  }
