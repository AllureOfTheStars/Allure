-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game mode definitions.
module Content.ModeKind
  ( cdefs
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.IntMap.Strict as IM

import Content.ModeKindPlayer
import Game.LambdaHack.Common.ContentDef
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Content.ModeKind

cdefs :: ContentDef ModeKind
cdefs = ContentDef
  { getSymbol = msymbol
  , getName = mname
  , getFreq = mfreq
  , validateSingle = validateSingleModeKind
  , validateAll = validateAllModeKind
  , content = contentFromList
      [raid, brawl, shootout, escape, zoo, ambush, exploration, safari, safariSurvival, battle, battleSurvival, defense, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverExploration, screensaverSafari]
  }
raid,        brawl, shootout, escape, zoo, ambush, exploration, safari, safariSurvival, battle, battleSurvival, defense, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverExploration, screensaverSafari :: ModeKind

-- What other symmetric (two only-one-moves factions) and asymmetric vs crowd
-- scenarios make sense (e.g., are good for a tutorial or for standalone
-- extreme fun or are impossible as part of a crawl)?
-- sparse melee at night: no, shade ambush in brawl is enough
-- dense melee: no, keeping big party together is a chore and big enemy
--   party is less fun than huge enemy party
-- crowd melee in daylight: no, possible in crawl and at night is more fun
-- sparse ranged at night: no, less fun than dense and if no reaction fire,
--   just a camp fest or firing blindly
-- dense ranged in daylight: no, less fun than at night with flares
-- crowd ranged: no, fish in a barel, less predictable and more fun inside
--   crawl, even without reaction fire

raid = ModeKind  -- mini-crawl
  { msymbol = 'r'
  , mname   = "raid"
  , mfreq   = [("raid", 1), ("campaign scenario", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mdesc   = "The Neptune Area Administration confirms isolated spottings of many kinds of vermin in non-residential areas of the Triton moon largest city. In other words, Triton City sewers need purging. The first person to break through to the other exit will be paid 100 gold grains. The Administration urges participants not to fight each other."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mdesc   = "\"You scoundrel! You cheated in the sewers. Come alone to the woody biosphere behind the saloon at noon, if you dare. Given that I win, I take back the gold. Otherwise, you get the scrapping rights for the giant spaceliner's hull in orbit. Yes, it's mine, you tramp; here are the papers.\""
  }

-- The trajectory tip is important because of tactics of scout looking from
-- behind a bush and others hiding in mist. If no suitable bushes,
-- fire once and flee into mist or behind cover. Then whomever is out of LOS
-- range or inside mist can shoot at the last seen enemy locations,
-- adjusting and according to ounds and incoming missile trajectories.
-- If the scount can't find bushes or glass building to set a lookout,
-- the other team member are more spotters and guardians than snipers
-- and that's their only role, so a small party makes sense.
shootout = ModeKind  -- sparse ranged in daylight
  { msymbol = 's'
  , mname   = "shootout"
  , mfreq   = [("shootout", 1), ("campaign scenario", 1)]
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mdesc   = "The fight crashes over to a nearby mechanized farm. Law enforcement, crippled by the ban on firearms, won't show up until only wounded and dying remain to be revived and locked up. Farm supplies, scattered around, just beg to be flung at foes as improvised missiles. Intense light makes it easy to aim and also to discern trajectory of soaring items (point at projectiles with the crosshair in aiming mode)."
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "escape"
  , mfreq   = [("escape", 1), ("campaign scenario", 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mdesc   = "Bloodied spaceship deed in hand nonwithstanding, one can take possesion of the derelict spaceliner only via a shuttle from the Triton Spaceport across the city. After hours of being chased in the opposite direction towards the border wall, you sneak back and make a desperate dash through the very HQ of the enemy. Any valuables you find in this public park turned miscreant lair will be fair compensation for your losses, but you need to reach the exit before the foes find you."
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { msymbol = 'b'
  , mname   = "zoo"
  , mfreq   = [("zoo", 1), ("campaign scenario", 1)]
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mdesc   = "As justified and satisfying as setting enemy lair on fire was, it backfired by immediatelly spreading to the public zoo on your path. Crazed animals mill around while the flames ignite greenery and consume nets, cages and didactic equipment. Certainly nobody is willing to pursue you any more, whether that's a good sign or bad."
  }

-- The tactic is to sneak in the dark, highlight enemy with thrown torches
-- (and douse thrown enemy torches with blankets) and only if this fails,
-- actually scout using extended noctovision.
-- With reaction fire, larger team is more fun.
--
-- For now, while we have no shooters with timeout, massive ranged battles
-- without reaction fire don't make sense, because then usually only one hero
-- shoots (and often also scouts) and others just gather ammo.
ambush = ModeKind  -- dense ranged with reaction fire at night
  { msymbol = 'm'
  , mname   = "ambush"
  , mfreq   = [("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Not even the unexplained ruin of the largest and tightest security of Neptune's spaceports will prevent you from claiming your prize. After all, you didn't take to the space to let others decide your fate. Onward!"
  }

exploration = ModeKind
  { msymbol = 'c'
  , mname   = "crawl (long)"
  , mfreq   = [ ("crawl (long)", 1), ("exploration", 1)
              , ("campaign scenario", 1) ]
  , mroster = rosterExploration
  , mcaves  = cavesExploration
  , mdesc   = "You got stranded while happily looting the blasted bridge of an extravagantly luxurious, old, inert cruise liner. The spaceship, supposedly long deserted and barely sustaining life support, suddenly disengaged the shuttle you came in, manoeuvred off Triton orbit and now heads away from Neptune. Your current plan is to break through, exploring and gathering spoils, to the auxiliary engineering hub and shuttle bay somewhere among the giant spaceship's uppermost decks. This will yet turn your biggest victory. There are animal cries down below and ominous silence up above."
  }

safari = ModeKind  -- easter egg available only via screensaver
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mdesc   = "\"In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the topmost level.\" This is a VR recording recovered from an alien nest debris."
  }

-- * Testing modes

safariSurvival = ModeKind  -- testing scenario
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

battle = ModeKind  -- testing scenario
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  }

battleSurvival = ModeKind  -- testing scenario
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

defense = ModeKind  -- testing scenario; perhaps real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesExploration
  , mdesc   = "Don't let the half-witted humans derail your operation and flee, like the puny, naked, tentacle-less beasts that they are!"
  }

-- * Screensaver modes

screensave :: AutoLeader -> Roster -> Roster
screensave auto r =
  let f [] = []
      f ((player, initial) : rest) =
        (player {fleaderMode = LeaderAI auto}, initial) : rest
  in r {rosterList = f $ rosterList r}

screensaverRaid = raid
  { mname   = "auto-raid"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterRaid
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterBrawl
  }

screensaverShootout = shootout
  { mname   = "auto-shootout"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterShootout
  }

screensaverEscape = escape
  { mname   = "auto-escape"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterEscape
  }

screensaverZoo = zoo
  { mname   = "auto-zoo"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterZoo
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterAmbush
  }

screensaverExploration = exploration
  { mname   = "auto-crawl"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterExploration
  }

screensaverSafari = safari
  { mname   = "auto-safari"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = -- changing leader by client needed, because of TFollow
              screensave (AutoLeader False True) rosterSafari
  }

rosterRaid, rosterBrawl, rosterShootout, rosterEscape, rosterZoo, rosterAmbush, rosterExploration, rosterSafari, rosterSafariSurvival, rosterBattle, rosterBattleSurvival, rosterDefense :: Roster

rosterRaid = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiRaid}
                   , [(2, 1, "hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fhiCondPoly = hiRaid }
                   , [(2, 1, "hero")] )
                 , ( playerAnimal  -- starting over escape
                   , [(2, 2, "animal")] )
                 , ( playerRobot
                   , [(2, 1, "robot")] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Robot Anarchy")
                  , ("Spacefarer", "Horror Den")
                  , ("Spacefarer", "Red Collar Bro")
                  , ("Red Collar Bro", "Animal Kingdom")
                  , ("Red Collar Bro", "Robot Anarchy")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [("Robot Anarchy", "Animal Kingdom")] }

rosterBrawl = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller }
                   , [(3, 3, "hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller }
                   , [(3, 3, "hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

-- Exactly one scout gets a sight boost, to help the aggressor, because he uses
-- the scout for initial attack, while camper (on big enough maps)
-- can't guess where the attack would come and so can't position his single
-- scout to counter the stealthy advance.
rosterShootout = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller }
                   , [(5, 1, "scout hero"), (5, 2, "ranger hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller }
                   , [(5, 1, "scout hero"), (5, 2, "ranger hero")] )
                 , (playerRobot, [])  -- neutral
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterEscape = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiEscapist}
                   , [(7, 1, "scout hero"), (7, 2, "escapist hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False  -- start on escape
                                    , fhiCondPoly = hiDweller }
                   , [(7, 1, "scout hero"), (7, 7, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterZoo = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller }
                   , [(8, 5, "soldier hero")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(8, 100, "mobile animal")] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller }
                   , [(9, 1, "scout hero"), (9, 5, "ambusher hero")] )
                 , ( playerAntiHero { fname = "Gray Off-world Mercenary"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller }
                   , [(9, 1, "scout hero"), (9, 5, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Gray Off-world Mercenary")
                  , ("Spacefarer", "Horror Den")
                  , ("Gray Off-world Mercenary", "Horror Den") ]
  , rosterAlly = [] }

rosterExploration = Roster
  { rosterList = [ ( playerHero
                   , [(3, 3, "hero")] )
                 , ( playerMonster
                   , [] )
                 , ( playerAnimal
                   , -- Fun from the start to avoid empty initial level:
                     [ (3, 2 + d 2, "animal")  -- many, because no spawning
                     -- Optional huge battle at the end:
                     , (12, 100, "mobile animal") ] )
                 , ( playerRobot
                   , [(3, 2 + d 2, "robot")] ) ]  -- many, because no spawning
  , rosterEnemy = [ ("Spacefarer", "Alien Hierarchy")
                  , ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

-- No horrors faction needed, because spawned heroes land in civilian faction.
rosterSafari = Roster
  { rosterList = [ ( playerMonsterTourist
                   , [(4, 15, "monster")] )
                 , ( playerHunamConvict
                   , [(4, 3, "civilian")] )
                 , ( playerAnimalMagnificent
                   , [(7, 20, "mobile animal")] )
                 , ( playerAnimalExquisite  -- start on escape
                   , [(10, 30, "mobile animal")] )
                 , (playerHorror, []) ]  -- construction hooter; neutral
  , rosterEnemy = [ ("Alien Tourist Office", "Hunam Convict")
                  , ( "Alien Tourist Office"
                    , "Animal Magnificent Specimen Variety" )
                  , ( "Alien Tourist Office"
                    , "Animal Exquisite Herds and Packs" ) ]
  , rosterAlly = [ ( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs" )
                 , ( "Animal Magnificent Specimen Variety"
                   , "Hunam Convict" )
                 , ( "Hunam Convict"
                   , "Animal Exquisite Herds and Packs" ) ] }

rosterSafariSurvival = rosterSafari
  { rosterList = [ ( playerMonsterTourist
                       { fleaderMode = LeaderAI $ AutoLeader True True
                       , fhasUI = False }
                   , [(4, 15, "monster")] )
                 , ( playerHunamConvict
                   , [(4, 3, "civilian")] )
                 , ( playerAnimalMagnificent
                     { fleaderMode = LeaderUI $ AutoLeader True False
                     , fhasUI = True }
                   , [(7, 20, "mobile animal")] )
                 , ( playerAnimalExquisite
                   , [(10, 30, "mobile animal")] )
                 , (playerHorror, []) ] }

rosterBattle = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller }
                   , [(5, 5, "soldier hero")] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(5, 35, "mobile monster")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(5, 20, "mobile animal")] )
                 , ( playerRobot
                   , [(5, 15, "mobile robot")] ) ]
  , rosterEnemy = [ ("Spacefarer", "Alien Hierarchy")
                  , ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiDweller
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , [(5, 5, "soldier hero")] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(5, 35, "mobile monster")] )
                 , ( playerAnimal { fneverEmpty = True
                                  , fhasUI = True }
                   , [(5, 20, "mobile animal")] )
                 , ( playerRobot
                   , [(5, 15, "mobile robot")] ) ] }

rosterDefense = rosterExploration
  { rosterList = [ ( playerAntiHero
                   , [(3, 3, "hero")] )
                 , ( playerAntiMonster
                   , [] )
                 , ( playerAnimal
                   , -- Fun from the start to avoid empty initial level:
                     [ (3, 2 + d 2, "animal")  -- many, because no spawning
                     -- Optional huge battle at the end:
                     , (12, 100, "mobile animal") ] )
                 , ( playerRobot
                   , [(3, 2 + d 2, "robot")] ) ] }

cavesRaid, cavesBrawl, cavesShootout, cavesEscape, cavesZoo, cavesAmbush, cavesExploration, cavesSafari, cavesBattle :: Caves

cavesRaid = IM.fromList [(2, "caveRaid")]

cavesBrawl = IM.fromList [(3, "caveBrawl")]

cavesShootout = IM.fromList [(5, "caveShootout")]

cavesEscape = IM.fromList [(7, "caveEscape")]

cavesZoo = IM.fromList [(8, "caveZoo")]

cavesAmbush = IM.fromList [(9, "caveAmbush")]

cavesExploration = IM.fromList $
  [(1, "outermost")]
  ++ [(2, "shallow random 2")]
  ++ [(3, "caveBridge")]
  ++ [(4, "caveNoise")]
  ++ [(5, "default random")]
  ++ [(6, "default random")]
  ++ [(7, "deep random")]
  ++ [(8, "deep random")]
  ++ [(9, "deep random")]
  ++ [(10, "caveEmptyExit")]
  ++ [(11, "deep random")]
  ++ [(12, "caveNoise2")]

cavesSafari = IM.fromList [ (4, "caveSafari1")
                          , (7, "caveSafari2")
                          , (10, "caveSafari3") ]

cavesBattle = IM.fromList [(5, "caveBattle")]
