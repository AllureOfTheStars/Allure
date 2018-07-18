-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2018 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game mode definitions.
module Content.ModeKind
  ( content
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.IntMap.Strict as IM

import Content.ModeKindPlayer
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Content.ModeKind

content :: [ModeKind]
content =
  [raid, brawl, shootout, escape, zoo, ambush, crawl, crawlEmpty, crawlSurvival, dig, safari, safariSurvival, battle, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari]

raid,    brawl, shootout, escape, zoo, ambush, crawl, crawlEmpty, crawlSurvival, dig, safari, safariSurvival, battle, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari :: ModeKind

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
  , mname   = "raid (1)"
  , mfreq   = [("raid", 1), ("campaign scenario", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mdesc   = "Neptune Area Administration confirms isolated spottings of various oversize vermin in non-residential zones of the Triton moon's largest city. To put it plainly: Triton City sewers need purging. The first person to break through to the other exit will be paid 100 gold grains. The Administration strongly urges participants not to resort to violence against each other."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl (2)"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mdesc   = "\"You scoundrel! You cheated in the sewers. Come alone to the woody biosphere behind the saloon at noon, if you dare. Given that I win, I take back all your gold. Otherwise, you get the scrapping rights for the giant spaceliner's hull in orbit. Yes, it's mine, you tramp; here's the docking transmitter and the paperwork.\""
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
  , mname   = "shootout (3)"
  , mfreq   = [("shootout", 1), ("campaign scenario", 1)]
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mdesc   = "The fight crashes over to a nearby mechanized farm. Law enforcement, crippled by the ban on firearms, won't show up until only wounded and dying remain to be revived and locked up. Farm supplies, scattered around, beg to be flung at foes as improvised missiles. Intense light makes it easy to aim and to discern trajectory of soaring items (point at projectiles with the crosshair in aiming mode)."
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "escape (4)"
  , mfreq   = [("escape", 1), ("campaign scenario", 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mdesc   = "Bloodied spaceship deed in hand notwithstanding, you can reach the derelict spaceliner only via a shuttle from the Triton Spaceport across the city. After hours of being chased in the opposite direction towards the border wall, you sneak back and make a desperate dash through the very territory of the pursuing gang. Any valuables you come upon in this public park turned miscreant lair will be fair compensation for your losses, but you need to find the exit before the foes find you."
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { msymbol = 'b'
  , mname   = "zoo (5)"
  , mfreq   = [("zoo", 1), ("campaign scenario", 1)]
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mdesc   = "As justified and satisfying as setting the enemy headquarters on fire has been, it backfires when the blaze immediately spreads to the public zoo on the path to the spaceport. Crazed animals mill around while the flames ignite greenery and consume nets, cages and security equipment. Whether that's a good sign or bad, apparently nobody is willing to pursue you any more."
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
  , mname   = "ambush (6)"
  , mfreq   = [("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mdesc   = "Not even the unexplained ruin of the largest and tightest security of Neptune's spaceports will prevent you from claiming your prize. After all, you didn't take to the space to let others decide your fate. Onward!"
  }

crawl = ModeKind
  { msymbol = 'c'
  , mname   = "crawl (long)"
  , mfreq   = [("crawl", 1), ("campaign scenario", 1)]
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mdesc   = "You get stranded while looting, with utmost satisfaction, the blasted bridge of an old and extravagantly luxurious cruise liner. The inert spaceship, supposedly long deserted and barely able to sustain life support, suddenly tremors and dials all her ion engine indicators up to red overdrive. Consoles are damaged beyond repair, but the flickering space map shows the ship manoeuvre deftly off Triton orbit and away from Neptune. There is only static on all communication channels, so your plan is to scour the surounding dilapidated decks gathering all missing squad members and any valuables and get back to the spaceport the way you came, in the shuttle. You are determined to fight for the lives and freedom of your crew and not to leave the ship without taking of her wealth what is rightfully yours."
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

dig = ModeKind
  { msymbol = 'd'
  , mname   = "dig"
  , mfreq   = [("dig", 1)]
  , mroster = rosterCrawl
  , mcaves  = cavesDig
  , mdesc   = "Delve deeper!"
  }

crawlEmpty = ModeKind
  { msymbol = 'c'
  , mname   = "crawl empty"
  , mfreq   = [("crawl empty", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawl
  , mdesc   = "Enjoy the free space."
  }

crawlSurvival = ModeKind
  { msymbol = 'd'
  , mname   = "crawl survival"
  , mfreq   = [("crawl survival", 1)]
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mdesc   = "Lure the human intruders deeper and deeper."
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mdesc   = "Don't let the half-witted humans derail your operation and flee, like the puny, naked, tentacle-less beasts that they are!"
  }

defenseEmpty = ModeKind
  { msymbol = 'e'
  , mname   = "defense empty"
  , mfreq   = [("defense empty", 1)]
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawl
  , mdesc   = "Lord over."
  }

-- * Screensaver modes

screensave :: AutoLeader -> Roster -> Roster
screensave auto r =
  let f [] = []
      f ((player, initial) : rest) =
        (player {fleaderMode = LeaderAI auto}, initial) : rest
  in r {rosterList = f $ rosterList r}

screensaverRaid = raid
  { mname   = "auto-raid (1)"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterRaid
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl (2)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterBrawl
  }

screensaverShootout = shootout
  { mname   = "auto-shootout (3)"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterShootout
  }

screensaverEscape = escape
  { mname   = "auto-escape (4)"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterEscape
  }

screensaverZoo = zoo
  { mname   = "auto-zoo (5)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterZoo
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush (6)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterAmbush
  }

screensaverCrawl = crawl
  { mname   = "auto-crawl (long)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterCrawl
  }

screensaverSafari = safari
  { mname   = "auto-safari"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = -- changing leader by client needed, because of TFollow
              screensave (AutoLeader False True) rosterSafari
  }

rosterRaid, rosterBrawl, rosterShootout, rosterEscape, rosterZoo, rosterAmbush, rosterCrawl, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafari, rosterSafariSurvival, rosterBattle, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

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
                                    , fneverEmpty = False  -- loot after killing
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
                 , ( playerAntiHero { fname = "Gray Off-World Mercenary"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiDweller }
                   , [(9, 1, "scout hero"), (9, 5, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Gray Off-World Mercenary")
                  , ("Spacefarer", "Horror Den")
                  , ("Gray Off-World Mercenary", "Horror Den") ]
  , rosterAlly = [] }

rosterCrawl = Roster
  { rosterList = [ ( playerHero
                   , [(3, 3, "crawl hero")] )
                 , ( playerMonster
                   , [] )
                 , ( playerAnimal
                   , -- Optional huge battle at the end:
                     [(12, 100, "mobile animal")] )
                 , ( playerRobot
                   , [] ) ]  -- gentle introduction
  , rosterEnemy = [ ("Spacefarer", "Alien Hierarchy")
                  , ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

rosterCrawlEmpty = Roster
  { rosterList = [ ( playerHero
                   , [(1, 1, "crawl hero")] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = []
  , rosterAlly = [] }

rosterCrawlSurvival = rosterCrawl
  { rosterList = [ ( playerHero { fleaderMode =
                                    LeaderAI $ AutoLeader True False
                                , fhasUI = False }
                   , [(3, 3, "crawl hero")] )
                 , ( playerMonster
                   , [] )
                 , ( playerAnimal {fhasUI = True}
                   , -- Fun from the start to avoid empty initial level:
                     [ (3, 5 + 1 `d` 2, "animal")  -- many, because no spawning
                     -- Optional huge battle at the end:
                     , (12, 100, "mobile animal") ] )
                 , ( playerRobot
                   , [] ) ] }  -- gentle introduction

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
                    , "Animal Exquisite Herds and Packs Galore" )
                  , ( "Animal Magnificent Specimen Variety"
                    , "Hunam Convict" )
                  , ( "Hunam Convict"
                    , "Animal Exquisite Herds and Packs Galore" ) ]
  , rosterAlly = [ ( "Animal Magnificent Specimen Variety"
                   , "Animal Exquisite Herds and Packs Galore" ) ] }

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
                 , ( playerRobot {fneverEmpty = True}
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
                 , ( playerRobot {fneverEmpty = True}
                   , [(5, 15, "mobile robot")] ) ] }

rosterDefense = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , [(3, 3, "crawl hero")] )
                 , ( playerAntiMonster
                   , [] )
                 , ( playerAnimal
                   , -- Fun from the start to avoid empty initial level:
                     [ (3, 5 + 1 `d` 2, "animal")  -- many, because no spawning
                     -- Optional huge battle at the end:
                     , (12, 100, "mobile animal") ] )
                 , ( playerRobot
                   , [] ) ] }  -- gentle introduction

rosterDefenseEmpty = rosterCrawl
  { rosterList = [ ( playerAntiMonster {fneverEmpty = True}
                   , [(4, 1, "scout monster")] )
                 , (playerHorror, []) ]  -- for summoned animals
  , rosterEnemy = []
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesShootout, cavesEscape, cavesZoo, cavesAmbush, cavesCrawl, cavesDig, cavesSafari, cavesBattle :: Caves

cavesRaid = IM.fromList [(2, "caveRaid")]

cavesBrawl = IM.fromList [(3, "caveBrawl")]

cavesShootout = IM.fromList [(5, "caveShootout")]

cavesEscape = IM.fromList [(7, "caveEscape")]

cavesZoo = IM.fromList [(8, "caveZoo")]

cavesAmbush = IM.fromList [(9, "caveAmbush")]

cavesCrawl = IM.fromList $
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

cavesDig = IM.fromList $
  [ (1, "outermost")
  , (2, "shallow random 2")
  , (3, "caveEmpty") ]
  ++ zip [4, 5] (repeat "default random")
  ++ zip [6, 7, 8, 9] (repeat "deep random")
  ++ [(10, "caveNoise2")]
  ++ zip [11 .. 1000] (repeat "deep random")
  ++ zip [1001 .. 2000] (repeat "default random")

cavesSafari = IM.fromList [ (4, "caveSafari1")
                          , (7, "caveSafari2")
                          , (10, "caveSafari3") ]

cavesBattle = IM.fromList [(5, "caveBattle")]
