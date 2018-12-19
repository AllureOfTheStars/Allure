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

import Content.ModeKindPlayer
import Game.LambdaHack.Common.Dice
import Game.LambdaHack.Common.Misc
import Game.LambdaHack.Content.CaveKind (CaveKind)
import Game.LambdaHack.Content.ModeKind

content :: [ModeKind]
content =
  [raid, brawl, shootout, hunt, escape, zoo, ambush, crawl, crawlEmpty, crawlSurvival, dig, see, safari, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari]

raid,    brawl, shootout, hunt, escape, zoo, ambush, crawl, crawlEmpty, crawlSurvival, dig, see, safari, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverCrawl, screensaverSafari :: ModeKind

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
  , mendMsg = [ (Killed, "That was unfortunate. The bill for the rescue team and for the subsequent nano medbot treatment will be beyond the pale. Perhaps more stealth was needed? Perhaps the items lying around the area could aid survival instead of ending up ignored or passively hoarded? Or perhaps a wise course of action would be to choose a Neptune Area Administration challenge with a lower difficulty?")
              , (Defeated, "Sadly, you got worked up in the tunnels while another team snatched the prize. Remember, you are at the Outer Frontier to gain wealth and independence through industriousness and commerce and that means clashing with competing agents, not just fighting feral nature.")
              , (Escape, "You are the first to clear a route through the sewer system. Triton City authorities will now be able to establish a perimeter and mop up the side tunnels. You collect your reward of 100 gold grains and start looking for a way to invest it profitably at this Solar System's commercial frontier, abounding in more or less (usualy less) regulated opportunities.") ]
  , mdesc   = "Neptune Area Administration confirms isolated spottings of oversize vermin in non-residential zones of the Triton moon's largest city. To put it plainly: Triton City sewers need purging. The first person to break through to the other exit will be paid 100 gold grains. The Administration \"strongly urges participants not to resort to violence against each other.\" However, no punitive consequences are specified, not even disqualification from the contest."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl (2)"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = []
  , mdesc   = "\"You scoundrel! You cheated in the sewers. Come alone to the woody biosphere behind the saloon at noon, if you dare. Given that I win, I take back all your gold. Otherwise, you get the scrapping rights for the giant spaceliner's hull in orbit. Yes, it's mine, you tramp; here's the docking transmitter and the paperwork. The fight is to the last man standing, no break, no exit.\""
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
  , mendMsg = []
  , mdesc   = "The fight crashes over to a nearby mechanized farm. Law enforcement, crippled by the ban on firearms, won't show up until only wounded and dying remain to be revived and locked up. Farm supplies, scattered around, beg to be flung at foes as improvised missiles. Intense light makes it easy to aim and to discern trajectory of soaring items (point at projectiles with the crosshair in aiming mode)."
  }

hunt = ModeKind  -- melee vs ranged with reaction fire in daylight
  { msymbol = 'h'
  , mname   = "hunt (4)"
  , mfreq   = [("hunt", 1), ("campaign scenario", 1)]
  , mroster = rosterHunt
  , mcaves  = cavesHunt
  , mendMsg = []
  , mdesc   = "Who is the hunter and who is the prey? The only criterion is being conscious or incapacited when the chase ends. As far as the eye can see, the landscape is desolate."
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "escape (5)"
  , mfreq   = [("escape", 1), ("campaign scenario", 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mendMsg = []
  , mdesc   = "Bloodied spaceship deed in hand notwithstanding, you can reach the derelict spaceliner only via a shuttle from the Central Triton Spaceport across the city. After hours of being chased in the opposite direction towards the border wall, you sneak back and make a desperate dash through the very territory of the pursuing gang. Any valuables you come upon in this public park turned miscreant lair will be fair compensation for your losses, but you need to find the exit before the foes find you. Reign in your wrath, don't chase your foes, foiling their plans by eluding them will be revenge enough."
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { msymbol = 'b'
  , mname   = "zoo (6)"
  , mfreq   = [("zoo", 1), ("campaign scenario", 1)]
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mendMsg = []
  , mdesc   = "As justified and satisfying as setting the enemy headquarters on fire has been, it backfires when the blaze immediately spreads to the public zoo on the path to the spaceport. Crazed animals mill around while the flames ignite greenery and consume nets, cages and security equipment. Whether that's a good sign or bad, apparently nobody is willing to pursue you any more. However, now you are on your own, having to completely clean up the area, up to the last lurking predator, in order to safely move through."
  }

-- The tactic is to sneak in the dark, highlight enemy with thrown torches
-- (and douse thrown enemy torches with blankets) and only if this fails,
-- actually scout using extended noctovision.
-- With reaction fire, larger team is more fun.
--
-- For now, while we have no shooters with timeout, massive ranged battles
-- without reaction fire don't make sense, because then usually only one hero
-- shoots (and often also scouts) and others just gather ammo.
ambush = ModeKind  -- dense ranged with reaction fire vs melee at night
  { msymbol = 'm'
  , mname   = "ambush (7)"
  , mfreq   = [("ambush", 1), ("campaign scenario", 1)]
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mendMsg = []
  , mdesc   = "Not even the unexplained ruin of the Central Triton Spaceport will prevent you from claiming the prize awaiting you at the orbit. After all, you didn't take to the space to let others decide your fate. There is still no news coverage from what was the largest and tightest security facitily in the whole Neptune Area. Without waiting for explanations nor for the personnel to return, you creep along the abandoned booths, scouting for any airlock with a shuttle still attached."  -- this is the only scenario with no objective specified, to give a bit of suspense, misdirection and mystery until the first win; being the last of the small scenarios, it won't scare off new players
  }

crawl = ModeKind
  { msymbol = 'c'
  , mname   = "crawl (long)"
  , mfreq   = [("crawl", 1), ("campaign scenario", 1)]
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, "It was not supposed to end this way. Perhaps more stealth was in order? Perhaps the gathered items should be used for survival instead of hoarded? Or perhaps the challenge, chosen freely but without awareness of the grisly difficulty, was insurmountable and lost from the very start? Nobody is going to find out, even if humans ever set their feet here again and prevail, another time, another way.")
              , (Escape, "The shuttle doors close behind, docking clamps grind in farewell and the giant rotating disc slowly tumbles away in rear view. You feel at once a relief and a sense of loss. This is not finished. You are changed forever, but you know nothing. You've heard the call, but you made no answer. You came for petty change, found a treasure beyond comprehension, then barely escaped with your life as the prize.\nAnd nobody will believe you at home. But you don't need their understanding any more. You have enough money to heal, regroup, track the ship down and try again. It's your personal space cruiser, after all, with a world of its own, inside.") ]
  , mdesc   = "You get stranded while looting, with utmost satisfaction, the blasted bridge of an old and extravagantly luxurious cruise liner. The inert spaceship, supposedly long deserted and barely able to sustain life support, suddenly tremors and dials her ion engines up to red overdrive. The space map flickering among the irreversibly damaged consoles shows the ship manoeuvre deftly off Triton orbit and purposefully climb the Neptune's gravity well. There's no way to control the ship and static floods all communication channels. You decide to scour the surrounding dilapidated decks for missing squad members and get back to the spaceport the way you came, in your shuttle. However, you are determined not to leave the ship without taking at least a portion of the wealth that is rightfully yours. You bloodily fought for every grain of it. You look closer at your surroundings."
  }

safari = ModeKind  -- easter egg available only via screensaver
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [("safari", 1)]
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mendMsg = []
  , mdesc   = "\"In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the topmost level.\" This is a VR recording recovered from an alien nest debris."
  }

-- * Testing modes

dig = ModeKind
  { msymbol = 'd'
  , mname   = "dig"
  , mfreq   = [("dig", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesDig
  , mendMsg = []
  , mdesc   = "Delve deeper!"
  }

see = ModeKind
  { msymbol = 'a'
  , mname   = "see"
  , mfreq   = [("see", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesSee
  , mendMsg = []
  , mdesc   = "See all!"
  }

crawlEmpty = ModeKind
  { msymbol = 'c'
  , mname   = "crawl empty"
  , mfreq   = [("crawl empty", 1)]
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mdesc   = "Enjoy the free space."
  }

crawlSurvival = ModeKind
  { msymbol = 'd'
  , mname   = "crawl survival"
  , mfreq   = [("crawl survival", 1)]
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mdesc   = "Lure the human intruders deeper and deeper."
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [("safari survival", 1)]
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mendMsg = []
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [("battle", 1)]
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mendMsg = []
  , mdesc   = "Odds are stacked against those that unleash the horrors of abstraction."
  }

battleDefense = ModeKind
  { msymbol = 'f'
  , mname   = "battle defense"
  , mfreq   = [("battle defense", 1)]
  , mroster = rosterBattleDefense
  , mcaves  = cavesBattle
  , mendMsg = []
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [("battle survival", 1)]
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mendMsg = []
  , mdesc   = "Odds are stacked for those that breathe mathematics."
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [("defense", 1)]
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mdesc   = "Don't let the half-witted humans derail your operation and flee, like the puny, naked, tentacle-less beasts that they are!"
  }

defenseEmpty = ModeKind
  { msymbol = 'e'
  , mname   = "defense empty"
  , mfreq   = [("defense empty", 1)]
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
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

screensaverHunt = hunt
  { mname   = "auto-hunt (4)"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterHunt
  }

screensaverEscape = escape
  { mname   = "auto-escape (5)"
  , mfreq   = [("starting", 1), ("starting JS", 1), ("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterEscape
  }

screensaverZoo = zoo
  { mname   = "auto-zoo (6)"
  , mfreq   = [("no confirms", 1)]
  , mroster = screensave (AutoLeader False False) rosterZoo
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush (7)"
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

rosterRaid, rosterBrawl, rosterShootout, rosterHunt, rosterEscape, rosterZoo, rosterAmbush, rosterCrawl, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafari, rosterSafariSurvival, rosterBattle, rosterBattleDefense, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

rosterRaid = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiHeroShort}
                   , [(3, 1, "hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fhiCondPoly = hiHeroShort }
                   , [(3, 1, "hero")] )
                 , ( playerAnimal  -- starting over escape
                   , [(3, 2, "animal")] )
                 , ( playerRobot
                   , [(3, 1, "robot")] )
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
                                , fhiCondPoly = hiHeroMedium }
                   , [(4, 3, "hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(4, 3, "hero")] )
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
                                , fhiCondPoly = hiHeroMedium }
                   , [(5, 1, "scout hero"), (5, 2, "ranger hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(5, 1, "scout hero"), (5, 2, "ranger hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterHunt = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(7, 10, "soldier hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(7, 1, "scout hero"), (7, 5, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterEscape = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiHeroMedium}
                   , [(9, 1, "scout hero"), (9, 2, "escapist hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False  -- start on escape
                                    , fneverEmpty = False  -- loot after killing
                                    , fhiCondPoly = hiHeroMedium }
                   , [(9, 1, "scout hero"), (9, 7, "ambusher hero")] )
                 , (playerHorror, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterZoo = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , [(11, 5, "soldier hero")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(11, 100, "mobile animal")] )
                 , (playerHorror, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , [(13, 1, "scout hero"), (13, 5, "ambusher hero")] )
                 , ( playerAntiHero { fname = "Gray Off-World Mercenary"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(13, 12, "soldier hero")] )
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
                     [(15, 100, "mobile animal")] )
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
                 , (playerHorror, []) ]  -- for spawned and summoned monsters
  , rosterEnemy = []
  , rosterAlly = [] }

rosterCrawlSurvival = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , [(3, 3, "crawl hero")] )
                 , ( playerMonster
                   , [] )
                 , ( playerAnimal {fhasUI = True}
                   , -- Fun from the start to avoid empty initial level:
                     [ (3, 5 + 1 `d` 2, "animal")  -- many, because no spawning
                     -- Optional huge battle at the end:
                     , (15, 100, "mobile animal") ] )
                 , ( playerRobot
                   , [] ) ] }  -- gentle introduction

-- No horrors faction needed, because spawned heroes land in civilian faction.
rosterSafari = Roster
  { rosterList = [ ( playerMonsterTourist
                   , [(5, 15, "monster")] )
                 , ( playerHunamConvict
                   , [(5, 3, "civilian")] )
                 , ( playerAnimalMagnificent
                   , [(10, 20, "mobile animal")] )
                 , ( playerAnimalExquisite  -- start on escape
                   , [(15, 30, "mobile animal")] )
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
                   , [(5, 15, "monster")] )
                 , ( playerHunamConvict
                   , [(5, 3, "civilian")] )
                 , ( playerAnimalMagnificent
                       { fleaderMode = LeaderUI $ AutoLeader True False
                       , fhasUI = True }
                   , [(10, 20, "mobile animal")] )
                 , ( playerAnimalExquisite
                   , [(15, 30, "mobile animal")] )
                 , (playerHorror, []) ] }

rosterBattle = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , [(10, 5, "soldier hero")] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(10, 35, "mobile monster")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(10, 20, "mobile animal")] )
                 , ( playerRobot {fneverEmpty = True}
                   , [(10, 15, "mobile robot")] ) ]
  , rosterEnemy = [ ("Spacefarer", "Alien Hierarchy")
                  , ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

rosterBattleDefense = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , [(10, 5, "soldier hero")] )
                 , ( playerMonster { fneverEmpty = True
                                   , fhasUI = True }
                   , [(10, 35, "mobile monster")] )
                 , ( playerAnimal {fneverEmpty = True}
                   , [(10, 20, "mobile animal")] )
                 , ( playerRobot {fneverEmpty = True}
                   , [(10, 15, "mobile robot")] ) ] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , [(10, 5, "soldier hero")] )
                 , ( playerMonster {fneverEmpty = True}
                   , [(10, 35, "mobile monster")] )
                 , ( playerAnimal { fneverEmpty = True
                                  , fhasUI = True }
                   , [(10, 20, "mobile animal")] )
                 , ( playerRobot {fneverEmpty = True}
                   , [(10, 15, "mobile robot")] ) ] }

rosterDefense = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , [(3, 3, "crawl hero")] )
                 , ( playerAntiMonster
                   , [] )
                 , ( playerAnimal
                   , -- Fun from the start to avoid empty initial level:
                     [ (3, 5 + 1 `d` 2, "animal")  -- many, because no spawning
                     -- Optional huge battle at the end:
                     , (15, 100, "mobile animal") ] )
                 , ( playerRobot
                   , [] ) ] }

rosterDefenseEmpty = rosterCrawl
  { rosterList = [ ( playerAntiMonster {fneverEmpty = True}
                   , [(4, 1, "scout monster")] )
                 , (playerHorror, []) ]  -- for spawned and summoned animals
  , rosterEnemy = []
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesShootout, cavesHunt, cavesEscape, cavesZoo, cavesAmbush, cavesCrawl, cavesCrawlEmpty, cavesDig, cavesSee, cavesSafari, cavesBattle :: Caves

cavesRaid = [([3], ["caveRaid"])]

cavesBrawl = [([4], ["caveBrawl"])]

cavesShootout = [([5], ["caveShootout"])]

cavesHunt = [([7], ["caveHunt"])]

cavesEscape = [([9], ["caveEscape"])]

cavesZoo = [([11], ["caveZoo"])]

cavesAmbush = [([13], ["caveAmbush"])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([1], ["caveOutermost"])
  , ([2], ["caveShallowRogue"])
  , ([3], ["caveBridge"])
  , ([4], ["caveNoise"])
  , ([5], ["caveRogue"])
  , ([6], ["caveArena"])
  , ([7], ["caveResidential"])
       -- reversed order, to match @reverse@ later on
  , ([8], ["caveLaboratory"])
  , ([11, 10, 9], ["default random", "default random", "caveMuseum"])
  , ([12], ["caveExit"])
  , ([14, 13], ["default random", "caveCasino"])
  , ([15], ["cavePower"]) ]

-- Reversed to have the last cave small and exactly in the middle
-- of the screen.
cavesCrawl = reverse listCrawl

cavesCrawlEmpty = reverse $
  map (\(ns, grps) ->
        (ns, if grps == ["caveBridge"] then ["caveShallowRogue"] else grps))
      listCrawl

renumberCaves :: Int -> ([Int], [GroupName CaveKind])
              -> ([Int], [GroupName CaveKind])
renumberCaves offset (ns, l) = (map (+ offset) ns, l)

cavesDig = reverse $ concat $ zipWith (map . renumberCaves)
                                      [0, 15 ..]
                                      (replicate 100 listCrawl)

cavesSee = let numberCaves n c = ([n], [c])
           in reverse $ zipWith numberCaves [1..]
              $ concatMap (replicate 8) allCaves

allCaves :: [GroupName CaveKind]
allCaves =
  [ "caveRaid", "caveBrawl", "caveShootout", "caveHunt", "caveEscape", "caveZoo"
  , "caveAmbush"
  , "caveRogue", "caveResidential", "caveLaboratory", "caveArena", "caveCasino"
  , "caveMuseum", "caveNoise", "cavePower", "caveOutermost", "caveExit" ]

cavesSafari = reverse [ ([5], ["caveSafari1"])
                      , ([10], ["caveSafari2"])
                      , ([15], ["caveSafari3"]) ]

cavesBattle = [([10], ["caveBattle"])]
