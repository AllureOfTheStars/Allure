-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
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
-- crowd ranged: no, fish in a barrel, less predictable and more fun inside
--   crawl, even without reaction fire

raid = ModeKind  -- mini-crawl
  { msymbol = 'r'
  , mname   = "raid (1)"
  , mfreq   = [("raid", 1), ("campaign scenario", 1)]
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mendMsg = [ (Killed, "That was unfortunate. The bill for the rescue team and for the subsequent nano medbot treatment will reach the stars. Perhaps more stealth was needed? Perhaps the items lying around the area could aid survival instead of ending up ignored or passively hoarded? Or perhaps a wise course of action would be to choose a Neptune Area Administration challenge with a lower difficulty?")
              , (Defeated, "Sadly, you got worked up in the tunnels while another team snatched the prize. Remember, you are at the Outer Frontier to gain wealth and independence through industriousness and commerce and that means clashing with competing agents, not just fighting feral nature.")
              , (Escape, "You are the first to clear a route through the sewer system. Triton City authorities will now be able to establish a perimeter and mop up the side tunnels. You collect your reward of 100 gold grains and start looking for a way to invest it profitably at this Solar System's commercial frontier, abounding in more or less (usually less) regulated opportunities.") ]
  , mdesc   = "Neptune Area Administration confirms isolated spottings of oversize vermin in non-residential zones of the Neptune's Triton moon's largest city. To put it plainly: Triton City sewers need purging. The first person to break through to the other exit will be paid 100 gold grains. The Administration \"strongly urges participants not to resort to violence against each other.\" However, no punitive consequences are specified, not even disqualification from the contest."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl (2)"
  , mfreq   = [("brawl", 1), ("campaign scenario", 1)]
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = [ (Killed, "That treacherous villain didn't honour his word and brought his friends to the fight. It would still not turn so bad if we remembered to use terrain to protect us from missiles or even completely hide our presence and if we honourably kept together to the end, at the same time preventing the overwhelming enemy forces from brutishly ganging up on our modest-sized, though valiant, squad.")
              , (Conquer, "Bringing help was a sober and prudent move that resulted in well-earned victory and a splendid trophy of a title to a real inter-planetary space vessel. Unfortunately, the treacherous foe called reinforcements at the last moment, which start to arrive just now. It may be wise to move the celebration of the victory to a more fitting area, assuming that the dignified translocation can be accomplished timely and inconspicuously.") ]
  , mdesc   = "\"You scoundrel! You cheated in the sewers. Come alone to the woody biosphere behind the saloon at noon, if you dare. Given that I win, I take back all your gold. Otherwise, you get the scrapping rights for the giant spaceliner's hull in orbit. Yes, it's mine, you tramp; here's the docking transmitter and the paperwork. The fight is to the last man standing, no evasion, no breaks for nano-healing in town.\""
  }

-- The trajectory tip is important because of tactics of scout looking from
-- behind a bush and others hiding in mist. If no suitable bushes,
-- fire once and flee into mist or behind cover. Then whomever is out of LOS
-- range or inside mist can shoot at the last seen enemy locations,
-- adjusting aim according to sounds and incoming missile trajectories.
-- If the scout can't find bushes or glass building to set a lookout,
-- the other team members are more spotters and guardians than snipers
-- and that's their only role, so a small party makes sense.
shootout = ModeKind  -- sparse ranged in daylight
  { msymbol = 's'
  , mname   = "shootout (3)"
  , mfreq   = [("shootout", 1), ("campaign scenario", 1)]
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mendMsg = [ (Killed, "This is a disgrace. How is a thuggish robbery in broad daylight even possible in a moon city that styles itself as the capital of Outer System technological innovation and commercial opportunity? Where are the municipal surveillance drones, normally so eager to eavesdrop and needlessly complicate an honest tax-free business, when one's health and wealth for once depend on their nosy presence? Speaking of drones, we could use one in this skirmish, or even just a human lookout placed in a covered but unobstructed spot. Then the rest of the squad could snipe from concealment or from a safe distance. Barring that, we would end up in a better shape even if we all hid and fired blindly. We'd listen to impact sounds and wait with ten-fold vigilance for incoming enemy missiles, in order to register their trajectories and derive hints of enemy location. Apparently, ranged combat requires a change of pace and better planning than our previous illustrious successes accustomed us to.")
              , (Conquer, "That was a good fight, with skillful application of missiles, cover and concealment. The outcome is especially commendable given the high bar of tactical proficiency. Not even professional enforcement units can routinely deduce enemy position from the trajectory of their projectiles nor by firing without line of sight and interpreting auditory cues. However, while this steep hurdle is overcome, the chase is not over yet.") ]
  , mdesc   = "The fight crashes over to a nearby mechanized farm. Law enforcement, crippled by the ban on firearms, won't show up until only wounded and dying remain to be revived and locked up. Farm supplies, scattered around, beg to be flung at foes as improvised missiles. Intense light makes it easy to aim and to discern trajectory of soaring items (point at enemy projectiles with the crosshair in aiming mode)."
  }

hunt = ModeKind  -- melee vs ranged with reaction fire in daylight
  { msymbol = 'h'
  , mname   = "hunt (4)"
  , mfreq   = [("hunt", 1), ("campaign scenario", 1)]
  , mroster = rosterHunt
  , mcaves  = cavesHunt
  , mendMsg = [ (Killed, "Next time let's try to remember we are not on a sightseeing expedition. Also, leaving concealment is risky, leaving cover is foolhardy and wandering off is deadly. Also, what was that taking pictures by the mangrove tree all about? Were you trying to immortalize your handsome faces in case our shared pool of money was not enough to revive your sorry carcasses after the defeat? Good call, because after paying the techno-medical bills we are broke and the military grade communication and reaction fire implants of the gang foot soldiers that chase us don't raise our prospects either. And we were so close to complete victory and unfathomable wealth, if only we strove to lower the difficulty of this mission instead of raising it.")  -- the guy is wrong about implants, but being wrong is plausible when the team is killed off/chased off and can't scour the battleground
    -- this is in the middle of the scenario list and the mission is not tricky, so a subtle reminder about lowering difficulty, in case the player struggles
              , (Conquer, "We chased them off, like we knew that we would. It feels nice to stick together and prevail. Now we can do no wrong just minding our business and going our way to the spaceport. We taught them a lesson, despite their superior equipment, and nobody else needs to be harmed while we take possession of our rightful property, the glorious spaceship in Triton's orbit.") ]
  , mdesc   = "Who is the hunter and who is the prey? The only criterion is remaining conscious versus becoming incapacitated when the chase ends."
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "escape (5)"
  , mfreq   = [("escape", 1), ("campaign scenario", 1)]
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mendMsg = [ (Killed, "Somebody must have tipped the gang guards off. However, us walking along a lit trail, yelling, could have been a contributing factor. Also, it's worth noting that the torches prepared for this assault are best used as thrown makeshift flares. On the other hand, equipping a lit torch makes one visible in the dark, regrettably but not quite unexpectedly. Lastly, the goal of this foray was to find the exit back to the city, marked by a yellow '>' sign, and to gather some treasure along the way, but not to bedevil every local evildoer, as much as they do deserve it.")
              , (Conquer, "It was enough to reach the escape area, namely the exit tunnel from the park marked by yellow '>' symbol. Spilling that much blood was risky and unnecessary. Having said that --- impressive indeed.")
              , (Escape, "Congratulations, you took your revenge and it's heavy in your pockets.") ]
  , mdesc   = "Bloodied spaceship deed in hand notwithstanding, you can reach the derelict spaceliner only via a shuttle from the Central Triton Spaceport across the city. After hours of being chased in the opposite direction towards the border wall, you sneak back and make a desperate dash through the very den of the pursuing gang. Any valuables you come upon in this public park turned miscreant lair will be fair compensation for your losses, but you need to find the exit before the foes find you. Reign in your wrath and don't unnecessarily attack your tormentors. Foiling their plans by eluding them will be revenge enough."
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { msymbol = 'b'
  , mname   = "zoo (6)"
  , mfreq   = [("zoo", 1), ("campaign scenario", 1)]
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mendMsg = [ (Killed, "Against such an onslaught, only clever positioning, use of terrain and patient vigilance gives any chance of survival.")
              , (Conquer, "That was a grim harvest. The city is safe again. So are your precious selves, with nothing and no one blocking your way to the spaceport any more.") ]
  , mdesc   = "As justified and satisfying as setting the enemy headquarters on fire has been, it backfires when the blaze immediately spreads to the public zoo on the path to the spaceport. Crazed animals mill around while the flames ignite greenery and consume nets, cages and security equipment. Whether that's a good sign or bad, apparently nobody is willing to pursue you any more. You are on your own, having to completely clean up the area, up to the last lurking predator, in order to safely move through."
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
  , mendMsg = [ (Killed, "You turned out to be the prey, this time, not the hunter. In fact, you are not even in the hunters' league. And it's hard to operate a space dock while being chased round a spaceport. When fighting against such odds, passively waiting for enemy to spring a trap is to no avail, because a professional team can sneak in darkness and ambush the ambushers. Granted, good positioning is crucial, so that each squad member can overwatch the battlefield and fire opportunistically, using the recently recovered mil-grade communication equipment. However, there is no hope without active scouting, throwing lit objects and probing suspect areas with missiles while paying attention to sounds. And that may still not be enough.")
              , (Conquer, "The new communication equipment, enabling simultaneous ranged attacks with indirect aiming, apparently proved effective beyond expectations. With the mercenaries gone and nobody else having the slightest wish to interfere, the shuttle to the space cruiser at orbit is easy to launch at last. Now is your moment of glory. Now your turbulent adventure ends and the boring life of space cruiser scrap parts supplier or, as it may be, of a refurbished giant space liner operator, commences.") ]
  , mdesc   = "Not even the unexplained ruin of the Central Triton Spaceport will prevent you from claiming the prize awaiting you at the orbit. After all, you didn't take to the stars to let others decide your fate. There is still no news coverage from what was the largest and tightest security facility in the whole Neptune Area. Without waiting for explanations nor for the personnel to return, you creep along the abandoned booths, scouting for any airlock with a shuttle still attached."  -- this is the only scenario with no objective specified, to give a bit of suspense, misdirection and mystery until the first win (death gives a hint only); being the last of the small scenarios, it won't scare off new players
  }

crawl = ModeKind
  { msymbol = 'c'
  , mname   = "crawl (long)"
  , mfreq   = [("crawl", 1), ("campaign scenario", 1)]
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, "It was not supposed to end this way. Perhaps more stealth was in order? Perhaps the gathered items should be used for survival instead of hoarded? Or perhaps the challenge, chosen freely but without awareness of the grisly difficulty, was insurmountable and lost from the very start? Nobody is going to find out, even if humans ever set their feet here again and prevail, another time, another way.")
              , (Escape, "The shuttle doors close behind, docking clamps grind in farewell and the giant rotating disc slowly tumbles away in rear view. You feel at once a relief and a sense of loss. This is not finished. You are changed forever, but you know nothing. You've heard the call, but you made no answer. You came for petty change, found a treasure beyond comprehension, then barely escaped with your life as the prize.\nAnd nobody will believe you at home. But you don't need their understanding any more. You have enough money to heal, regroup, track the ship down and try again. It's your personal space cruiser, after all, with a world of its own, inside.") ]
  , mdesc   = "You are busy looting, with utmost satisfaction, the blasted bridge of an old and extravagantly luxurious cruise liner. Suddenly, the inert spaceship, supposedly long deserted and barely able to sustain life support, tremors and dials her fusion engines up to red overdrive. The space map flickering among the irreversibly damaged consoles shows the ship manoeuvre deftly off Triton orbit and purposefully climb the Neptune's gravity well. There's no way to control the ship and static floods all communication channels. You decide to scour the nearby dilapidated decks for missing squad members and get back to the spaceport the way you came, in your shuttle. However, you are determined not to leave the ship without taking at least a portion of the wealth that is rightfully yours."
 -- later, when the player can visit other level sections: you turn on the deck status list display and notice that most levels are fully pressurized, including the complete autonomous slice of the disc that includes the bridge deck
  }

safari = ModeKind  -- Easter egg available only via screensaver
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
  , mdesc   = "Odds are stacked for those that ally with the strongest."
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
                   , [(7, 7, "soldier hero")] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , [(7, 1, "scout hero"), (7, 6, "ambusher hero")] )
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
                   , [(9, 1, "scout hero"), (9, 6, "ambusher hero")] )
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
