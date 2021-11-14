-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Definitions of game mode kinds.
module Content.ModeKind
  ( -- * Group name patterns
    groupNamesSingleton, groupNames
  , -- * Content
    content
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
  , pattern GAUNTLET, pattern RAID, pattern BRAWL, pattern LONG, pattern CRAWL, pattern FOGGY, pattern SHOOTOUT, pattern PERILOUS, pattern HUNT, pattern NIGHT, pattern ESCAPE, pattern BURNING, pattern ZOO, pattern RANGED, pattern AMBUSH, pattern SAFARI, pattern DIG, pattern SEE, pattern SHORT, pattern CRAWL_EMPTY, pattern CRAWL_SURVIVAL, pattern SAFARI_SURVIVAL, pattern BATTLE, pattern BATTLE_DEFENSE, pattern BATTLE_SURVIVAL, pattern DEFENSE, pattern DEFENSE_EMPTY
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Content.CaveKind hiding (content, groupNames, groupNamesSingleton)
import Content.FactionKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor
import Game.LambdaHack.Content.CaveKind (CaveKind, pattern DEFAULT_RANDOM)
import Game.LambdaHack.Content.FactionKind (Outcome (..))
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Defs
import Game.LambdaHack.Definition.DefsInternal

-- * Group name patterns

groupNamesSingleton :: [GroupName ModeKind]
groupNamesSingleton =
       [GAUNTLET, RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, ESCAPE, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY]

pattern GAUNTLET, RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, ESCAPE, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY :: GroupName ModeKind

groupNames :: [GroupName ModeKind]
groupNames = []

pattern GAUNTLET = GroupName "gauntlet"
pattern RAID = GroupName "raid"
pattern BRAWL = GroupName "brawl"
pattern LONG = GroupName "long crawl"
pattern CRAWL = GroupName "crawl"
pattern FOGGY = GroupName "foggy shootout"
pattern SHOOTOUT = GroupName "shootout"
pattern PERILOUS = GroupName "perilous hunt"
pattern HUNT = GroupName "hunt"
pattern NIGHT = GroupName "night escape"
pattern ESCAPE = GroupName "escape"
pattern BURNING = GroupName "burning zoo"
pattern ZOO = GroupName "zoo"
pattern RANGED = GroupName "ranged ambush"
pattern AMBUSH = GroupName "ambush"
pattern SAFARI = GroupName "safari"
pattern DIG = GroupName "dig"
pattern SEE = GroupName "see"
pattern SHORT = GroupName "short"
pattern CRAWL_EMPTY = GroupName "crawlEmpty"  -- only the first word matters
pattern CRAWL_SURVIVAL = GroupName "crawlSurvival"
pattern SAFARI_SURVIVAL = GroupName "safariSurvival"
pattern BATTLE = GroupName "battle"
pattern BATTLE_DEFENSE = GroupName "battleDefense"
pattern BATTLE_SURVIVAL = GroupName "battleSurvival"
pattern DEFENSE = GroupName "defense"
pattern DEFENSE_EMPTY = GroupName "defenseEmpty"

-- * Content

content :: [ModeKind]
content =
  [gauntlet, raid, brawl, crawl, shootout, hunt, escape, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverGauntlet, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverSafari]

gauntlet,    raid, brawl, crawl, shootout, hunt, escape, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverGauntlet, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverSafari :: ModeKind

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

gauntlet = ModeKind
  { mname   = "gauntlet (tutorial, 1)"
  , mfreq   = [(GAUNTLET, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mattract = False
  , mroster = rosterGauntlet
  , mcaves  = cavesGauntlet
  , mendMsg = [ (Killed, "That was unfortunate. Perhaps the growing wave of rogue robots indeed could not be stemmed. The bill for the nano medbot treatment will reach the stars once the checkout clock alerts authorities and a full drone city sweep (another bill) recovers your remains.\nBut was it really impossible to run a few dozen meters into the tunnel and trigger the red alarm console? Perhaps one person could distract the replicants, while the other searched for a passage around them? Or was the challenge just too difficult and could the difficulty have been lowered otherwise?")  -- hint, hint
              , (Conquer, "Not alerting the authorities was a choice that paid off handsomely. You can now collect the semiconductors parts from the infected robots all for yourself. Nobody needs to know. Replicant scrap gives a hefty premium on the darknet, even counting in anonymizing intermediaries. This will make for an enormous PTSD-shedding party.")
              , (Escape, "The moment you press the red button, robots get distracted and disperse towards the walls, scanning. You disengage and watch fascinated, but you don't get to see what happens next. The security force contacts you and hauls you up the chute, where you are sternly reprimanded and, unexpectedly, released scot-free after being sworn to silence. Oh well, saving own life is definitely worth more than whatever makes the officer that shooed you off so excited.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Two heroes vs. Spawned enemies"
      , "* Incapacitate all enemies faster than they can spawn"
      , "* Or find and activate the red alarm console ASAP"
      ]
  , mdesc   = T.intercalate "\n"
      [ "Taking this shortcut may not have been the best idea. The tunnel's entry chute was much deeper than the net search suggested and the landing turned out on a pile of robot scrap that dampened the shock, but promptly started moving on its own, shooting rays of light down the corridor and drowning the area in clanking and echoes."
      , "Such deep tunnels serve exclusively as emergency secondary connections between Triton City forges, farms and population centers. They are devoid of amenities and normally unused except by lazy maintenance crews storing and then leaving behind defunct machinery and leftover spare parts. No chance anybody could be contacted from here, except through a red alarm console, mandatory per every 100m of city corridors."
      , "Gasping huddled behind a hardware stack you appraise your misery. That must be the Internet of Things gone wrong, the notorious Robot Replicants, viruses that take over robots, whether turned on or off, modify, repair, rebuild, clone, merge and divide, spawn and multiply. You've heard about them but you weren't one of the lucky few that experienced the thrill of seeing them. And survived." ]
  , mreason = "This is a simple introductory tutorial adventure. It teaches (avoiding) combat, aggression, audacity, speed and multiple good endings. And dying."
  , mhint   = "Speaking plainly, the two good endings are escaping thanks to the alarm console and killing off all the infestation. The former requires a long and nerve-wrecking jog, while being harassed by robots, to the other end of the tunnel. The latter is hard, because replicants initially spawn faster than they can be killed and they keep spawning for as long as a single survivor hides under a crate somewhere. Killing off all robots results in higher score, unless it required much more time.\nWhen meleeing, it's best to keep all characters together, but when running past enemies, a sole sprinter will have a higher speed, though breaking through an unexpected wall of enemies may prove impossible for a single brave. If any of the endings seem unattainable, it may be wise to lower game  difficulty level from main menu and/or return after trying out subsequent game modes, battle-hardened and full of mastery."
  }

raid = ModeKind
  { mname   = "raid (tutorial, 2)"
  , mfreq   = [(RAID, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mattract = False
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mendMsg = [ (Killed, "The search&rescue and nano-medical revival fees are going to kill you (anew). Perhaps more stealth was needed? Perhaps the items lying around the area could aid survival instead of ending up ignored or passively hoarded? Or perhaps a wise course of action would be to choose a Neptune Economic Area Administration challenge with a lower difficulty?")
              , (Defeated, "Sadly, you got worked up in the tunnels while another team snatched the prize. Remember, you are at the Outer Frontier to gain wealth and independence through industriousness and commerce. This pits you in a fight against competing agents, not just against the feral nature.")
              , (Escape, "You are the first to clear a route through the sewer system. Triton City authorities will now be able to establish a perimeter and mop up the side tunnels. You collect your reward of 100 gold grains and start looking for a way to invest it profitably on this Solar System's commercial frontier, abounding in more or less (usually less) regulated opportunities.\nAfter some thought you decide to start by splurging on genetic enhancement for your team. The effects won't be visible at once, but you have to plan ahead, having just made fresh enemies.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Two heroes vs. Competition and Spawned enemies"
      , "* Gather gold"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "Neptune Economic Area Administration confirms isolated spottings of oversize vermin in non-residential zones of the Neptune's Triton moon's largest city. To put it plainly: Triton City sewers need purging. The first person to break through to the other exit will be paid 100 gold grains. The Administration \"strongly urges participants not to resort to violence against each other.\" However, no punitive consequences are specified, not even disqualification from the contest."
  , mreason = "In addition to initiating the game plot, this adventure teaches treasure gathering and item use, looking after the shared inventory stash and dealing with many enemy factions at once. Combat, however, is not a focus, so relax, explore, gather loot, find the exit and escape. With some luck, you won't even need to fight anything."
  , mhint   = "You can't use gathered items in your next encounters, so trigger any consumables at will, in particular the throwaway electronic chips as common as pebbles on the muddy sewer floors.\nFeel free to scout with only one of the heroes and keep the other one immobile, e.g., standing guard over the squad's shared inventory stash. If in grave danger, retreat with the scout to join forces with the guard. The more gold collected and the faster the victory, the higher your score in this encounter."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { mname   = "brawl (tutorial, 3)"
  , mfreq   = [(BRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mattract = False
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = [ (Killed, "That treacherous villain didn't honour his word and brought his friends to the fight. It would still not turn so bad if we remembered to use terrain to protect us from missiles or even completely hide our presence and if we honourably kept together to the end, at the same time preventing the overwhelming enemy forces from brutishly ganging up on our modest-sized, though valiant, squad.\nHaving to repurchase the genetic therapy is the most painful result. If repeated, that's going to send you broke and in shame to Earth, to start collecting your Basic Income.")
              , (Conquer, "Bringing help was a sober and prudent move that resulted in well-earned victory and a splendid trophy of a title to a real inter-planetary space vessel. Unfortunately, the treacherous foe called reinforcements at the last moment, a new wave arriving even now. It may be wise to move the celebration of the victory to a more fitting area, assuming that the dignified translocation can be accomplished timely and inconspicuously.") ]
  , mrules  = T.intercalate "\n"
      [ "* Two levels"
      , "* Three heroes vs. Six human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Last evening: \"You scoundrel! You cheated in the sewers, fighting two against one. Come alone to the woody biosphere behind the saloon at noon, if you dare. Given that I win, I take back all your gold. Otherwise, you get the scrapping rights for the giant spaceliner's hull in orbit.\nYes, it's mine, you tramp; here's the docking transmitter and the paperwork. The fight is to the last man standing, no evasion, no breaks for nano-healing in town.\"\nIt's noon now."
  , mreason = "In addition to advancing the game plot, this encounter trains melee, squad formation, stealth and stairs use. On each level separately, the battle is symmetric: three vs three. Similar are also goals (incapacitate all enemies) and squad capabilities (only the pointman moves, while all others either melee or wait). Observe and mimic the enemies. If you can't see an enemy that apparently can see you, in reversed circumstances you would have the same advantage. Savour the relative fairness --- you won't find any in the main crawl adventure that follows."
  , mhint   = "Run a short distance with Shift or LMB, switch the pointman with Tab, repeat. In open terrain, if you keep distance between teammates, this resembles the leap frog infantry tactics. For best effects, end each sprint behind a cover or concealment.\nOnce you clear a level, descend by bumping into stairs. Use Tab to switch to remaining heroes until all gather on the new level.\nIf you get beaten repeatedly, try using all consumables you find, particularly the vials that collect healing extracts abounding in this rich biosphere. Ponder the hints from the defeat message, in particular the one about keeping your party together once the opponents are spotted. However, if you want to discover a winning tactics on your own, make sure to ignore any such tips until you succeed."
  }

crawl = ModeKind
  { mname   = "long crawl (main)"
  , mfreq   = [(LONG, 1), (CRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, "It was not supposed to end this way. Perhaps more stealth was in order? Perhaps foes that didn't carry any key resources for your survival nor escape could have been eluded and ignored? Perhaps the gathered items should be used for survival instead of hoarded? Or perhaps the challenge, chosen freely but without awareness of the grisly difficulty, was insurmountable and lost from the very start?")
              , (Escape, "The shuttle doors close behind, docking clamps grind in farewell and the giant rotating disc slowly tumbles away in rear view. You feel at once a relief and a sense of loss. This is not finished. You are changed forever, but you know nothing. You've heard the call, but you made no answer. You came for petty change, found a treasure beyond comprehension, then barely escaped with your life as the prize.\nAnd nobody will believe you at home. But you don't need their understanding any more. You have enough money to heal, regroup, track the ship down and try again. It's your personal space cruiser, after all, with a world of its own, inside.") ]
  , mrules  = T.intercalate "\n"
      [ "* Many levels, some requiring tools to access"
      , "* Three heroes vs. Spawned enemies"
      , "* Gather gold, gems and stimpacks"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "You are busy looting, with utmost satisfaction, the blasted bridge of an old and extravagantly luxurious cruise liner.\nSuddenly, the inert spaceship, supposedly long deserted and barely able to sustain life support, tremors and dials her fusion engines up to red overdrive. The space map flickering among the irreversibly damaged consoles shows the ship manoeuvre deftly off Triton orbit and purposefully climb the Neptune's gravity well. There's no way to control the ship and static floods all communication channels.\nYou decide to scour the nearby dilapidated decks for missing squad members, this time sending them in pairs and mapping the area properly, and then get back to the spaceport the way you came, in your shuttle. However, you are determined not to leave the ship without taking at least a portion of the wealth that is rightfully yours."
  , mreason = "This is the main, longest and most replayable scenario of the game. The fundamental goal is the survival of your crew. Sub-goals will present themselves as you take in newly visited spaceship decks and figure out ways to reach those that are presently cut off."
  , mhint   = "If you keep dying, attempt the subsequent adventures as a breather (perhaps at lowered difficulty). They fill the gaps in the plot and teach particular skills that may come in handy and help you discover new tactics of your own or come up with a strategy for staving off the attrition. On the other hand, experimenting with the initial adventures, e.g., to obtain a higher score, may open to you more efficient ways of solving the puzzles."
 -- later, when the player can visit other level sections: you turn on the deck status list display and notice that most levels are fully pressurized, including the complete autonomous slice of the disc that includes the bridge deck
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
  { mname   = "foggy shootout (4)"
  , mfreq   = [(FOGGY, 1), (SHOOTOUT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mendMsg = [ (Killed, "This is a disgrace. How is a thuggish robbery in broad daylight even possible in a moon city that styles itself as the capital of Outer System technological innovation and commercial opportunity? Where are the municipal surveillance drones, normally so eager to eavesdrop and needlessly complicate an honest tax-free business, when one's health and wealth for once depend on their nosy presence?\nSpeaking of drones, we could use one in this skirmish, or even just a human lookout placed in a covered but unobstructed spot. Then the rest of the squad could snipe from concealment or from a safe distance.\nBarring that, we would end up in a better shape even if we all hid and fired blindly. We'd listen to impact sounds and wait vigilantly for incoming enemy missiles in order to register their trajectories and derive hints of enemy location. Apparently, ranged combat requires a change of pace and better planning than our previous illustrious successes accustomed us to.")
              , (Conquer, "That was a good fight, with skillful application of missiles, cover and concealment. The outcome is especially commendable given the high bar of tactical proficiency. Not even professional enforcement units can routinely deduce enemy position from the trajectory of their projectiles nor by firing without line of sight and interpreting auditory cues. However, while this steep hurdle is overcome, the chase is not over yet.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes vs. Three human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "The fight crashes over to a nearby mechanized farm. Law enforcement, crippled by the ban on firearms, won't show up until only wounded and dying remain to be revived and locked up.\nParcels and flasks of agricultural chemicals, scattered around, beg to be flung at foes as improvised missiles. Intense light makes it easy to aim and to discern trajectory of soaring items (by pointing at enemy projectiles with the crosshair in aiming mode). The effects of your last infracellular therapy finally start showing."
  , mreason = "This adventure is a flashback, picking the plot up where brawl (2) left it. It also teaches specifically the ranged combat skill in the simplified setup of a fully symmetric battle."
  , mhint   = "Try to come up with the best squad formation for this tactical challenge. Don't despair if you run out of ammo, because if you aim truly, enemy is left with few hit points remaining. Fight, ranged or melee, until all aggressors are disabled."
  }

hunt = ModeKind  -- melee vs ranged with reaction fire in daylight
  { mname   = "perilous hunt (5)"
  , mfreq   = [(PERILOUS, 1), (HUNT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterHunt
  , mcaves  = cavesHunt
  , mendMsg = [ (Killed, "Next time let's try to remember we are not on a sightseeing expedition. Also, leaving concealment is risky, leaving cover is foolhardy and wandering off is deadly. Also, what was that taking pictures by the mangrove tree all about? Were you trying to immortalize your handsome faces in case our shared pool of money was not enough to revive your sorry carcasses after the defeat?\nGood call, because after paying the techno-medical bills we are broke, while the gang foot soldiers that chase us seem to have military grade communication and reaction fire implants. And we were so close to complete victory and unfathomable wealth, if only we strove to lower the difficulty of this mission instead of raising it.")
      -- the guy is wrong about implants (though the items are genetically attuned), but being wrong is plausible when the team is killed off/chased off and can't scour the battleground
      -- this is in the middle of the scenario list and the mission is not tricky, so a subtle reminder about lowering difficulty, in case the player struggles
              , (Conquer, "We chased them off, like we knew that we would. It feels nice to stick together and prevail. Now we can do no wrong just minding our business and going our way to the spaceport. We taught them a lesson, despite their superior equipment, and nobody else needs to be harmed while we take possession of our rightful property, the glorious spaceship in Triton's orbit.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Seven heroes vs. Seven human enemies capable of concurrent attacks"
      , "* Minimize losses"
      , "* Incapacitate all human enemies ASAP"
      ]
  , mdesc   = "Who is the hunter and who is the prey? The only criterion is last man standing when the chase ends."
  , mreason = "This is yet another reminiscence of the events that led to the long crawl adventure. This encounter is quite a tactical challenge, because enemies are allowed to fling their ammo simultaneously at your team, which has no such ability."
  , mhint   = "Try not to outshoot the enemy, but to instead focus more on melee tactics. A useful concept here is communication overhead. Any team member that is not waiting and spotting for everybody, but acts, e.g., melees or moves or manages items, slows down all other team members by roughly 10%, because they need to keep track of his actions. Therefore, if other heroes melee, consider carefully if it makes sense to come to their aid, slowing them while you move, or if it's better to stay put and monitor the perimeter. This is true for all factions and all actors on each level separately, except the pointman of each faction, if it has one."  -- this also eliminates lag in big battles and helps the player to focus on combat and not get distracted by distant team members frantically trying to reach the battleground in time
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { mname   = "night escape (6)"
  , mfreq   = [(NIGHT, 1), (ESCAPE, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mendMsg = [ (Killed, "Somebody must have tipped the gang guards off. However, us walking along a lit trail, yelling, could have been a contributing factor. Also, it's worth noting that the torches prepared for this assault are best used as thrown makeshift flares.\nOn the other hand, equipping a lit torch makes one visible in the dark, regrettably but not quite unexpectedly. Lastly, the goal of this foray was to find the exit back to the city, marked by a yellow '>' sign, and to gather some treasure along the way. Not to harass every local evildoer, as much as they do deserve it.")
              , (Conquer, "It was enough to reach the escape area, namely the exit tunnel from the park marked by yellow '>' symbol. Spilling that much blood was risky. unnecessary and alerted the authorities. Having said that --- impressive indeed.")
              , (Escape, "Congratulations, you took your revenge and it's heavy in your pockets.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes vs. Seven human enemies capable of concurrent attacks"
      , "* Minimize losses"
      , "* Gather gems"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "Bloodied spaceship deed in hand notwithstanding, you can reach the derelict spaceliner only via a shuttle from the Central Triton Spaceport across the city. After hours of being chased in the opposite direction towards the border wall, you sneak back and make a desperate dash through the very den of the pursuing gang. Any valuables you come upon in this public park turned miscreant lair will be fair compensation for your losses, but you need to find the exit before the foes find you. Rein in your wrath and don't attack your tormentors. Foiling their plans by eluding them will be revenge enough."
  , mreason = "The focus of this installment is on stealthy exploration under the threat of numerically superior enemy."
  , mhint   = ""
  }

zoo = ModeKind  -- asymmetric crowd melee at night
  { mname   = "burning zoo (7)"
  , mfreq   = [(BURNING, 1), (ZOO, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mendMsg = [ (Killed, "Against such an onslaught, only clever positioning, use of terrain and patient vigilance gives any chance of survival.")
              , (Conquer, "That was a grim harvest. The city is safe again. So are your precious selves, with nothing and no one blocking your way to the spaceport any more.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Five heroes vs. Many enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "As justified and satisfying as the setting of enemy headquarters on fire has been, it backfires when the blaze immediately spreads to the public zoo on the path to the spaceport. Crazed animals mill around while the flames ignite greenery and consume nets, cages and security equipment. Whether that's a good sign or bad, apparently nobody is willing to pursue you any more. You are on your own, having to completely clean up the area, up to the last lurking predator, in order to safely move through."
  , mreason = "This is a crowd control exercise, at night, with a raging fire."
  , mhint   = "Note that communication overhead, as explained in perilous hunt adventure hints, makes it impossible for any faction to hit your heroes by more than 10 normal speed actors each turn. However, this is still too many, so position is everything."
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
  { mname   = "ranged ambush (8)"
  , mfreq   = [(RANGED, 1), (AMBUSH, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mendMsg = [ (Killed, "You turned out to be the prey, this time, not the hunter. In fact, you are not even in the hunters' league. When fighting against such odds, passively waiting for enemy to spring a trap is to no avail, because a professional team can sneak in darkness and ambush the ambushers.\nGranted, good positioning is crucial, so that each squad member can overwatch the battlefield and fire opportunistically, using the recently recovered mil-grade communication equipment. However, there is no hope without active scouting, throwing lit objects and probing suspect areas with missiles while paying attention to sounds. And that may still not be enough.")
              , (Conquer, "The new communication equipment enabling simultaneous ranged attacks with indirect aiming proved effective beyond expectation. With the mercenaries gone and nobody else having the slightest wish to interfere, the shuttle to the space cruiser at orbit is easy to launch at last. You let yourself bask in the battle's afterglow of bliss and relief. Now your turbulent adventure ends and the boring life of space cruiser scrap parts supplier or, as it may be, of a refurbished giant space liner operator, commences.\nA pity that the last round of shoddy genetic enhancements, bought at the grey market, scandalously auto-reverts at this very moment, leaving your personalized equipment that attuned to the previous genetic configuration inoperable. Fortunately, danger, debt and the gangster debt collectors are now behind you and the grey market won't see you ever again.") ]
  , mrules  = T.intercalate "\n"
      [ "* One level only"
      , "* Three heroes with concurrent attacks vs. Unidentified foes"
      , "* Minimize losses"
      , "* Assert control of the situation ASAP"
      ]
  , mdesc   = "Not even the unexplained carnage at the Central Triton Spaceport will prevent you from claiming the prize awaiting at the orbit. After all, you didn't take to the stars to let others decide your fate. There is still no news coverage from this ruin of what was the largest facility with tightest security in the whole Neptune Economic Area. Without waiting for explanations nor for the personnel to return, you creep along the burning booths, scouting for any airlock with a shuttle still attached and a way to restore power needed for the docking gear."
  , mreason = "In this encounter, finally, your heroes are able to all use ranged attacks simultaneously, given enough ammunition. Once you prevail in the encounter, the story catches up with the start of the main adventure, the long crawl." -- this is the only scenario with no objective specified, to give a bit of suspense, misdirection and mystery until the first win (death gives a hint only); being the last of the small scenarios, it won't scare off new players
  , mhint   = "Beware of friendly fire, particularly from explosives. But you need no more hints. Go fulfill your destiny!"
  }

safari = ModeKind  -- Easter egg available only via screensaver
  { mname   = "safari"
  , mfreq   = [(SAFARI, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = T.intercalate "\n"
      [ "* Three levels"
      , "* Many teammates capable of concurrent action vs. Many enemies"
      , "* Minimize losses"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "\"In this simulation you'll discover the joys of hunting the most exquisite of Earth's flora and fauna, both animal and semi-intelligent. Exit at the topmost level.\" This is a VR recording recovered from an alien nest debris."
  , mreason = "This is an Easter egg. The default squad doctrine is that all team members follow the pointman, but it can be changed from the settings submenu of the main menu."
  , mhint   = ""
  }

-- * Testing modes

dig = ModeKind
  { mname   = "dig"
  , mfreq   = [(DIG, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesDig
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Delve deeper!"
  , mreason = ""
  , mhint   = ""
  }

see = ModeKind
  { mname   = "see"
  , mfreq   = [(SEE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesSee
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all!"
  , mreason = ""
  , mhint   = ""
  }

short = ModeKind
  { mname   = "short"
  , mfreq   = [(SHORT, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesShort
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all short scenarios!"
  , mreason = ""
  , mhint   = ""
  }

crawlEmpty = ModeKind
  { mname   = "crawl empty"
  , mfreq   = [(CRAWL_EMPTY, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Enjoy the extra legroom."
  , mreason = ""
  , mhint   = ""
  }

crawlSurvival = ModeKind
  { mname   = "crawl survival"
  , mfreq   = [(CRAWL_SURVIVAL, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lure the human intruders deeper and deeper."
  , mreason = ""
  , mhint   = ""
  }

safariSurvival = ModeKind
  { mname   = "safari survival"
  , mfreq   = [(SAFARI_SURVIVAL, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  , mreason = ""
  , mhint   = ""
  }

battle = ModeKind
  { mname   = "battle"
  , mfreq   = [(BATTLE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked against those that reveal what should have been kept in the dark."
  , mreason = ""
  , mhint   = ""
  }

battleDefense = ModeKind
  { mname   = "battle defense"
  , mfreq   = [(BATTLE_DEFENSE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterBattleDefense
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that set the rules."
  , mreason = ""
  , mhint   = ""
  }

battleSurvival = ModeKind
  { mname   = "battle survival"
  , mfreq   = [(BATTLE_SURVIVAL, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that ally with the strongest."
  , mreason = ""
  , mhint   = ""
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { mname   = "defense"
  , mfreq   = [(DEFENSE, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Don't let the half-witted humans derail your operation and flee, like the puny, naked, tentacle-less beasts that they are!"
  , mreason = "This is an initial sketch of the reversed crawl game mode. Play on high difficulty to avoid guaranteed victories against the pitiful humans."
  , mhint   = ""
  }

defenseEmpty = ModeKind
  { mname   = "defense empty"
  , mfreq   = [(DEFENSE_EMPTY, 1)]
  , mtutorial = False
  , mattract = False
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lord over empty halls."
  , mreason = ""
  , mhint   = ""
  }

-- * Screensaver modes

screensaverGauntlet = gauntlet
  { mname   = "auto-gauntlet (1)"
  , mfreq   = []
  , mattract = True
  }

screensaverRaid = raid
  { mname   = "auto-raid (2)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverBrawl = brawl
  { mname   = "auto-brawl (3)"
  , mfreq   = []
  , mattract = True
  }

screensaverCrawl = crawl
  { mname   = "auto-crawl (long)"
  , mfreq   = []
  , mattract = True
  }

screensaverShootout = shootout
  { mname   = "auto-shootout (4)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverHunt = hunt
  { mname   = "auto-hunt (5)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverEscape = escape
  { mname   = "auto-escape (6)"
  , mfreq   = [(INSERT_COIN, 2)]
  , mattract = True
  }

screensaverZoo = zoo
  { mname   = "auto-zoo (7)"
  , mfreq   = []
  , mattract = True
  }

screensaverAmbush = ambush
  { mname   = "auto-ambush (8)"
  , mfreq   = []
  , mattract = True
  }

screensaverSafari = safari
  { mname   = "auto-safari"
  , mfreq   = [(INSERT_COIN, 1)]
  , mattract = True
  }

rosterGauntlet, rosterRaid, rosterBrawl, rosterCrawl, rosterShootout, rosterHunt, rosterEscape, rosterZoo, rosterAmbush, rosterSafari, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafariSurvival, rosterBattle, rosterBattleDefense, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

rosterGauntlet =
  [ ( EXPLORER_EXTERMINATOR
    , [(1, 2, EXTERMINATOR_HERO)] )
  , ( ROBOT_VIRUS
    , [] )  -- avoid a robot that sleeps far away and keeps the faction spawning
  , (HORROR_REPRESENTATIVE, []) ]

rosterRaid =
  [ ( ANIMAL_REPRESENTATIVE  -- starting over escape
    , [(2, 2, ANIMAL)] )
  , ( EXPLORER_SHORT
    , [(2, 2, HERO)] )
  , ( COMPETITOR_SHORT
    , [(2, 1, RAIDER_HERO)] )
  , ( ROBOT_REPRESENTATIVE
    , [(2, 1, ROBOT)] )
  , (HORROR_REPRESENTATIVE, []) ]  -- for summoned monsters

rosterBrawl =
  [ ( EXPLORER_NO_ESCAPE
    , [(3, 3, BRAWLER_HERO)] )
        -- start heroes on stairs, since they go first
  , ( COMPETITOR_NO_ESCAPE
    , [ (3, 3, BRAWLER_HERO)
      , (2, 3, BRAWLER_HERO) ] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterCrawl =
  [ ( EXPLORER_REPRESENTATIVE  -- start on stairs so that stash is handy
    , [(3, 3, CRAWL_HERO)] )
  , ( MONSTER_REPRESENTATIVE
    , [] )
  , ( ANIMAL_REPRESENTATIVE
    , [ (2, 5, ANIMAL)
      , (3, 4, ANIMAL)
      , -- Optional huge battle at the end:
        (16, 100, MOBILE_ANIMAL) ] )
  , ( ROBOT_REPRESENTATIVE
    , [(2, 4, ROBOT)] )
  , ( ROBOT_VIRUS
    , [] ) ]

-- Exactly one scout gets a sight boost, to help the aggressor, because he uses
-- the scout for initial attack, while camper (on big enough maps)
-- can't guess where the attack would come and so can't position his single
-- scout to counter the stealthy advance.
rosterShootout =
  [ ( EXPLORER_NO_ESCAPE
    , [(5, 2, RANGER_HERO), (5, 1, SCOUT_HERO)] )
  , ( COMPETITOR_NO_ESCAPE
    , [(5, 2, RANGER_HERO), (5, 1, SCOUT_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterHunt =
  [ ( EXPLORER_NO_ESCAPE
    , [(6, 7, SOLDIER_HERO)] )
  , ( COMPETITOR_NO_ESCAPE
    , [(6, 6, AMBUSHER_HERO), (6, 1, SCOUT_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterEscape =
  [ ( COMPETITOR_NO_ESCAPE  -- start on exit
    , [(7, 6, AMBUSHER_HERO), (7, 1, SCOUT_HERO)] )
  , ( EXPLORER_MEDIUM
    , [(7, 2, ESCAPIST_HERO), (7, 1, SCOUT_HERO)] )
      -- second on the list to let the bros occupy the exit
  , (HORROR_REPRESENTATIVE, []) ]

rosterZoo =
  [ ( EXPLORER_TRAPPED
    , [(8, 5, SOLDIER_HERO)] )
  , ( ANIMAL_CAPTIVE
    , [(8, 100, MOBILE_ANIMAL)] )
  , (HORROR_REPRESENTATIVE, []) ]  -- for summoned monsters

rosterAmbush =
  [ ( EXPLORER_NO_ESCAPE
    , [(9, 5, AMBUSHER_HERO), (9, 1, SCOUT_HERO)] )
  , ( OFF_WORLD_REPRESENTATIVE
    , [(9, 12, MERCENARY_HERO)] )
  , (HORROR_REPRESENTATIVE, []) ]

-- No horrors faction needed, because spawned heroes land in civilian faction.
rosterSafari =
  [ ( MONSTER_TOURIST
    , [(5, 15, MONSTER)] )
  , ( CONVICT_REPRESENTATIVE
    , [(5, 2, CIVILIAN)] )
  , ( ANIMAL_MAGNIFICENT
    , [(10, 15, MOBILE_ANIMAL)] )
  , ( ANIMAL_EXQUISITE  -- start on escape
    , [(16, 20, MOBILE_ANIMAL)] )
  , (HORROR_REPRESENTATIVE, []) ]
      -- construction hooter; neutral

rosterCrawlEmpty =
  [ ( EXPLORER_PACIFIST
    , [(1, 1, CRAWL_HERO)] )
  , (HORROR_PACIFIST, []) ]
      -- for spawned and summoned monsters

rosterCrawlSurvival =
  [ ( EXPLORER_AUTOMATED
    , [(3, 3, CRAWL_HERO)] )
  , ( MONSTER_REPRESENTATIVE
    , [(5, 1, MONSTER)] )
  , ( ANIMAL_OR_ROBOT_NARRATING
    , [(5, 10, ANIMAL)] )  -- explore unopposed for some time
  , (HORROR_REPRESENTATIVE, []) ]

rosterSafariSurvival =
  [ ( MONSTER_TOURIST_PASSIVE
    , [(5, 15, MONSTER)] )
  , ( CONVICT_REPRESENTATIVE
    , [(5, 3, CIVILIAN)] )
  , ( ANIMAL_MAGNIFICENT_NARRATING
    , [(10, 20, MOBILE_ANIMAL)] )
  , ( ANIMAL_EXQUISITE
    , [(16, 30, MOBILE_ANIMAL)] )
  , (HORROR_REPRESENTATIVE, []) ]

rosterBattle =
  [ ( EXPLORER_TRAPPED
    , [(10, 5, SOLDIER_HERO)] )
  , ( MONSTER_CAPTIVE
    , [(10, 35, MOBILE_MONSTER)] )
  , ( ANIMAL_CAPTIVE
    , [(10, 20, MOBILE_ANIMAL)] )
  , ( ROBOT_CAPTIVE
    , [(10, 15, MOBILE_ROBOT)] ) ]

rosterBattleDefense =
  [ ( EXPLORER_AUTOMATED_TRAPPED
    , [(10, 5, SOLDIER_HERO)] )
  , ( MONSTER_CAPTIVE_NARRATING
    , [(10, 35, MOBILE_MONSTER)] )
  , ( ANIMAL_CAPTIVE
    , [(10, 20, MOBILE_ANIMAL)] )
  , ( ROBOT_CAPTIVE
    , [(10, 15, MOBILE_ROBOT)] ) ]

rosterBattleSurvival =
  [ ( EXPLORER_AUTOMATED_TRAPPED
    , [(10, 5, SOLDIER_HERO)] )
  , ( MONSTER_CAPTIVE
    , [(10, 35, MOBILE_MONSTER)] )
  , ( ANIMAL_CAPTIVE_NARRATING
    , [(10, 20, MOBILE_ANIMAL)] )
  , ( ROBOT_CAPTIVE
    , [(10, 15, MOBILE_ROBOT)] ) ]

rosterDefense =
  [ ( EXPLORER_AUTOMATED
    , [(3, 3, CRAWL_HERO)] )
  , ( MONSTER_ANTI
    , [] )
  , ( ANIMAL_REPRESENTATIVE
    , -- Fun from the start to avoid empty initial level:
      [ (3, 5 + 1 `d` 2, ANIMAL)  -- many, because no spawning
      -- Optional huge battle at the end:
      , (16, 100, MOBILE_ANIMAL) ] )
  , ( ROBOT_REPRESENTATIVE
    , [] ) ]

rosterDefenseEmpty =
  [ ( MONSTER_ANTI_PACIFIST
    , [(4, 1, SCOUT_MONSTER)] )
  , (HORROR_PACIFIST, []) ]
      -- for spawned and summoned animals

cavesGauntlet, cavesRaid, cavesBrawl, cavesCrawl, cavesShootout, cavesHunt, cavesEscape, cavesZoo, cavesAmbush, cavesSafari, cavesDig, cavesSee, cavesShort, cavesCrawlEmpty, cavesBattle :: Caves

cavesGauntlet = [([1], [CAVE_GAUNTLET])]

cavesRaid = [([2], [CAVE_RAID])]

cavesBrawl = reverse [([2], [CAVE_BRAWL_ALT]), ([3], [CAVE_BRAWL])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([1], [CAVE_OUTERMOST])
  , ([2], [CAVE_SHALLOW_ROGUE])
  , ([3], [CAVE_BRIDGE])
  , ([4], [CAVE_NOISE])
  , ([8, 7, 6, 5], [CAVE_VIRUS, CAVE_ROGUE, CAVE_ARENA, CAVE_RESIDENTIAL])
       -- reversed order, to match @reverse@ later on
  , ([9], [CAVE_LABORATORY])
  , ([12, 11, 10], [DEFAULT_RANDOM, DEFAULT_RANDOM, CAVE_MUSEUM])
  , ([13], [CAVE_EXIT])
  , ([15, 14], [DEFAULT_RANDOM, CAVE_CASINO])
  , ([16], [CAVE_POWER]) ]

cavesCrawl = reverse listCrawl

cavesShootout = [([5], [CAVE_SHOOTOUT])]

cavesHunt = [([6], [CAVE_HUNT])]

cavesEscape = [([7], [CAVE_ESCAPE])]

cavesZoo = [([8], [CAVE_ZOO])]

cavesAmbush = [([9], [CAVE_AMBUSH])]

cavesSafari = reverse [ ([5], [CAVE_SAFARI_1])
                      , ([10], [CAVE_SAFARI_2])
                      , ([16], [CAVE_SAFARI_3]) ]

cavesDig =
  reverse $ concat
  $ zipWith (map . renumberCaves)
            [0, 16 ..]
            (replicate 100 listCrawl)
--            [0, 2 ..]
--            (replicate 100 [([1], [CAVE_OUTERMOST]),([2], [CAVE_EXIT])])

renumberCaves :: Int -> ([Int], [GroupName CaveKind])
              -> ([Int], [GroupName CaveKind])
renumberCaves offset (ns, l) = (map (+ offset) ns, l)

cavesSee = let numberCaves n c = ([n], [c])
           in reverse $ zipWith numberCaves [1..]
              $ concatMap (replicate 10) allCaves

cavesShort = let numberCaves n c = ([n], [c])
             in reverse $ zipWith numberCaves [1..]
                $ concatMap (replicate 100) $ take 7 allCaves

allCaves :: [GroupName CaveKind]
allCaves =
  [ CAVE_GAUNTLET, CAVE_RAID, CAVE_BRAWL, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_ESCAPE
  , CAVE_ZOO, CAVE_AMBUSH
  , CAVE_OUTERMOST, CAVE_SHALLOW_ROGUE, CAVE_BRIDGE, CAVE_NOISE, CAVE_ROGUE
  , CAVE_ARENA, CAVE_RESIDENTIAL, CAVE_VIRUS, CAVE_LABORATORY, CAVE_MUSEUM
  , CAVE_EXIT, CAVE_CASINO, CAVE_POWER ]

cavesCrawlEmpty = reverse $
  map (\(ns, grps) ->
        (ns, if grps == [CAVE_BRIDGE] then [CAVE_SHALLOW_ROGUE] else grps))
      listCrawl

cavesBattle = [([10], [CAVE_BATTLE])]
