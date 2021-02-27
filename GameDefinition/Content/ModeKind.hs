-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2020 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Game mode definitions.
module Content.ModeKind
  ( -- * Group names
    groupNamesSingleton, groupNames
  , -- * Content
    content
#ifdef EXPOSE_INTERNAL
  -- * Group name patterns
  , pattern RAID, pattern BRAWL, pattern LONG, pattern CRAWL, pattern FOGGY, pattern SHOOTOUT, pattern PERILOUS, pattern HUNT, pattern NIGHT, pattern ESCAPE, pattern BURNING, pattern ZOO, pattern RANGED, pattern AMBUSH, pattern SAFARI, pattern DIG, pattern SEE, pattern SHORT, pattern CRAWL_EMPTY, pattern CRAWL_SURVIVAL, pattern SAFARI_SURVIVAL, pattern BATTLE, pattern BATTLE_DEFENSE, pattern BATTLE_SURVIVAL, pattern DEFENSE, pattern DEFENSE_EMPTY
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import qualified Data.Text as T

import Content.CaveKind hiding (content, groupNames, groupNamesSingleton)
import Content.ItemKindActor
import Content.ModeKindPlayer
import Game.LambdaHack.Content.CaveKind (CaveKind, pattern DEFAULT_RANDOM)
import Game.LambdaHack.Content.ModeKind
import Game.LambdaHack.Core.Dice
import Game.LambdaHack.Definition.Defs

-- * Group name patterns

groupNamesSingleton :: [GroupName ModeKind]
groupNamesSingleton =
       [RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, ESCAPE, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY]

pattern RAID, BRAWL, LONG, CRAWL, FOGGY, SHOOTOUT, PERILOUS, HUNT, NIGHT, ESCAPE, BURNING, ZOO, RANGED, AMBUSH, SAFARI, DIG, SEE, SHORT, CRAWL_EMPTY, CRAWL_SURVIVAL, SAFARI_SURVIVAL, BATTLE, BATTLE_DEFENSE, BATTLE_SURVIVAL, DEFENSE, DEFENSE_EMPTY :: GroupName ModeKind

groupNames :: [GroupName ModeKind]
groupNames = [NO_CONFIRMS]

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
  [raid, brawl, crawl, shootout, hunt, escape, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverSafari]

raid,    brawl, crawl, shootout, hunt, escape, zoo, ambush, safari, dig, see, short, crawlEmpty, crawlSurvival, safariSurvival, battle, battleDefense, battleSurvival, defense, defenseEmpty, screensaverRaid, screensaverBrawl, screensaverCrawl, screensaverShootout, screensaverHunt, screensaverEscape, screensaverZoo, screensaverAmbush, screensaverSafari :: ModeKind

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

raid = ModeKind
  { msymbol = 'r'
  , mname   = "raid (tutorial, 1)"
  , mfreq   = [(RAID, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mroster = rosterRaid
  , mcaves  = cavesRaid
  , mendMsg = [ (Killed, "That was unfortunate. The bill for the rescue team and for the subsequent nano medbot treatment will reach the stars. Perhaps more stealth was needed? Perhaps the items lying around the area could aid survival instead of ending up ignored or passively hoarded? Or perhaps a wise course of action would be to choose a Neptune Area Administration challenge with a lower difficulty?")
              , (Defeated, "Sadly, you got worked up in the tunnels while another team snatched the prize. Remember, you are at the Outer Frontier to gain wealth and independence through industriousness and commerce and that means clashing with competing agents, not just fighting the feral nature.")
              , (Escape, "You are the first to clear a route through the sewer system. Triton City authorities will now be able to establish a perimeter and mop up the side tunnels. You collect your reward of 100 gold grains and start looking for a way to invest it profitably on this Solar System's commercial frontier, abounding in more or less (usually less) regulated opportunities.\nAfter some thought you decide to start by splurging on genetic enhancement for your team. The effects won't be visible at once and you have no time to lose, having just made fresh enemies.") ]
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Two heroes vs. Spawned enemies"
      , "* Gather gold"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "Neptune Area Administration confirms isolated spottings of oversize vermin in non-residential zones of the Neptune's Triton moon's largest city. To put it plainly: Triton City sewers need purging. The first person to break through to the other exit will be paid 100 gold grains. The Administration \"strongly urges participants not to resort to violence against each other.\" However, no punitive consequences are specified, not even disqualification from the contest."
  , mreason = "In addition to initiating the game plot, this adventure provides an introductory tutorial. Relax, explore, gather loot, find the exit and escape. With some luck, you won't even need to fight anything."
  , mhint   = "You can't use gathered items in your next encounters, so trigger any consumables at will, in particular the electronic chips as common as pebbles on the muddy sewer floors.\nFeel free to scout with only one of the heroes and keep the other one immobile, e.g., standing guard over the squad's shared inventory stash. If in grave danger, retreat with the scout to join forces with the guard. The more gold collected and the faster the victory, the higher your score in this encounter."
  }

brawl = ModeKind  -- sparse melee in daylight, with shade for melee ambush
  { msymbol = 'k'
  , mname   = "brawl (tutorial, 2)"
  , mfreq   = [(BRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = True
  , mroster = rosterBrawl
  , mcaves  = cavesBrawl
  , mendMsg = [ (Killed, "That treacherous villain didn't honour his word and brought his friends to the fight. It would still not turn so bad if we remembered to use terrain to protect us from missiles or even completely hide our presence and if we honourably kept together to the end, at the same time preventing the overwhelming enemy forces from brutishly ganging up on our modest-sized, though valiant, squad. Having to repurchase the genetic therapy was the most painful outcome, one that would send you broke and in shame to Earth, if repeated, to start collecting your Basic Income.")
              , (Conquer, "Bringing help was a sober and prudent move that resulted in well-earned victory and a splendid trophy of a title to a real inter-planetary space vessel. Unfortunately, the treacherous foe called reinforcements at the last moment, a new wave arriving even now. It may be wise to move the celebration of the victory to a more fitting area, assuming that the dignified translocation can be accomplished timely and inconspicuously.") ]
  , mrules  = T.intercalate "\n" $
      [ "* Two levels"
      , "* Three heroes vs. Three human enemies per level"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "\"You scoundrel! You cheated in the sewers, fighting two against one. Come alone to the woody biosphere behind the saloon at noon, if you dare. Given that I win, I take back all your gold. Otherwise, you get the scrapping rights for the giant spaceliner's hull in orbit. Yes, it's mine, you tramp; here's the docking transmitter and the paperwork. The fight is to the last man standing, no evasion, no breaks for nano-healing in town.\""
  , mreason = "In addition to advancing the game plot, this encounter trains melee, squad formation, stealth and stairs use. On each level separately, the battle is symmetric, both in numbers, goals (incapacitate all enemies) and squad capabilities (only the pointman moves, while all others either melee or wait). Observe and mimic the enemies. If you can't see an enemy that apparently can see you, in reversed circumstances you would have the advantage. Savour the relative fairness --- you won't find any in the main crawl adventure that follows."
  , mhint   = "Run a short distance with Shift or LMB, switch the pointman with Tab, repeat. In open terrain, if you keep distance between teammates, this resembles the leap frog infantry tactics. For best effects, end each sprint behind a cover or concealment.\nOnce you clear a level, descend by bumping into stairs. Use Tab to switch to remaining heroes until all gather on the new level.\nIf you get beaten repeatedly, try using all consumables you find, particularly the vials that collect healing extracts abounding in this rich biosphere. Ponder the hints from the defeat message, in particular the one about keeping your party together once the opponents are spotted. However, if you want to discover a winning tactics on your own, make sure to ignore any such tips until you succeed."
  }

crawl = ModeKind
  { msymbol = 'c'
  , mname   = "long crawl (main)"
  , mfreq   = [(LONG, 1), (CRAWL, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mroster = rosterCrawl
  , mcaves  = cavesCrawl
  , mendMsg = [ (Killed, "It was not supposed to end this way. Perhaps more stealth was in order? Perhaps foes that didn't carry any key resources to your survival nor to your escape could have been eluded and ignored? Perhaps the gathered items should be used for survival instead of hoarded? Or perhaps the challenge, chosen freely but without awareness of the grisly difficulty, was insurmountable and lost from the very start? Nobody is going to find out, even if humans ever set their feet here again and prevail, another time, another way.")
              , (Escape, "The shuttle doors close behind, docking clamps grind in farewell and the giant rotating disc slowly tumbles away in rear view. You feel at once a relief and a sense of loss. This is not finished. You are changed forever, but you know nothing. You've heard the call, but you made no answer. You came for petty change, found a treasure beyond comprehension, then barely escaped with your life as the prize.\nAnd nobody will believe you at home. But you don't need their understanding any more. You have enough money to heal, regroup, track the ship down and try again. It's your personal space cruiser, after all, with a world of its own, inside.") ]
  , mrules  = T.intercalate "\n" $
      [ "* Many levels, some requiring tools to access"
      , "* Three heroes vs. Spawned enemies"
      , "* Gather gold, gems and stimpacks"
      , "* Find exit and escape ASAP"
      ]
  , mdesc   = "You are busy looting, with utmost satisfaction, the blasted bridge of an old and extravagantly luxurious cruise liner.\nSuddenly, the inert spaceship, supposedly long deserted and barely able to sustain life support, tremors and dials her fusion engines up to red overdrive. The space map flickering among the irreversibly damaged consoles shows the ship manoeuvre deftly off Triton orbit and purposefully climb the Neptune's gravity well. There's no way to control the ship and static floods all communication channels.\nYou decide to scour the nearby dilapidated decks for missing squad members, this time sending them in pairs and mapping the area properly, and then get back to the spaceport the way you came, in your shuttle. However, you are determined not to leave the ship without taking at least a portion of the wealth that is rightfully yours."
  , mreason = "This is the main, longest and most replayable scenario of the game. The fundamental goal is survival of your crew. Sub-goals will present themselves as you take in newly visited spaceship decks and figure out ways to reach those that are presently cut off."
  , mhint   = "If you keep dying, attempt the subsequent adventures as a breather (perhaps at lowered difficulty). They fill the gaps in the plot and teach particular skills that may come in handy and help you discover new tactics of your own or come up with a strategy for staving off the attrition. Also experimenting with the initial adventures may answer some questions you didn't have when you attempted them originally."
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
  { msymbol = 's'
  , mname   = "foggy shootout (3)"
  , mfreq   = [(FOGGY, 1), (SHOOTOUT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mroster = rosterShootout
  , mcaves  = cavesShootout
  , mendMsg = [ (Killed, "This is a disgrace. How is a thuggish robbery in broad daylight even possible in a moon city that styles itself as the capital of Outer System technological innovation and commercial opportunity? Where are the municipal surveillance drones, normally so eager to eavesdrop and needlessly complicate an honest tax-free business, when one's health and wealth for once depend on their nosy presence? Speaking of drones, we could use one in this skirmish, or even just a human lookout placed in a covered but unobstructed spot. Then the rest of the squad could snipe from concealment or from a safe distance.\nBarring that, we would end up in a better shape even if we all hid and fired blindly. We'd listen to impact sounds and wait with ten-fold vigilance for incoming enemy missiles, in order to register their trajectories and derive hints of enemy location. Apparently, ranged combat requires a change of pace and better planning than our previous illustrious successes accustomed us to.")
              , (Conquer, "That was a good fight, with skillful application of missiles, cover and concealment. The outcome is especially commendable given the high bar of tactical proficiency. Not even professional enforcement units can routinely deduce enemy position from the trajectory of their projectiles nor by firing without line of sight and interpreting auditory cues. However, while this steep hurdle is overcome, the chase is not over yet.") ]
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Three heroes vs. Three human enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "The fight crashes over to a nearby mechanized farm. Law enforcement, crippled by the ban on firearms, won't show up until only wounded and dying remain to be revived and locked up. Parcels and flasks of agricultural chemicals, scattered around, beg to be flung at foes as improvised missiles. Intense light makes it easy to aim and to discern trajectory of soaring items (point at enemy projectiles with the crosshair in aiming mode). The effects of the last infracellular therapy finally start showing."
  , mreason = "This adventure is a flashback, picking the plot up where brawl (2) left it. It also teaches specifically the ranged combat skill in the simplified setup of fully symmetric battle."
  , mhint   = "Try to come up with the best squad formation for this tactical challenge. Don't despair if you run out of ammo, because if you aim truly, enemy has few hit points left at this time. Fight, ranged or melee, until all aggressors are disabled."
  }

hunt = ModeKind  -- melee vs ranged with reaction fire in daylight
  { msymbol = 'h'
  , mname   = "perilous hunt (4)"
  , mfreq   = [(PERILOUS, 1), (HUNT, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mroster = rosterHunt
  , mcaves  = cavesHunt
  , mendMsg = [ (Killed, "Next time let's try to remember we are not on a sightseeing expedition. Also, leaving concealment is risky, leaving cover is foolhardy and wandering off is deadly. Also, what was that taking pictures by the mangrove tree all about? Were you trying to immortalize your handsome faces in case our shared pool of money was not enough to revive your sorry carcasses after the defeat? Good call, because after paying the techno-medical bills we are broke and the military grade communication and reaction fire implants of the gang foot soldiers that chase us don't raise our prospects either. And we were so close to complete victory and unfathomable wealth, if only we strove to lower the difficulty of this mission instead of raising it.")
      -- the guy is wrong about implants (though the items are genetically attuned), but being wrong is plausible when the team is killed off/chased off and can't scour the battleground
      -- this is in the middle of the scenario list and the mission is not tricky, so a subtle reminder about lowering difficulty, in case the player struggles
              , (Conquer, "We chased them off, like we knew that we would. It feels nice to stick together and prevail. Now we can do no wrong just minding our business and going our way to the spaceport. We taught them a lesson, despite their superior equipment, and nobody else needs to be harmed while we take possession of our rightful property, the glorious spaceship in Triton's orbit.") ]
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Seven heroes vs. Seven human enemies capable of concurrent attacks"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "Who is the hunter and who is the prey? The only criterion is last man standing when the chase ends."
  , mreason = "This is yet another reminiscence of the events that led to the long crawl adventure. This encounter is quite a tactical challenge, because enemies are allowed to fling their ammo simultaneously at your team, which has no such ability."
  , mhint   = "Try not to outshoot the enemy, but to instead focus more on melee tactics. A useful concept here is communication overhead. Any team member that is not waiting and spotting for everybody, but acts, e.g., melees or moves or manages items, slows down all other team members by rougly 10%, because they need to keep track of his actions. Therefore, if other heroes melee, consider carefully if it makes sense to come to their aid, slowing them while you move, or if it's better to stay put and monitor the perimeter. This is true for all factions and all actors on each level separately, except the pointman of each faction, if any."  -- this also eliminates lag in big battles and helps the player to focus on combat and not get distracted by distant team members frantically trying to reach the battleground in time
  }

escape = ModeKind  -- asymmetric ranged and stealth race at night
  { msymbol = 'e'
  , mname   = "night escape (5)"
  , mfreq   = [(NIGHT, 1), (ESCAPE, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mroster = rosterEscape
  , mcaves  = cavesEscape
  , mendMsg = [ (Killed, "Somebody must have tipped the gang guards off. However, us walking along a lit trail, yelling, could have been a contributing factor. Also, it's worth noting that the torches prepared for this assault are best used as thrown makeshift flares. On the other hand, equipping a lit torch makes one visible in the dark, regrettably but not quite unexpectedly. Lastly, the goal of this foray was to find the exit back to the city, marked by a yellow '>' sign, and to gather some treasure along the way, but not to bedevil every local evildoer, as much as they do deserve it.")
              , (Conquer, "It was enough to reach the escape area, namely the exit tunnel from the park marked by yellow '>' symbol. Spilling that much blood was risky and unnecessary. Having said that --- impressive indeed.")
              , (Escape, "Congratulations, you took your revenge and it's heavy in your pockets.") ]
  , mrules  = T.intercalate "\n" $
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
  { msymbol = 'b'
  , mname   = "burning zoo (6)"
  , mfreq   = [(BURNING, 1), (ZOO, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mroster = rosterZoo
  , mcaves  = cavesZoo
  , mendMsg = [ (Killed, "Against such an onslaught, only clever positioning, use of terrain and patient vigilance gives any chance of survival.")
              , (Conquer, "That was a grim harvest. The city is safe again. So are your precious selves, with nothing and no one blocking your way to the spaceport any more.") ]
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Five heroes vs. Many enemies"
      , "* Minimize losses"
      , "* Incapacitate all enemies ASAP"
      ]
  , mdesc   = "As justified and satisfying as setting the enemy headquarters on fire has been, it backfires when the blaze immediately spreads to the public zoo on the path to the spaceport. Crazed animals mill around while the flames ignite greenery and consume nets, cages and security equipment. Whether that's a good sign or bad, apparently nobody is willing to pursue you any more. You are on your own, having to completely clean up the area, up to the last lurking predator, in order to safely move through."
  , mreason = "This is a crowd control exercise, at night, with a raging fire."
  , mhint   = "Note that communication overhead, as explained in perilous hunt adventure hints, makes it impossible for any faction to hit your heroes by more than 10 normal speed actors each turn. However, this is still too much, so position is everything."
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
  , mname   = "ranged ambush (7)"
  , mfreq   = [(RANGED, 1), (AMBUSH, 1), (CAMPAIGN_SCENARIO, 1)]
  , mtutorial = False
  , mroster = rosterAmbush
  , mcaves  = cavesAmbush
  , mendMsg = [ (Killed, "You turned out to be the prey, this time, not the hunter. In fact, you are not even in the hunters' league. And it's hard to operate docking equipment while being chased round a spaceport. When fighting against such odds, passively waiting for enemy to spring a trap is to no avail, because a professional team can sneak in darkness and ambush the ambushers. Granted, good positioning is crucial, so that each squad member can overwatch the battlefield and fire opportunistically, using the recently recovered mil-grade communication equipment. However, there is no hope without active scouting, throwing lit objects and probing suspect areas with missiles while paying attention to sounds. And that may still not be enough.")
              , (Conquer, "The new communication equipment, enabling simultaneous ranged attacks with indirect aiming, apparently proved effective beyond expectations. With the mercenaries gone and nobody else having the slightest wish to interfere, the shuttle to the space cruiser at orbit is easy to launch at last. Now is your moment of glory. Now your turbulent adventure ends and the boring life of space cruiser scrap parts supplier or, as it may be, of a refurbished giant space liner operator, commences.\nA pity that the last round of shoddy genetic enhancements, bought at the grey market, scandalously auto-reverts at this very moment, leaving the personalized equipment that attuned to the previous genetic configuration inoperable. Fortunately, danger, debt and the gangster debt collectors are now behind you and the grey market won't see you again.") ]
  , mrules  = T.intercalate "\n" $
      [ "* One level only"
      , "* Three heroes with concurrent attacks vs. Unidentified foes"
      , "* Minimize losses"
      , "* Assert control of the situation ASAP"
      ]
  , mdesc   = "Not even the unexplained carnage at the Central Triton Spaceport will prevent you from claiming the prize awaiting you at the orbit. After all, you didn't take to the stars to let others decide your fate. There is still no news coverage from the ruin of what was the largest and tightest security facility in the whole Neptune Area. Without waiting for explanations nor for the personnel to return, you creep along the burning booths, scouting for any airlock with a shuttle still attached and a way to restore power needed for the docking gear."
  , mreason = "In this encounter, finally, your heroes are able to all use ranged attacks at once, given enough ammunition. Once you prevail in the encounter, the story catches up with the start of the main adventure, the long crawl." -- this is the only scenario with no objective specified, to give a bit of suspense, misdirection and mystery until the first win (death gives a hint only); being the last of the small scenarios, it won't scare off new players
  , mhint   = ""
  }

safari = ModeKind  -- Easter egg available only via screensaver
  { msymbol = 'f'
  , mname   = "safari"
  , mfreq   = [(SAFARI, 1)]
  , mtutorial = False
  , mroster = rosterSafari
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = T.intercalate "\n" $
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
  { msymbol = 'd'
  , mname   = "dig"
  , mfreq   = [(DIG, 1)]
  , mtutorial = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesDig
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Delve deeper!"
  , mreason = ""
  , mhint   = ""
  }

see = ModeKind
  { msymbol = 'a'
  , mname   = "see"
  , mfreq   = [(SEE, 1)]
  , mtutorial = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesSee
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all!"
  , mreason = ""
  , mhint   = ""
  }

short = ModeKind
  { msymbol = 's'
  , mname   = "short"
  , mfreq   = [(SHORT, 1)]
  , mtutorial = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesShort
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "See all short scenarios!"
  , mreason = ""
  , mhint   = ""
  }

crawlEmpty = ModeKind
  { msymbol = 'c'
  , mname   = "crawl empty"
  , mfreq   = [(CRAWL_EMPTY, 1)]
  , mtutorial = False
  , mroster = rosterCrawlEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Enjoy the extra legroom."
  , mreason = ""
  , mhint   = ""
  }

crawlSurvival = ModeKind
  { msymbol = 'd'
  , mname   = "crawl survival"
  , mfreq   = [(CRAWL_SURVIVAL, 1)]
  , mtutorial = False
  , mroster = rosterCrawlSurvival
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lure the human intruders deeper and deeper."
  , mreason = ""
  , mhint   = ""
  }

safariSurvival = ModeKind
  { msymbol = 'u'
  , mname   = "safari survival"
  , mfreq   = [(SAFARI_SURVIVAL, 1)]
  , mtutorial = False
  , mroster = rosterSafariSurvival
  , mcaves  = cavesSafari
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "In this simulation you'll discover the joys of being hunted among the most exquisite of Earth's flora and fauna, both animal and semi-intelligent."
  , mreason = ""
  , mhint   = ""
  }

battle = ModeKind
  { msymbol = 'b'
  , mname   = "battle"
  , mfreq   = [(BATTLE, 1)]
  , mtutorial = False
  , mroster = rosterBattle
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked against those that reveal what should have been kept in the dark."
  , mreason = ""
  , mhint   = ""
  }

battleDefense = ModeKind
  { msymbol = 'f'
  , mname   = "battle defense"
  , mfreq   = [(BATTLE_DEFENSE, 1)]
  , mtutorial = False
  , mroster = rosterBattleDefense
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that set the rules."
  , mreason = ""
  , mhint   = ""
  }

battleSurvival = ModeKind
  { msymbol = 'i'
  , mname   = "battle survival"
  , mfreq   = [(BATTLE_SURVIVAL, 1)]
  , mtutorial = False
  , mroster = rosterBattleSurvival
  , mcaves  = cavesBattle
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Odds are stacked for those that ally with the strongest."
  , mreason = ""
  , mhint   = ""
  }

defense = ModeKind  -- perhaps a real scenario in the future
  { msymbol = 'e'
  , mname   = "defense"
  , mfreq   = [(DEFENSE, 1)]
  , mtutorial = False
  , mroster = rosterDefense
  , mcaves  = cavesCrawl
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Don't let the half-witted humans derail your operation and flee, like the puny, naked, tentacle-less beasts that they are!"
  , mreason = "This is an initial sketch of the reversed crawl game mode. Play on high difficulty to avoid guaranteed victories against the pitiful humans."
  , mhint   = ""
  }

defenseEmpty = ModeKind
  { msymbol = 'e'
  , mname   = "defense empty"
  , mfreq   = [(DEFENSE_EMPTY, 1)]
  , mtutorial = False
  , mroster = rosterDefenseEmpty
  , mcaves  = cavesCrawlEmpty
  , mendMsg = []
  , mrules  = ""
  , mdesc   = "Lord over empty halls."
  , mreason = ""
  , mhint   = ""
  }

-- * Screensaver modes

screensaverRaid = screensave (AutoLeader False False) $ raid
  { mname   = "auto-raid (1)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverBrawl = screensave (AutoLeader False False) $ brawl
  { mname   = "auto-brawl (2)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

screensaverCrawl = screensave (AutoLeader False False) $ crawl
  { mname   = "auto-crawl (long)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

screensaverShootout = screensave (AutoLeader False False) $ shootout
  { mname   = "auto-shootout (3)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverHunt = screensave (AutoLeader False False) $ hunt
  { mname   = "auto-hunt (4)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverEscape = screensave (AutoLeader False False) $ escape
  { mname   = "auto-escape (5)"
  , mfreq   = [(INSERT_COIN, 2), (NO_CONFIRMS, 1)]
  }

screensaverZoo = screensave (AutoLeader False False) $ zoo
  { mname   = "auto-zoo (6)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

screensaverAmbush = screensave (AutoLeader False False) $ ambush
  { mname   = "auto-ambush (7)"
  , mfreq   = [(NO_CONFIRMS, 1)]
  }

-- changing leader by client needed, because of TFollow
screensaverSafari = screensave (AutoLeader False True) $ safari
  { mname   = "auto-safari"
  , mfreq   = [(INSERT_COIN, 1), (NO_CONFIRMS, 1)]
  }

teamExplorer, teamCompetitor, teamCivilian, teamMercenary :: TeamContinuity
teamExplorer = TeamContinuity 0
teamCompetitor = TeamContinuity 1
teamCivilian = TeamContinuity 2
teamMercenary = TeamContinuity 3

rosterRaid, rosterBrawl, rosterCrawl, rosterShootout, rosterHunt, rosterEscape, rosterZoo, rosterAmbush, rosterSafari, rosterCrawlEmpty, rosterCrawlSurvival, rosterSafariSurvival, rosterBattle, rosterBattleDefense, rosterBattleSurvival, rosterDefense, rosterDefenseEmpty :: Roster

rosterRaid = Roster
  { rosterList = [ ( playerHero {fhiCondPoly = hiHeroShort}
                   , Just teamExplorer
                   , [(2, 2, HERO)] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fhiCondPoly = hiHeroShort }
                   , Just teamCompetitor
                   , [(2, 1, HERO)] )
                 , ( playerAnimal  -- starting over escape
                   , Nothing
                   , [(2, 2, ANIMAL)] )
                 , ( playerRobot
                   , Nothing
                   , [(2, 1, ROBOT)] )
                 , (playerHorror, Nothing, []) ]  -- for summoned monsters
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
                   , Just teamExplorer
                   , [(3, 3, BRAWLER_HERO)] )
                       -- start heroes on stairs, since they go first
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , Just teamCompetitor
                   , [ (3, 3, BRAWLER_HERO)
                     , (2, 3, BRAWLER_HERO) ] )
                 , (playerHorror, Nothing, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterCrawl = Roster
  { rosterList = [ ( playerHero
                   , Just teamExplorer
                   , [(3, 3, CRAWL_HERO)] )
                 , ( playerMonster
                   , Nothing
                   , [] )
                 , ( playerAnimal
                   , Nothing
                   , [ (2, 5, ANIMAL)
                     , (3, 4, ANIMAL)
                     , -- Optional huge battle at the end:
                       (15, 100, MOBILE_ANIMAL) ] )
                 , ( playerRobot
                   , Nothing
                   , [(2, 4, ROBOT)] ) ]
  , rosterEnemy = [ ("Spacefarer", "Alien Hierarchy")
                  , ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Robot Anarchy") ]
  , rosterAlly = [ ("Alien Hierarchy", "Animal Kingdom")
                 , ("Alien Hierarchy", "Robot Anarchy")
                 , ("Robot Anarchy", "Animal Kingdom") ] }

-- Exactly one scout gets a sight boost, to help the aggressor, because he uses
-- the scout for initial attack, while camper (on big enough maps)
-- can't guess where the attack would come and so can't position his single
-- scout to counter the stealthy advance.
rosterShootout = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , Just teamExplorer
                   , [(5, 1, SCOUT_HERO), (5, 2, RANGER_HERO)] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , Just teamCompetitor
                   , [(5, 1, SCOUT_HERO), (5, 2, RANGER_HERO)] )
                 , (playerHorror, Nothing, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterHunt = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , Just teamExplorer
                   , [(6, 7, SOLDIER_HERO)] )
                 , ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , Just teamCompetitor
                   , [(6, 1, SCOUT_HERO), (6, 6, AMBUSHER_HERO)] )
                 , (playerHorror, Nothing, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterEscape = Roster
  { rosterList = [ ( playerAntiHero { fname = "Red Collar Bro"
                                    , fcanEscape = False  -- start on escape
                                    , fneverEmpty = False  -- loot after killing
                                    , fhiCondPoly = hiHeroMedium }
                   , Just teamCompetitor
                   , [(7, 1, SCOUT_HERO), (7, 6, AMBUSHER_HERO)] )
                 , ( playerHero {fhiCondPoly = hiHeroMedium}
                   , Just teamExplorer
                   , [(7, 1, SCOUT_HERO), (7, 2, ESCAPIST_HERO)] )
                     -- second on the list to let bros occupy the exit
                 , (playerHorror, Nothing, []) ]
  , rosterEnemy = [ ("Spacefarer", "Red Collar Bro")
                  , ("Spacefarer", "Horror Den")
                  , ("Red Collar Bro", "Horror Den") ]
  , rosterAlly = [] }

rosterZoo = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , Just teamExplorer
                   , [(8, 5, SOLDIER_HERO)] )
                 , ( playerAnimal {fneverEmpty = True}
                   , Nothing
                   , [(8, 100, MOBILE_ANIMAL)] )
                 , (playerHorror, Nothing, []) ]  -- for summoned monsters
  , rosterEnemy = [ ("Spacefarer", "Animal Kingdom")
                  , ("Spacefarer", "Horror Den") ]
  , rosterAlly = [] }

rosterAmbush = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroMedium }
                   , Just teamExplorer
                   , [(9, 1, SCOUT_HERO), (9, 5, AMBUSHER_HERO)] )
                 , ( playerAntiHero { fname = "Gray Off-World Mercenary"
                                    , fcanEscape = False
                                    , fhiCondPoly = hiHeroMedium }
                   , Just teamMercenary
                   , [(9, 12, MERCENARY_HERO)] )
                 , (playerHorror, Nothing, []) ]
  , rosterEnemy = [ ("Spacefarer", "Gray Off-World Mercenary")
                  , ("Spacefarer", "Horror Den")
                  , ("Gray Off-World Mercenary", "Horror Den") ]
  , rosterAlly = [] }

-- No horrors faction needed, because spawned heroes land in civilian faction.
rosterSafari = Roster
  { rosterList = [ ( playerMonsterTourist
                   , Nothing
                   , [(5, 15, MONSTER)] )
                 , ( playerHunamConvict
                   , Just teamCivilian
                   , [(5, 2, CIVILIAN)] )
                 , ( playerAnimalMagnificent
                   , Nothing
                   , [(10, 15, MOBILE_ANIMAL)] )
                 , ( playerAnimalExquisite  -- start on escape
                   , Nothing
                   , [(15, 20, MOBILE_ANIMAL)] )
                 , (playerHorror, Nothing, []) ]
                     -- construction hooter; neutral
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

rosterCrawlEmpty = Roster
  { rosterList = [ ( playerHero
                   , Just teamExplorer
                   , [(1, 1, CRAWL_HERO)] )
                 , (playerHorror, Nothing, []) ]
                     -- for spawned and summoned monsters
  , rosterEnemy = []
  , rosterAlly = [] }

rosterCrawlSurvival = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , Just teamExplorer
                   , [(3, 3, CRAWL_HERO)] )
                 , ( playerMonster
                   , Nothing
                   , [(5, 1, MONSTER)] )
                 , ( playerAnimal {fhasUI = True}
                   , Nothing
                   , [(5, 10, ANIMAL)] )  -- explore unopposed for some time
                 , ( playerRobot
                   , Nothing
                   , [(3, 3, ROBOT)] ) ] }

rosterSafariSurvival = rosterSafari
  { rosterList = [ ( playerMonsterTourist
                       { fleaderMode = LeaderAI $ AutoLeader True True
                       , fhasUI = False }
                   , Nothing
                   , [(5, 15, MONSTER)] )
                 , ( playerHunamConvict
                   , Just teamCivilian
                   , [(5, 3, CIVILIAN)] )
                 , ( playerAnimalMagnificent
                       { fleaderMode = LeaderUI $ AutoLeader True False
                       , fhasUI = True }
                   , Nothing
                   , [(10, 20, MOBILE_ANIMAL)] )
                 , ( playerAnimalExquisite
                   , Nothing
                   , [(15, 30, MOBILE_ANIMAL)] )
                 , (playerHorror, Nothing, []) ] }

rosterBattle = Roster
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong }
                   , Just teamExplorer
                   , [(10, 5, SOLDIER_HERO)] )
                 , ( playerMonster {fneverEmpty = True}
                   , Nothing
                   , [(10, 35, MOBILE_MONSTER)] )
                 , ( playerAnimal {fneverEmpty = True}
                   , Nothing
                   , [(10, 20, MOBILE_ANIMAL)] )
                 , ( playerRobot {fneverEmpty = True}
                   , Nothing
                   , [(10, 15, MOBILE_ROBOT)] ) ]
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
                   , Just teamExplorer
                   , [(10, 5, SOLDIER_HERO)] )
                 , ( playerMonster { fneverEmpty = True
                                   , fhasUI = True }
                   , Nothing
                   , [(10, 35, MOBILE_MONSTER)] )
                 , ( playerAnimal {fneverEmpty = True}
                   , Nothing
                   , [(10, 20, MOBILE_ANIMAL)] )
                 , ( playerRobot {fneverEmpty = True}
                   , Nothing
                   , [(10, 15, MOBILE_ROBOT)] ) ] }

rosterBattleSurvival = rosterBattle
  { rosterList = [ ( playerHero { fcanEscape = False
                                , fhiCondPoly = hiHeroLong
                                , fleaderMode =
                                    LeaderAI $ AutoLeader False False
                                , fhasUI = False }
                   , Just teamExplorer
                   , [(10, 5, SOLDIER_HERO)] )
                 , ( playerMonster {fneverEmpty = True}
                   , Nothing
                   , [(10, 35, MOBILE_MONSTER)] )
                 , ( playerAnimal { fneverEmpty = True
                                  , fhasUI = True }
                   , Nothing
                   , [(10, 20, MOBILE_ANIMAL)] )
                 , ( playerRobot {fneverEmpty = True}
                   , Nothing
                   , [(10, 15, MOBILE_ROBOT)] ) ] }

rosterDefense = rosterCrawl
  { rosterList = [ ( playerAntiHero
                   , Just teamExplorer
                   , [(3, 3, CRAWL_HERO)] )
                 , ( playerAntiMonster
                   , Nothing
                   , [] )
                 , ( playerAnimal
                   , Nothing
                   , -- Fun from the start to avoid empty initial level:
                     [ (3, 5 + 1 `d` 2, ANIMAL)  -- many, because no spawning
                     -- Optional huge battle at the end:
                     , (15, 100, MOBILE_ANIMAL) ] )
                 , ( playerRobot
                   , Nothing
                   , [] ) ] }

rosterDefenseEmpty = rosterCrawl
  { rosterList = [ ( playerAntiMonster {fneverEmpty = True}
                   , Nothing
                   , [(4, 1, SCOUT_MONSTER)] )
                 , (playerHorror, Nothing, []) ]
                     -- for spawned and summoned animals
  , rosterEnemy = []
  , rosterAlly = [] }

cavesRaid, cavesBrawl, cavesCrawl, cavesShootout, cavesHunt, cavesEscape, cavesZoo, cavesAmbush, cavesSafari, cavesDig, cavesSee, cavesShort, cavesCrawlEmpty, cavesBattle :: Caves

cavesRaid = [([2], [CAVE_RAID])]

cavesBrawl = reverse [([2], [CAVE_BRAWL_ALT]), ([3], [CAVE_BRAWL])]

listCrawl :: [([Int], [GroupName CaveKind])]
listCrawl =
  [ ([1], [CAVE_OUTERMOST])
  , ([2], [CAVE_SHALLOW_ROGUE])
  , ([3], [CAVE_BRIDGE])
  , ([4], [CAVE_NOISE])
  , ([7, 6, 5], [CAVE_ROGUE, CAVE_ARENA, CAVE_RESIDENTIAL])
       -- reversed order, to match @reverse@ later on
  , ([8], [CAVE_LABORATORY])
  , ([11, 10, 9], [DEFAULT_RANDOM, DEFAULT_RANDOM, CAVE_MUSEUM])
  , ([12], [CAVE_EXIT])
  , ([14, 13], [DEFAULT_RANDOM, CAVE_CASINO])
  , ([15], [CAVE_POWER]) ]

cavesCrawl = reverse listCrawl

cavesShootout = [([5], [CAVE_SHOOTOUT])]

cavesHunt = [([6], [CAVE_HUNT])]

cavesEscape = [([7], [CAVE_ESCAPE])]

cavesZoo = [([8], [CAVE_ZOO])]

cavesAmbush = [([9], [CAVE_AMBUSH])]

cavesSafari = reverse [ ([5], [CAVE_SAFARI_1])
                      , ([10], [CAVE_SAFARI_2])
                      , ([15], [CAVE_SAFARI_3]) ]

cavesDig =
  reverse $ concat
  $ zipWith (map . renumberCaves)
            [0, 15 ..]
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
  [ CAVE_RAID, CAVE_BRAWL, CAVE_SHOOTOUT, CAVE_HUNT, CAVE_ESCAPE, CAVE_ZOO
  , CAVE_AMBUSH
  , CAVE_OUTERMOST, CAVE_SHALLOW_ROGUE, CAVE_BRIDGE, CAVE_NOISE, CAVE_ROGUE
  , CAVE_ARENA, CAVE_RESIDENTIAL, CAVE_LABORATORY, CAVE_MUSEUM, CAVE_EXIT
  , CAVE_CASINO, CAVE_POWER ]

cavesCrawlEmpty = reverse $
  map (\(ns, grps) ->
        (ns, if grps == [CAVE_BRIDGE] then [CAVE_SHALLOW_ROGUE] else grps))
      listCrawl

cavesBattle = [([10], [CAVE_BATTLE])]
