Playing Allure of the Stars
===========================

The following blurb is a copy of the game intro screen.

               Your party of trusty explorers is about to test
               its fortune by plundering a vast derelict
               passenger cruiser. The spaceship was abandoned
               and lost after an accident at the Solar System's
               outer frontier, but recently unexpectedly
               reappeared. Reportedly, deposit safes and jewelry
               inventories were never salvaged. Neither were
               emptied the gilded robot holds, the expensive
               arboretum nor the extravagant whole-deck habitat
               of natural and enhanced wild animals.

               Up to this day, countless autonomous life support
               and damage mitigation subsystems with redundant
               power sources busy themselves throughout dozens
               of decks. They seem successful, because there are
               abundant traces of ongoing metabolism all over
               the ship. That should make exploration so much
               easier.

               Yours can't be the only shady Neptune Area crew
               interested in the giant wreck, so you shouldn't
               count on reaching it unopposed. The feral
               outer frontier denizens are not famed
               for scruples nor for restraint when using
               hazardous nano, cyber and bio technologies,
               so never assume you are safe. Be ready to hide
               in shadows, create distractions, set up ambushes,
               bump into unspeakable horrors, puzzling machinery
               and astounding treasures and make creative use
               of all you find, because you are on your own.
               If ever you turn your back in fear, expect to be
               chased tirelessly by sight, sound and smell.

Allure of the Stars is a turn-based game. You issue a command.
Then you watch its results unfold on the screen without you being able
to intervene. Then all settles down and you have as much time
as you want to inspect the battlefield and think about your next move.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, a lot of imagination is required,
because the game's plot and characters are yet very lightly sketched.
However there are already hours and days of replayable fun to be had
and gameplay mastery to build, confirm via high scores and pit against
challenges. Contributions of all kinds are welcome.
Please offer feedback to mikolaj.konarski@funktory.com or, preferably,
at any of the public forums.

If the game window is too large for your screen or you experience
other technical issues, please consult
[README.md](https://github.com/AllureOfTheStars/Allure/blob/master/README.md)
or describe your problem on gitter or the issue tracker.


Game map
--------

The map of any particular scenario consists of one or many levels
and each level has a large number of tiles with a particular
terrain kind on each. The game world is persistent, i.e., every time
the player visits a level during a single game, its layout is the same.

Terrain is depicted with non-letter and non-digit (except zero `0`)
characters, the same as items lying on the ground, though blocky
solid symbol are more likely to be non-passable terrain than items.
In case of doubt, one of the aiming commands (keypad `/`, with default
keybinding) cycles through all visible and remembered items on the level
and another (keypad `*`, with default keybinding) through all foes.
The basic terrain kinds are as follows.

    terrain type                           on-screen symbol
    wall                                   #
    tree or rock or man-made column        0
    rubble                                 &
    bush, transparent obstacle             %
    trap, ice obstacle                     ^
    closed door                            +
    open door                              '
    smoke or fog                           ;
    ground, corridor                       .
    water                                  ~
    stairs or exit up                      <
    stairs or exit down                    >

Actors are marked with lower and upper case letters and with
characters `@` and `1` through `9` (but never `0`). Player-controlled
heroes are always bright white and by default they are selected
(e.g., to run together) so they have a blue highlight around their symbol.
If player manages to control animals or other actors, they retain their
letter and color, but gain a highlight as well.

So, for example, the following map shows a room with a closed door,
full of actors, connected by a corridor with a room with an open door,
a pillar, a staircase down and rubble that obscures one of the corners.
The lower row of the larger room is full of items.

    ######       ######
    #@19.#########....&&
    #r...+.......'...0.>&&#
    #Ra..#########[?!,)$"=#
    ######       ##########


Heroes
------

The heroes are displayed on the map with bright white color (red if they are
about to fall down) and symbols `@` and `1` through `9` (never `0`).
The currently chosen party leader is yellow-highlighted on the map
and his attributes are displayed at the bottom-most status line which,
in its most complex form, looks as follows.

    *@12   2m/s Calm: 20/60 HP: 33/50 Leader: Haskell Alvin   6d1+5% 4d1

The line starts with the list of party members, with the current leader
highlighted in yellow. Most commands involve only the leader, including
movement with keyboard's keypad or `LMB` (left mouse button). If more
heroes are selected (highlighted in blue), they run together
whenever `:` or `S-LMB` (while holding Shift) over map area is pressed.
Any sleeping hero is highlighted in green and can be woken up
by yelling with `%`, which also taunts or stresses nearby enemies.

Next on the bottom-most status line is the leader's current and maximum
Calm (morale, composure, focus, attentiveness), then his current
and maximum HP (hit points, health). The colon after "Calm" turning
into a dot signifies that the leader is in a position without ambient
illumination, making a stealthy conduct easier. A brace sign instead
of a colon after "HP" means the leader is braced for combat
(see section [Basic Commands](#basic-commands)).

In the second half of the bottom-most status line, the leader's name
is shown. Then come damage dice of the leader's melee weapons and leader's
appendages, ordered by their power. The dice of the first recharged weapon,
the one that would be used in this moment, is adorned with percentage
damage bonus collected from the whole equipment of the leader.
If the dice are displayed with upper-case `D` instead of lower-case `d`,
the weapon has additional effects apart of the usual kinetic damage.
The nature of the effects can be appraised via the `E`quipment screen.

Weapon damage and other item properties are displayed using
the dice notation `xdy`, which denotes `x` rolls of `y`-sided dice.
A variant written `xdLy` is additionally scaled by the level depth
in proportion to the maximal level depth (at the first level it's
always one, then it grows up to full rolled value at the last level).
Section [Monsters](#monsters) below describes combat resolution in detail,
including the role of the percentage damage bonus.

The second, the upper status line describes the current level in relation
to the party.

    5  Lofty hall    [33% seen] X-hair: dire basilisk    [**__]

First comes the depth of the current level and its name.
Then the percentage of its explorable tiles already seen by the heroes.
The `X-hair` (aiming crosshair) is the common focus of the whole party,
marked on the map with a red box and manipulated with mouse
or movement keys in aiming mode. In this example, the crosshair points
at a dire basilisk monster, with its hit points drawn as a bar.

Instead of a monster, the `X-hair` area may describe a position on the map,
a recently spotted item on the floor or an item in inventory selected
for further action or, if none are available, a summary of the team status.
For example, this form

    5  Lofty hall    [33% seen] X-hair: exact spot (71,12)    p15 l10

indicates that the party is aiming at an exact spot on the map.
At the end of the status line comes the length of the shortest
path from the leader's position to the spot and the straight-line
distance between the two points, one that a flung projectile would travel.


Basic Commands
--------------

This section is a copy of the few basic screens of in-game help. The help
pages are automatically generated based on a game's keybinding content and
on overrides in the player's config file. The remaining in-game help screens,
not shown here, list all game commands grouped by categories in detail.
A text snapshot of the complete in-game help is in
[InGameHelp.txt](InGameHelp.txt).

Walk throughout a level with mouse or numeric keypad (left diagram below)
or the Vi editor keys (right) or with a compact laptop setup (middle) that
requires enabling in config.ui.ini. Run until disturbed with Shift or Control.
Go-to with LMB (left mouse button). Run collectively via S-LMB (holding Shift).

               7 8 9          7 8 9          y k u
                \|/            \|/            \|/
               4-5-6          u-i-o          h-.-l
                /|\            /|\            /|\
               1 2 3          j k l          b j n

In aiming mode, the same keys (and mouse) move the x-hair (aiming crosshair).
Press `KP_5` (`5` on keypad) to wait, bracing for impact, which reduces any
damage taken and prevents displacement by foes. Press `S-KP_5` or `C-KP_5`
(the same key with Shift or Control) to lurk 0.1 of a turn, without bracing.
Displace enemies by running into them with Shift/Control or S-LMB. Search,
open, descend and attack by bumping into walls, doors, stairs and enemies.
The best melee weapon is automatically chosen from your equipment
and from among your body parts.

The following commands, joined with the basic set above,
let you accomplish anything in the game, though
not necessarily with the fewest keystrokes. You can also
play the game exclusively with a mouse, or both mouse
and keyboard. (See the ending help screens for mouse commands.)
Lastly, you can select a command with arrows or mouse directly
from the help screen or the dashboard and execute it on the spot.

    keys         command
    E            manage equipment of the leader
    g or ,       grab item(s)
    ESC          open main menu/finish aiming
    RET or INS   open dashboard/accept target
    SPACE        clear messages and show history
    S-TAB        cycle among all party members
    KP_* or !    cycle x-hair among enemies
    KP_/ or /    cycle x-hair among items
    c            close door
    %            yell/yawn

Screen area and UI mode (exploration/aiming) determine
mouse click effects. First, we give an overview
of effects of each button over the game map area.
The list includes not only left and right buttons, but also
the optional middle mouse button (MMB) and the mouse wheel,
which is also used over menus, to page-scroll them.
(For mice without RMB, one can use Control key with LMB.)

    keys         command
    LMB          go to pointer for 25 steps/fling at enemy
    S-LMB        run to pointer collectively for 25 steps/fling at enemy
    RMB or C-LMB start aiming at enemy under pointer
    C-RMB        open or close or alter at pointer
    MMB          snap x-hair to floor under pointer
    WHEEL-UP     swerve the aiming line
    WHEEL-DN     unswerve the aiming line


Advanced Commands
-----------------

For ranged attacks, setting the aiming crosshair beforehand is not mandatory,
because x-hair is set automatically as soon as a monster comes into view
and can still be adjusted for as long as the missile to fling is not chosen.
However, sometimes you want to examine the level map tile by tile
or assign persistent personal targets to party members.
The latter is essential in the rare cases when your henchmen
(non-leader characters) can move autonomously or fire opportunistically
(via innate skills or rare equipment). Also, if your henchman is adjacent
to more than one enemy, setting his target makes him melee a particular foe.

You can enter the aiming mode with the `*` keypad key that selects
enemies or the `/` keypad key that cycles among items on the floor
and marks a tile underneath an item. You can move x-hair
with direction keys and assign a personal target to the leader
with a `RET` key (Return, Enter). The details of the shared x-hair mark
are displayed in a status line close to the bottom of the screen,
as explained in section [Heroes](#heroes) above.

Commands for saving and exiting the current game, starting a new game,
configuring convenience settings for the current game and challenges
for the next game are listed in the main menu, brought up by the `ESC` key.
Game difficulty, from the challenges menu, determines hitpoints at birth:
difficulty below 5 multiplies hitpoints of player characters, difficulty
over 5 multiplies hitpoints of their enemies. Of the convenience settings,
the `suspect terrain` choice is particularly interesting, because it
determines not only screen display of the level map, but also whether
suspect tiles are considered for auto-explore and for the `C-?` command that
marks the nearest unexplored position.

The "lone wolf" challenge mode reduces player's starting actors to exactly
one (consequently, this does not affect the initial 'raid' scenario).
The "cold fish" challenge mode makes it impossible for player characters
to be healed by actors from other factions (this is a significant
restriction in the final 'crawl' scenario).

For a person new to roguelikes, the 'raid' scenario offers a gentle
introduction. The subsequent game scenarios lead the player along
an optional story arc. They gradually introduce squad combat,
stealth, opportunity fire, asymmetric battles and more.
Starting from the second scenario, the player controls a whole team
of characters and will develop his repertoire of squad formations,
preferred rendezvous locations and the use of light sources.

The last scenario, a multi-level crawl, is the gist and the main challenge
of the game. It spans 15 varied spaceship decks, requiring lots of time,
focus and flexibility to beat and providing considerable replayability.
The player has a choice of exploring a single level at a time or portions
of many floors along a single staircase. On some levels he may explore
and loot with a single scout eluding most opponents. On others he may be
forced to change the pace and perform a complete exterminatory sweep
involving his whole party. On yet others, his best course of action may be
to defend a key location until the first wave of attackers is broken.

The size of the crawl scenario calls for strategic thinking, including
resource management, area denial and unavoidable temporary retreats
to other levels. Compared to that, the smaller scenarios, where the arena
is a single floor only, provide a mostly tactical training
and additional entertainment by trying to beat a high-score.
They offer variety and a breather between the deaths^H^H^H^H^H^H the brave
attempts at the long crawl scenario. Some gameplay elements,
however, like totally symmetric skirmishes, a race to the exit or heavy
reliance on opportunity fire, feature only in the small scenarios.


Monsters
--------

The life of the heroes is full of dangers. Monstrosities, natural
and out of this world, roam the dark corridors and crawl from damp holes
day and night. While heroes pay attention to all other party members
and take care to move one at a time, monsters don't care about each other
and all move at once, sometimes brutally colliding by accident.

Monsters are depicted on the map with letters. Upper case letters
are unique monsters, often guardians of special floors, and lower case
letters are the rabble. If there are humans not from our team,
they are marked with `@` and `1` through `9` in other colours than white.

When a hero walks and bumps into a monster or a monster attacks
the hero, melee combat occurs. Hero *running* into and displacing
a monster (with the `Shift` or `Control` key), or the other way around,
does not inflict damage, but exchanges places. This gives the opponent
a free blow, but can improve the tactical situation or aid escape.
In some circumstances actors are immune to the displacing,
e.g., when both parties form a continuous front-line.

In melee combat, the best recharged equipped weapon (or the best fighting
organ that is not on cooldown) of each opponent is taken into account
for determining the damage and any extra effects of the blow.
To determine the damage dealt, the outcome of the weapon's damage dice roll
is multiplied by a percentage bonus. The bonus is calculated by taking
the damage bonus (summed from the equipped items of the attacker,
capped at 200%) minus the melee armor modifier of the defender
(capped at 200%, as well), with the outcome bounded between -99% and 99%,
which means that at least 1% of damage always gets through
and the damage is always lower than twice the dice roll.
The current leader's melee bonus, armor modifier and other detailed
skills can be viewed via the `#` command.

In ranged combat, the projectile is assumed to be attacking the defender
in melee, using itself as the weapon, with the usual dice and damage bonus.
This time, the ranged armor skill of the defender is taken into account
and, additionally, the speed of the missile (based on shape and weight)
figures in the calculation. You may propel any item from your inventory
(by default you are offered only the appropriate items; press `+` to cycle
item menu modes). Only items of a few kinds inflict any damage, but some
have other effects, beneficial, detrimental or mixed.

In-game detailed item descriptions contain melee and ranged damage estimates.
They do not take into account damage from effects and, if bonuses are not
known, guesses are based on averages for the item kind in question.
The displayed figures are rounded, but the game internally keeps track
of minute fractions of HP.

The stress of combat drains Calm, gradually limiting viewing radius and,
if Calm reaches zero and the actor is sufficiently impressed by his foes,
making him defect and surrender to their domination.
Whenever the monster's or hero's hit points reach zero,
the combatant is incapacitated and promptly dies.
When the last hero dies or is dominated, the scenario ends in defeat.


On Winning and Dying
--------------------

You win a scenario if you escape the location alive (which may prove
difficult, because your foes tend to gradually build up the ambush squad
blocking your escape route) or, in scenarios with no exit locations,
if you eliminate all opposition. In the former case, your score
is based predominantly on the gold and precious gems you've plundered.
In the latter case, your score is most influenced by the number
of turns you spent overcoming your foes (the quicker the victory, the better;
the slower the demise, the better). Bonus points, affected by the number
of heroes lost, are awarded only if you win. The score is heavily
modified by the chosen game difficulty, but not by any other challenges.

When all your heroes fall, you are going to invariably see a new foolhardy
party of adventurers clamoring to be led into the unknown perils. They start
their conquest from a new entrance, with no experience and no equipment,
and new undaunted enemies bar their way. Lead the new hopeful explorers
to fame, wealth and glory!
