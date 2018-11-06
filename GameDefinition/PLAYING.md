Playing Allure of the Stars
===========================

The following blurb is a copy of the game intro screen.

Your party of trusted adventurers is about to test its fortune by plundering
a vast derelict passenger cruiser. The spaceship was abandoned and lost
after an accident at the Solar System's outer frontier, but recently
unexpectedly reappeared. Reportedly, deposit safes and jewelry inventories
were never salvaged. Neither were emptied the gilded robot holds,
the expensive arboretum nor the extravagant whole-deck habitat of natural
and enhanced wild animals.

Up to this day, countless autonomous life support and damage mitigation
subsystems with redundant power sources busy themselves throughout dozens
of decks. They seem successful, because there are abundant traces of ongoing
metabolism all over the ship. That should make exploration so much easier.

Yours can't be the only shady Neptune Area crew interested in the giant wreck,
so you shouldn't count on reaching it unopposed. The feral outer frontier
denizens are not famed for scruples nor for restraint when using hazardous
nano, cyber and bio technologies, so never assume you are safe. Be ready
to hide in shadows, create distractions, set up ambushes, bump into
unspeakable horrors, puzzling machinery and astounding treasures and make
creative use of all you find, because you are on your own. If ever you turn
your back in fear, expect to be chased tirelessly by sight, sound and smell.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, a lot of imagination is required
at this stage of game development, but the game is already playable
and winnable. Contributions are welcome.

If the game window is too large for your screen or you experience
other technical issues, please consult
[README.md](https://github.com/AllureOfTheStars/Allure/blob/master/README.md)
or describe your problem on gitter or the issue tracker.


Heroes
------

The heroes are marked on the map with symbols `@` and `1` through `9`.
The currently chosen party leader is highlighted on the map
and his attributes are displayed at the bottommost status line,
which in its most complex form looks as follows.

    *@12        4d1+5% Calm: 20/60 HP: 33/50 Target: basilisk  [**__]

The line starts with the list of party members, with the current leader
highlighted. Most commands involve only the leader, including movement
with keyboard's keypad or `LMB` (left mouse button). If more heroes
are selected, e.g., by clicking on the list with `RMB` (right mouse button),
they run together whenever `:` or `RMB` over map area is pressed.

Next on the status line is the damage of the currently best melee weapon
the leader can use, then his current and maximum Calm (morale, composure,
focus, attentiveness), then his current and maximum HP (hit points, health).
At the end, the personal target of the leader is described, in this case
a basilisk monster, with hit points drawn as a bar. Additionally,
the colon after "Calm" turning into a dot signifies that the leader
is in a position without ambient illumination and a brace sign instead
of colon after "HP" means the leader is braced for combat (see section
[Basic Commands](#basic-commands)).

Instead of a monster, the target area may describe a position on the map,
a recently spotted item on the floor or an item in inventory selected
for further action or, if none are available, just display the current
leader name. Weapon damage and other item properties are displayed using
the dice notation `xdy`, which means `x` rolls of `y`-sided dice.
A variant denoted `xdLy` is additionally scaled by the level depth
in proportion to the maximal level depth (at the first level it's
always one, then it grows up to full rolled value at the last level).
Section [Monsters](#monsters) below describes combat resolution in detail,
including the percentage damage bonus seen in the example.

The second, upper status line describes the current level in relation
to the party.

    5  Lofty hall   [33% seen] X-hair: exact spot (71,12)  p15 l10

First comes the depth of the current level and its name.
Then the percentage of its explorable tiles already seen by the heroes.
The `X-hair` (aiming crosshair) is the common focus of the whole party,
marked on the map and manipulated with mouse or movement keys in aiming mode.
In this example, the corsshair points at an exact position on the map
and at the end of the status line comes the length of the shortest
path from the leader position to the spot and the straight-line distance
between the two points.


Game map
--------

The map of any particular scenario may consist of one or many
levels and each level consists of a large number of tiles.
The game world is persistent, i.e., every time the player visits
a level during a single game, its layout is the same.
The basic tile kinds are as follows.

    game map terrain type                  on-screen symbol
    wall                                   #
    tree or rock or man-made column        O
    rubble                                 &
    bush, transparent obstacle             %
    trap, ice                              ^
    closed door                            +
    open door                              '
    smoke or fog                           ;
    ground, corridor                       .
    water                                  ~
    stairs or exit up                      <
    stairs or exit down                    >

So, for example, the following map shows a room with a closed door
connected by a corridor with a room with an open door, a pillar,
staircase up and rubble that obscures one of the corners.

    ####       ####
    #..#########..&&
    #..+.......'.O.<&#
    #..#########.....#
    ####       #######


Basic Commands
--------------

This section is a copy of the basic few screens of in-game help. The help
pages are automatically generated based on a game's keybinding content and
on overrides in the player's config file. The remaining in-game help screens,
not shown here, list all game commands grouped by categories, in detail.
A text snapshot of the complete in-game help is in
[InGameHelp.txt](InGameHelp.txt).

Walk throughout a level with mouse or numeric keypad (left diagram below)
or its compact laptop replacement (middle) or the Vi text editor keys (right,
enabled in config.ui.ini). Run, until disturbed, by adding Shift or Control.
Go-to with LMB (left mouse button). Run collectively with RMB.

               7 8 9          7 8 9          y k u
                \|/            \|/            \|/
               4-5-6          u-i-o          h-.-l
                /|\            /|\            /|\
               1 2 3          j k l          b j n

In aiming mode, the same keys (and mouse) move the x-hair (aiming crosshair).
Press 'KP_5' ('5' on keypad, or 'i' or '.') to wait, bracing for impact,
which reduces any damage taken and prevents displacement by foes. Press
'C-KP_5' (the same key with Control) to wait 0.1 of a turn, without bracing.
You displace enemies by running into them with Shift/Control or RMB. Search,
open, descend and attack by bumping into walls, doors, stairs and enemies.
The best item to attack with is automatically chosen from among weapons
in your personal equipment and your body parts.

The following commands, joined with the basic set above, let you accomplish
anything in the game, though not necessarily with the fewest keystrokes.
You can also play the game exclusively with a mouse, or both mouse and
keyboard. See the ending help screens for mouse commands. Lastly, you can
select a command with arrows or mouse directly from the help screen
or the dashboard and execute it on the spot.

    keys         command
    g or ,       grab item(s)
    ESC          cancel aiming/open main menu
    RET or INS   accept target/open dashboard
    SPACE        clear messages/display history
    S-TAB        cycle among all party members
    KP_* or !    cycle x-hair among enemies
    C-c          open or close or alter
    +            swerve the aiming line

Screen area and UI mode (aiming/exploration) determine mouse click effects.
Here is an overview of effects of each button over most of the game map area.
The list includes not only left and right buttons, but also the optional
middle mouse button (MMB) and even the mouse wheel, which is normally used
over menus, to page-scroll them.
For mice without RMB, one can use C-LMB (Control key and left mouse button).

    keys         command
    LMB          set x-hair to enemy/go to pointer for 25 steps
    RMB or C-LMB fling at enemy/run to pointer collectively for 25 steps
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

You can enter the detailed aiming mode with the `*` keypad key that selects
enemies or the `/` keypad key that cycles among items on the floor
and marks a tile underneath an item. You can move x-hair with direction keys
and assign a personal target to the leader with `RET` key (Return, Enter).
The details of the shared x-hair position and of the personal target
are described in the status lines at the bottom of the screen,
as explained in section [Heroes](#heroes) above.

Commands for saving and exiting the current game, starting a new game,
configuring convenience settings for the current game and challenges
for the next game are listed in the main menu, brought up by the `ESC` key.
Game difficulty, from the challenges menu, determines
hitpoints at birth for any actor of any UI-using faction.
The "lone wolf" challenge mode reduces player's starting actors to exactly
one (consequently, this does not affect the initial 'raid' scenario).
The "cold fish" challenge mode makes it impossible for player characters
to be healed by actors from other factions (this is a significant
restriction in the final 'crawl' scenario).

For a person new to roguelikes, the 'raid' scenario offers a gentle
introduction. The subsequent game scenarios gradually introduce
squad combat, stealth, opportunity fire, asymmetric battles and more.
Starting from the second scenario, the player controls a whole team
of characters and will develop his repertoire of squad formations,
preferred rendezvous locations and the use of light sources. The last
scenario takes place in a multi-floor setting, giving player the choice
of exploration of a single level at a time or portions of many levels
along a single staircase and also of guarding staircases against
enemies from other levels or, inversely, avoiding the staircases.


Monsters
--------

The life of the heroes is full of dangers. Monstrosities, natural
and out of this world, roam the dark corridors and crawl from damp holes
day and night. While heroes pay attention to all other party members
and take care to move one at a time, monsters don't care about each other
and all move at once, sometimes brutally colliding by accident.

When a hero bumps into a monster or a monster attacks the hero,
melee combat occurs. Heroes and monsters running into one another
(with the `Shift` or `Control` key) do not inflict damage, but change places.
This gives the opponent a free blow, but can improve the tactical situation
or aid escape. In some circumstances actors are immune to the displacing,
e.g., when both parties form a continuous front-line.

In melee combat, the best equipped weapon (or the best fighting organ)
of each opponent is taken into account for determining the damage
and any extra effects of the blow. Since an item needs to be recharged
in order to have its full effect, weapons on timeout are only considered
according to their raw damage dice (the same as displayed at bottommost
status line).

To determine the damage dealt, the outcome of the weapon's damage dice roll
is multiplied by a percentage bonus. The bonus is calculated by taking
the damage bonus (summed from the equipped items of the attacker,
capped at 200%) minus the melee armor modifier of the defender
(capped at 200%, as well), with the outcome bounded between -99% and 99%,
which means that at least 1% of damage always gets through
and the damage is always lower than twice the dice roll.
The current leader's melee bonus, armor modifier and other detailed
skills can be viewed via the `#` command.

In ranged combat, the missile is assumed to be attacking the defender
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

The stress of combat drains Calm, gradually limiting the use of items and,
if Calm reaches zero and the actor is impressed by his foes,
making him defect and surrender to their domination.
Whenever the monster's or hero's hit points reach zero,
the combatant is incapacitated and promptly dies.
When the last hero dies or is dominated, the scenario ends in defeat.


On Winning and Dying
--------------------

You win a scenario if you escape the location alive (which may prove
difficult, because your foes gradually build up the ambush squads
blocking your escape routes) or, in scenarios with no exit locations,
if you eliminate all opposition. In the former case, your score
is based predominantly on the gold and precious gems you've plundered.
In the latter case, your score is most influenced by the number
of turns you spent overcoming your foes (the quicker the victory, the better;
the slower the demise, the better). Bonus points, affected by the number
of heroes lost, are awarded only if you win. The score is heavily
modified by the chosen game difficulty, but not by any other challenges.

When all your heroes fall, you are going to invariably see a new foolhardy
party of adventurers clamoring to be led into the unknown. They start
their conquest from a new entrance, with no experience and no equipment,
and new, undaunted enemies bar their way. Lead the new hopeful explorers
to fame, wealth and glory!
