Playing Allure of the Stars
===========================

Playing Allure of the Stars involves walking around a Solar System space-ship,
alone or in a party of trusted crew-members, gaining access to new levels,
bumping into hostile intruders, doors and walls, gathering technical resources
and making creative use of them. The bloodthirsty alien monsters do the same,
intelligence allowing, while tirelessly chasing the noble heroes
by smell and night-sight.

Once the few basic command keys and on-screen symbols are learned,
mastery and enjoyment of the game is the matter of tactical skill
and literary imagination. To be honest, a lot of imagination is required
right now, but the game is already playable and winnable.
Contributions welcome.


Terrain
-------

The goal of the hero is to explore the spaceship, battle the horrors within,
gather as much gold coins and precious gems as possible, and escape
to tell the tale. The spaceship consists of many levels covered with varying
terrain of the following basic kinds:

               terrain type                       on-screen symbol
               floor                              .
               wall                               #
               pillar                             O
               stairs up                          <
               stairs down                        >
               open door                          '
               closed door                        +

The game world is persistent, i.e., every time the player visits a level
during a single game, the level layout is the same. Some items
aid in exploration, e.g., a ring of searching improves the speed
of finding hidden doors by heroes and monsters. The higher the magical
bonus displayed for this and other items, the more effective it is.
Only the best item carried in a hero's or monster's inventory counts.
You can throw the rest away, but beware that your adversaries may pick it up
and use it against you.


Keys
----

You move throughout the level using the numerical keypad or
the vi text editor keys (also known as "Rogue-like keys").

               7 8 9     y k u
                \|/       \|/
               4-5-6     h-.-l
                /|\       /|\
               1 2 3     b j n

Shift and a movement key make the hero run in the indicated direction,
until anything of interest is spotted. '5' and '.' skip a turn.
(Note that If you are using the curses or vty frontends,
numerical keypad may not work correctly depending on the versions
of curses, terminfo and terminal emulators. Vi keys should work regardless.)
Melee, searching for secret doors and opening closed doors can be done
by bumping into a monster, a wall and a door, respectively.

Below are the default keys for major commands. Those of them that take
player time are marked with a *.

               key    command
               <      ascend a level*
               >      descend a level*
               ?      display help
               Q      quit without saving
               X      save and exit the game
               a      apply an applicable*
               c      close a door*
               d      drop an object*
               g      get an object*
               i      display inventory
               p      project a projectable*

To make a ranged attack, you need to set your target first, using
targeting mode. Note that the target, for the few commands that require any,
is indicated by the targeting cursor, but the origin of a command
--- the  hero that performs it --- is unaffacted by targeting. For example,
not the targeted door, but one adjacent to the selected hero is closed by him.
To avoid confusion, commands that take time are blocked when targeting
a remote level (when the cursor is on a different level than the selected hero).
The targeting commands and all the less used commands are listed below.
None of them takes hero time.

               key    command
               ESC    cancel action
               RET    accept choice
               SPACE  clear messages
               TAB    cycle among heroes on level
               *      target monster
               /      target location
               D      dump current configuration
               P      display previous messages
               V      display game version
               [      target next shallower level
               ]      target next deeper level
               {      target 10 levels shallower
               }      target 10 levels deeper
               0--9   select a hero anywhere in the spaceship (gtk only)

There are also some debug and cheat keys. Use at your own peril!

               key    command
               O      toggle "omniscience"
               I      inform about level meta-data
               R      rotate display modes


Monsters
--------

The hero is not alone in the spaceship. Monsters roam the dark halls
and crawl from damp air-ducts day and night. While heroes pay attention
to all other party members and take moves sequentially, one after another,
monsters don't care about each other and all move at once,
sometimes brutally colliding by mistake.

When the hero bumps into a monster or a monster attacks the hero,
melee combat occurs. The best weapon carried by each opponent
is taken into account for calculating bonus damage. The total damage
the current hero can potentially inflict is displayed at the bottom
of the screen. The total damage potential of a monster may change
as it finds and picks up new weapons. Heroes and monsters running
into another (with the Shift key) do not inflict damage, but change places.
This gives the opponent a free blow, but can improve the tactical situation
or aid escape.

Throwing weapons at targets wounds them, consuming the weapon in the process.
Target a monster with the '*' key from the top keboard row or from kepad.
You may throw any object in your possession (press '?' to choose
an object and press it again for a non-standard choice) or on the floor
(press '-'). Only objects of a few kinds inflict any damage.
Whenever a monster or a hero hit points reach zero, the combatant dies.
When the last hero dies, the game ends.


On Winning and Dying
--------------------

You win the game if you escape the spaceship alive. Your score is
the sum of all gold coins you've plundered plus 100 gold for each gem.
Only the loot in possession of the party members on level 1 counts
(the rest is considered MIA).

If all heroes die, your score is halved and only the treasure carried
by the last standing hero counts. You are free to start again
from the first level, but all your wealth and items
are gone and the spaceship and it's treasure look differently.
