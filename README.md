Allure of the Stars
===================

This is an alpha pre-release of Allure of the Stars,
a near-future Sci-Fi [roguelike] [2] and tactical squad game.
Long-term goals are high replayability and auto-balancing
through procedural content generation and persistent content
modification based on player behaviour. The game is written in [Haskell] [1]
using the [LambdaHack roguelike game engine] [5].


Compilation and installation
----------------------------

The game is best compiled and installed via Cabal, which also takes care
of all dependencies. The latest official version of the game can be downloaded
automatically by Cabal from [Hackage] [4] as follows

    cabal install Allure

For a more current snapshot, download the source from [github] [3]
and run Cabal from the main directory

    cabal install

The best frontend (keyboard support and colours)
is gtk, but if needed, you may compile the game binary with one
of the terminal frontends using Cabal flags, e.g,

    cabal install -fvty

To use a crude bot for testing the game, you have to compile with
the standard input/output frontend, as follows

    cabal install -fstd

and run the bot, for example, in the following way

    DumbBot 42 20000000 | Allure > /tmp/log

You may wish to tweak the game configuration file to let the bot play longer,
e.g., by making the dungeon much deeper, as in the supplied config.bot.


Further information
-------------------

For more information, visit the wiki at https://github.com/Mikolaj/Allure/wiki
and see the files PLAYING.md, CREDITS and LICENSE.

Have fun!


Copyright
---------

Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
Allure of the Stars is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program in file LICENSE.
If not, see <http://www.gnu.org/licenses/>.




[1]: http://www.haskell.org/
[2]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[3]: http://github.com/Mikolaj/Allure
[4]: http://hackage.haskell.org/package/Allure
[5]: http://github.com/kosmikus/LambdaHack
