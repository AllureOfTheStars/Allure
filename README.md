Allure of the Stars [![Build Status](https://secure.travis-ci.org/Mikolaj/Allure.png)](http://travis-ci.org/Mikolaj/Allure)
===================

This is an alpha release of Allure of the Stars,
a near-future Sci-Fi [roguelike] [2] and tactical squad game.
Long-term goals are high replayability and auto-balancing
through procedural content generation and persistent content
modification based on player behaviour. The game is written in [Haskell] [1]
using the [LambdaHack] [5] roguelike game engine.


Compilation and installation
----------------------------

The game is best compiled and installed via Cabal, which also takes care
of all dependencies. The latest official version of the game can be downloaded
automatically by Cabal from [Hackage] [4] as follows

    cabal install Allure

For a newer version, install a matching LambdaHack library snapshot
from a development branch, download the game source from [github] [3]
and run 'cabal install' from the main directory.


Compatibility notes
-------------------

The current code was tested with GHC 7.4.2, but should work with
later GHC versions as well.

If you are using the curses or vty frontends,
numerical keypad may not work correctly depending on the versions
of curses, terminfo and terminal emulators.
Selecting heroes via number keys or SHIFT-keypad keys is disabled
with curses, because CTRL-keypad for running does not work there,
so the numbers produced by the keypad have to be used. With vty on xterm,
CTRL-direction keys seem to work OK, but on rxvt they do not.
Vi keys (ykuhlbjn) should work everywhere regardless. Gtk works fine, too.


Further information
-------------------

For more information, visit the [wiki] [6]
and see the files PLAYING.md, CREDITS and LICENSE.

Have fun!


Copyright
---------

Copyright (c) 2008--2011 Andres Loeh, 2010--2013 Mikolaj Konarski

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
[6]: https://github.com/Mikolaj/Allure/wiki
