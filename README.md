Allure of the Stars [![Build Status](https://secure.travis-ci.org/Mikolaj/Allure.png)](http://travis-ci.org/Mikolaj/Allure)[![Build Status](https://drone.io/github.com/Mikolaj/Allure/status.png)](https://drone.io/github.com/Mikolaj/Allure/latest)
===================

This is an alpha release of Allure of the Stars,
a near-future Sci-Fi [roguelike] [2] and tactical squad game.
Long-term goals are high replayability and auto-balancing
through procedural content generation and persistent content
modification based on player behaviour. The game is written in [Haskell] [1]
using the [LambdaHack] [5] roguelike game engine.


Compilation and installation
----------------------------

The game is best compiled and installed via Cabal (already a part
of your OS distribution, or available within [The Haskell Platform] [7]),
which also takes care of all the dependencies. The latest official
version of the game can be downloaded automatically by Cabal
from [Hackage] [4] as follows

    cabal install Allure

For a newer version, install a matching LambdaHack library snapshot
from a development branch, download the game source from [github] [3]
and run `cabal install` from the main directory (for sample commands
see file `.travis.yml`).


Compatibility notes
-------------------

The current code was tested with GHC 7.6 and 7.8, but should also work with
other GHC versions (see file `.travis.yml.7.4.2` for GHC 7.4 commands).

If you are using the terminal frontends, numerical keypad may not work
correctly depending on versions of the libraries, terminfo and terminal
emulators. The curses frontend is not fully supported due to the limitations
of the curses library. With the vty frontend run in an xterm,
CTRL-keypad keys for running seem to work OK, but on rxvt they do not.
Vi keys (ykuhlbjn) should work everywhere regardless. GTK works fine, too.


Testing and debugging
---------------------

The `Makefile` contains many sample test commands. All that use the screensaver
game modes (AI vs. AI) and the simplest stdout frontend are gathered
in `make test`. Of these, travis runs one of the sets prefixed
`test-travis` on each push to the repo. Commands with prefix
`frontend` run AI vs. AI games with the standard, user-friendly frontend.
Commands with prefix `peek` set up a game mode where the player peeks
into AI moves each time an AI actor dies or autosave kicks in.
Run `Allure --help` to see a brief description of all debug options.
Of these, `--sniffIn` and `--sniffOut` are very useful (though verbose
and initially cryptic), for monitoring the traffic between clients
and the server. Some options in the config file may turn out useful too,
though they mostly overlap with commandline options (and will be totally
merged at some point).


Further information
-------------------

For more information, visit the [wiki] [6]
and see `GameDefinition/PLAYING.md`, `CREDITS` and `LICENSE`.

Have fun!


Copyright
---------

Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski

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
[7]: http://www.haskell.org/platform
