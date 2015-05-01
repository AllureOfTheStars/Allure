Allure of the Stars [![Build Status](https://travis-ci.org/AllureOfTheStars/Allure.svg?branch=master)](https://travis-ci.org/AllureOfTheStars/Allure)[![Build Status](https://drone.io/github.com/AllureOfTheStars/Allure/status.png)](https://drone.io/github.com/AllureOfTheStars/Allure/latest)
===================

[Allure of the Stars] [6] is a near-future Sci-Fi [roguelike] [2]
and tactical squad game. Have a look at [PLAYING.md](GameDefinition/PLAYING.md)
or jump straight into the fray.

![gameplay screenshot](GameDefinition/screenshot.png?raw=true)

The game is written in [Haskell] [1] using the [LambdaHack] [10]
roguelike game engine. Long-term goals of the project are high
replayability and auto-balancing through procedural content generation
and persistent content modification based on player behaviour.


Installation from binary archives
---------------------------------

Pre-compiled game binaries for some platforms are available through
the [release page] [11] and from the [Nix Packages Collection] [12].
To manually install a binary archive, make sure you have the GTK
libraries suite on your system, unpack the Allure archive
and run the executable in the unpacked directory.

On Windows, if you don't already have GTK installed (e.g., for the GIMP
picture editor) please download and run (with default settings)
the GTK installer from

http://sourceforge.net/projects/gtk-win/


Screen and keyboard configuration
---------------------------------

The game UI can be configured via a config file.
A file with the default settings, the same as built into the binary, is in
[GameDefinition/config.ui.default](GameDefinition/config.ui.default).
When the game is run for the first time, the file is copied to the official
location, which is `~/.Allure/config.ui.ini` on Linux and
`C:\Users\<username>\AppData\Roaming\Allure\config.ui.ini`
(or `C:\Documents And Settings\user\Application Data\Allure\config.ui.ini`
or something else altogether) on Windows.

Screen font can be changed and enlarged by editing the config file
 at its official location or by CTRL-right-clicking on the game window.

If you use the numeric keypad, use the NumLock key on your keyboard
to toggle the game keyboard mode. With NumLock off, you walk with the numeric
keys and run with SHIFT (or CONTROL) and the keys. This mode is probably
the best if you use mouse for running. When you turn NumLock on,
the reversed key setup enforces good playing habits by setting as the default
the run command (which automatically stops at threats, keeping you safe)
and requiring SHIFT (or CONTROL) for the error-prone step by step walking.

If you don't have the numeric keypad, you can use laptop keys (uk8o79jl)
or you can enable the Vi keys (aka roguelike keys) in the config file.


Compilation from source
-----------------------

If you want to compile your own binaries from the source code,
use Cabal (already a part of your OS distribution, or available within
[The Haskell Platform] [7]), which also takes care of all the dependencies.
You also need the GTK libraries for your OS. On Linux, remember to install
the -dev versions as well. On Windows follow [the same steps as for Wine] [13].
On OSX, if you encounter problems, you may want to
[compile the GTK libraries from sources] [14].

The latest official version of the game can be downloaded,
compiled and installed automatically by Cabal from [Hackage] [3] as follows

    cabal update
    cabal install gtk2hs-buildtools
    cabal install Allure --force-reinstalls

For a newer version, install a matching LambdaHack library snapshot
from a development branch, download the game source from [github] [5]
and run `cabal install` from the main directory.

Compatibility notes
-------------------

If you are using a terminal frontend, numeric keypad may not work
correctly depending on versions of the libraries, terminfo and terminal
emulators. The curses frontend is not fully supported due to the limitations
of the curses library. With the vty frontend started in an xterm,
CTRL-keypad keys for running seem to work OK, but on rxvt they do not.
The commands that require pressing CTRL and SHIFT together won't
work either, but fortunately they are not crucial to gameplay.
For movement, laptop (uk8o79jl) and Vi keys (hjklyubn, if enabled
in config.ui.ini) should work everywhere. GTK works fine, too, both
with numeric keypad and with mouse.


Testing and debugging
---------------------

The [Makefile](Makefile) contains many sample test commands.
Numerous tests that use the screensaver game modes (AI vs. AI)
and the dumb `stdout` frontend are gathered in `make test`.
Of these, travis runs `test-travis-*` on each push to the repo.
Test commands with prefix `frontend` start AI vs. AI games
with the standard, user-friendly gtk frontend.

Run `Allure --help` to see a brief description of all debug options.
Of these, `--sniffIn` and `--sniffOut` are very useful (though verbose
and initially cryptic), for monitoring the traffic between clients
and the server. Some options in the config file may prove useful too,
though they mostly overlap with commandline options (and will be totally
merged at some point).


Further information
-------------------

For more information, visit the [wiki] [4]
and see [PLAYING.md](GameDefinition/PLAYING.md), [CREDITS](CREDITS)
and [LICENSE](LICENSE).

Have fun!


Copyright
---------

Copyright (c) 2008--2011 Andres Loeh, 2010--2015 Mikolaj Konarski

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
[3]: http://hackage.haskell.org/package/Allure
[4]: https://github.com/AllureOfTheStars/Allure/wiki
[5]: http://github.com/AllureOfTheStars/Allure
[6]: http://allureofthestars.com
[7]: http://www.haskell.org/platform

[10]: http://github.com/LambdaHack/LambdaHack
[11]: https://github.com/AllureOfTheStars/Allure/releases/latest
[12]: http://hydra.cryp.to/search?query=Allure
[13]: http://www.haskell.org/haskellwiki/GHC_under_Wine#Code_that_uses_gtk2hs
[14]: http://www.edsko.net/2014/04/27/haskell-including-gtk-on-mavericks
