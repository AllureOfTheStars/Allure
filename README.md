Allure of the Stars
===================

[![Build Status](https://travis-ci.org/AllureOfTheStars/Allure.svg?branch=master)](https://travis-ci.org/AllureOfTheStars/Allure)
[![Hackage](https://img.shields.io/hackage/v/Allure.svg)](https://hackage.haskell.org/package/Allure)
[![Join the chat at https://gitter.im/AllureOfTheStars/Allure](https://badges.gitter.im/AllureOfTheStars/Allure.svg)](https://gitter.im/AllureOfTheStars/Allure?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Allure of the Stars[6] is a near-future Sci-Fi roguelike[2]
and tactical squad game.
Try out the browser version of the LambdaHack sample game at
[http://allureofthestars.com/play](http://allureofthestars.com/play)!
(It runs fastest on Chrome. Keyboard commands and savefiles
are supported only on recent enough versions of browsers.
Mouse should work everywhere.)

![gameplay screenshot](https://raw.githubusercontent.com/AllureOfTheStars/media/master/screenshot/crawl-0.6.0.0-8x8xb.png)

Please see the changelog file for recent improvements
and the issue tracker for short-term plans. Long term goals
are high replayability and auto-balancing through procedural
content generation and persistent content modification
based on player behaviour. Contributions are welcome.

The game is written in Haskell[1] using the LambdaHack [10]
roguelike game engine. Long-term goals of the project are high
replayability and auto-balancing through procedural content generation
and persistent content modification based on player behaviour.


Game installation from binary archives
--------------------------------------

The game runs rather slowly in the browser (fastest on Chrome)
and you are limited to only one font, though it's scalable.
Keyboard input and saving game progress requires recent enough
version of a browser (but mouse input is enough to play the game).
Also, savefiles are prone to corruption on the browser,
e.g., when it's closed while the game is still saving progress
(which takes a long time). Hence, after trying out the game,
you may prefer to use a native binary for your architecture, if it exists.

Pre-compiled game binaries for some platforms are available through
the release page[11] and from the Nix Packages Collection[12].
To use a pre-compiled binary archive, unpack it and run the executable
in the unpacked directory.

On Linux, make sure you have the SDL2 libraries suite installed on your system
(e.g., libsdl2, libsdl2-ttf). For Windows, the SDL2 library is already
contained in the game's binary archive.


Screen and keyboard configuration
---------------------------------

The game UI can be configured via a config file.
A file with the default settings, the same that is built into the binary,
is in [GameDefinition/config.ui.default](GameDefinition/config.ui.default).
When the game is run for the first time, the file is copied to the default
user data folder, which is `~/.Allure/` on Linux,
`C:\Users\<username>\AppData\Roaming\Allure\`
(or `C:\Documents And Settings\user\Application Data\Allure\`
or something else altogether) on Windows, and in RMB menu, under
`Inspect/Application/Local Storage` when run inside the Chrome browser.

Screen font can be changed by editing the config file in the user
data folder. For a small game window, the highly optimized
bitmap fonts 16x16x.fon, 8x8x.fon and 8x8xb.fon are the best,
but for larger window sizes or if you require international characters
(e.g. to give custom names to player characters), a modern scalable font
supplied with the game is the only option. The game window automatically
scales according to the specified font size. Display on SDL2
and in the browser is superior to all the other frontends,
due to custom square font and less intrusive ways of highlighting
interesting squares.

If you don't have a numeric keypad, you can use mouse or laptop keys
(uk8o79jl) for movement or you can enable the Vi keys (aka roguelike keys)
in the config file. If numeric keypad doesn't work, toggling
the Num Lock key sometimes helps. If running with the Shift key
and keypad keys doesn't work, try Control key instead.
The game is fully playable with mouse only, as well as with keyboard only,
but the most efficient combination for some players is mouse for go-to,
inspecting, and aiming at distant positions and keyboard for everything else.

If you are using a terminal frontend, numeric keypad may not work
correctly depending on versions of the libraries, terminfo and terminal
emulators. Toggling the Num Lock key may help.
The curses frontend is not fully supported due to the limitations
of the curses library. With the vty frontend started in an xterm,
Control-keypad keys for running seem to work OK, but on rxvt they do not.
The commands that require pressing Control and Shift together won't
work either, but fortunately they are not crucial to gameplay.


Compilation from source
-----------------------

If you want to compile native binaries from the source code,
use Cabal (already a part of your OS distribution, or available within
The Haskell Platform[7]), which also takes care of all the dependencies.

The recommended frontend is based on SDL2, so you need the SDL2 libraries
for your OS. On Linux, remember to install the -dev versions as well,
e.g., libsdl2-dev and libsdl2-ttf-dev on Ubuntu Linux 16.04.
(Compilation to Javascript for the browser is more complicated
and requires the ghcjs[15] compiler and optionally the Google Closure
Compiler[16] as well. See the [Makefile](Makefile) for more details.)

The latest official version of the game can be downloaded,
compiled for SDL2 and installed automatically by Cabal from Hackage[3]
as follows

    cabal update
    cabal install Allure --force-reinstalls

For a newer version, install a matching LambdaHack library snapshot
from a development branch, download the game source from [github] [5]
and run `cabal install` from the main directory.


Testing and debugging
---------------------

The [Makefile](Makefile) contains many sample test commands.
Numerous tests that use the screensaver game modes (AI vs. AI)
and the teletype frontend are gathered in `make test`.
Of these, travis runs `test-travis` on each push to github.
Test commands with prefix `frontend` start AI vs. AI games
with the standard, user-friendly frontend.

Run `Allure --help` to see a brief description of all debug options.
Of these, `--sniffIn` and `--sniffOut` are very useful (though verbose
and initially cryptic), for monitoring the traffic between clients
and the server. Some options in the config file may prove useful too,
though they mostly overlap with commandline options (and will be totally
merged at some point).


Further information
-------------------

For more information, visit the wiki[4]
and see [PLAYING.md](GameDefinition/PLAYING.md), [CREDITS](CREDITS)
and [LICENSE](LICENSE).

Have fun!


Copyright
---------

Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski

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
[12]: http://hydra.nixos.org/search?query=Allure
[13]: http://www.haskell.org/haskellwiki/GHC_under_Wine#Code_that_uses_gtk2hs
[14]: http://www.edsko.net/2014/04/27/haskell-including-gtk-on-mavericks
[15]: https://github.com/ghcjs/ghcjs
[16]: https://www.npmjs.com/package/google-closure-compiler
