Allure of the Stars
===================

[![Build Status](https://travis-ci.org/AllureOfTheStars/Allure.svg?branch=master)](https://travis-ci.org/AllureOfTheStars/Allure)
[![Hackage](https://img.shields.io/hackage/v/Allure.svg)](https://hackage.haskell.org/package/Allure)
[![Join the chat at https://gitter.im/AllureOfTheStars/Allure](https://badges.gitter.im/AllureOfTheStars/Allure.svg)](https://gitter.im/AllureOfTheStars/Allure?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Allure of the Stars is a near-future Sci-Fi roguelike[2]
and tactical squad game. Binaries and the game manual
are available at homepage[6], where you can also
try the game out in the browser at http://allureofthestars.com/play.
(It runs fastest on Chrome. Keyboard commands and savefiles
are supported only on recent enough versions of browsers.
Mouse should work everywhere.)

![gameplay screenshot](https://raw.githubusercontent.com/AllureOfTheStars/media/master/screenshot/allureofthestars.com.ambush.gif)

Not a single picture in this game. You have to imagine everything
yourself, like with a book (a grown-up book, without pictures).
Once you learn to imagine things, though, you can keep exploring
and mastering the world and making up stories for a long time.

The game is written in Haskell[1] using the LambdaHack[10]
roguelike game engine.
Please see the changelog file for recent improvements
and the issue tracker for short-term plans. Long term goals
are high replayability and auto-balancing through procedural
content generation and persistent content modification
based on player behaviour. Contributions are welcome.


Game installation from binary archives
--------------------------------------

The game runs rather slowly in the browser (fastest on Chrome)
and you are limited to only one font, though it's scalable.
Also, savefiles are prone to corruption on the browser,
e.g., when it's closed while the game is still saving progress
(which takes a long time). Hence, after trying out the game,
you may prefer to use a native binary for your architecture, if it exists.

Pre-compiled game binaries are available through
the release page[11] (and, for Windows, continuously from AppVeyor[18]).
To use a pre-compiled binary archive, unpack it and run the executable
in the unpacked directory or use program shortcuts from the installer,
if available.

On Linux, make sure you have the SDL2 libraries installed on your system
(e.g., libsdl2-2.0-0, libsdl2-ttf-2.0-0 on Ubuntu).
On Mac OS X, you need SDL2 installed, e.g.,
from [libsdlorg](https://www.libsdl.org/download-2.0.php).
For Windows, the SDL2 and all other needed libraries are already
contained in the game's binary archive.
Note that Windows binaries no longer work on Windows XP, since Cygwin
and MSYS2 dropped support for XP.


Screen and keyboard configuration
---------------------------------

The game UI can be configured via a config file.
The default settings, the same that are built into the binary,
are in [GameDefinition/config.ui.default](https://github.com/AllureOfTheStars/Allure/blob/master/GameDefinition/config.ui.default).
When the game is run for the first time, the file is copied to the default
user data folder, which is `~/.Allure/` on Linux,
`C:\Users\<username>\AppData\Roaming\Allure\`
(or `C:\Documents And Settings\user\Application Data\Allure\`
or something else altogether) on Windows, and in RMB menu, under
`Inspect/Application/Local Storage` when run inside the Chrome browser.

Screen font and consequently window size can be changed by editing
the config file in the user data folder. The default bitmap font
`16x16xw.bdf` covers most national characters in the Latin alphabet
(e.g. to give custom names to player characters) and results
in a game window of exactly 720p (standard HD) dimensions. The `8x8xb.fnt`
bitmap font results in a tiny window and covers latin-1 characters only.
The scalable `16x16xw.woff` font results in window sizes dependent
on the `fontSize` parameter in the config file. With `fontSize = 16`
it should look almost the same as the pixel-perfect `16x16xw.bdf`.

If you don't have a numeric keypad, you can use mouse for movement
or you can enable the compact laptop keys (uk8o79jl) or the Vi keys
(aka roguelike keys) in the config file. If numeric keypad doesn't work,
toggling the Num Lock key sometimes helps. If running with the Shift key
and keypad keys doesn't work, try Control key instead.
The game is fully playable with mouse only, as well as with keyboard only,
but the most efficient combination for some players is mouse for go-to,
inspecting, and aiming at distant positions and keyboard for everything else.


Compilation from source
-----------------------

If you want to compile native binaries from the source code,
use Cabal (already a part of your OS distribution, or available within
The Haskell Platform[7]), which also takes care of all the dependencies.

The recommended frontend is based on SDL2, so you need the SDL2 libraries
for your OS. On Linux, remember to install the -dev versions as well,
e.g., libsdl2-dev and libsdl2-ttf-dev on Ubuntu Linux 16.04.
(Compilation to JavaScript for the browser is more complicated
and requires the ghcjs[15] compiler and optionally the Google Closure
Compiler[16] as well.)

The latest official version of the game can be downloaded,
compiled for SDL2 and installed automatically by Cabal from Hackage[3]
as follows

    cabal update
    cabal install Allure

For a newer version, install a matching LambdaHack library snapshot,
clone the game source from github[5] and run `cabal install`
from the main directory.


Testing and debugging
---------------------

The [Makefile](https://github.com/AllureOfTheStars/Allure/blob/master/Makefile)
contains many sample test commands.
Numerous tests that use the screensaver game modes (AI vs. AI)
and the teletype frontend are gathered in `make test`.
Of these, travis runs `test-travis` on each push to github.
Test commands with prefix `frontend` start AI vs. AI games
with the standard, user-friendly frontend.

Run `Allure --help` to see a brief description of all debug options.
Of these, the `--sniff` option is very useful (though verbose
and initially cryptic), for displaying the traffic between clients
and the server. Some options in the config file may prove useful too,
though they mostly overlap with commandline options (and will be totally
merged at some point).


Coding style
------------

Stylish Haskell is used for slight auto-formatting at buffer save; see
[.stylish-haskell.yaml](https://github.com/AllureOfTheStars/Allure/blob/master/.stylish-haskell.yaml).
As defined in the file, indentation is 2 spaces wide and screen is
80-columns wide. Spaces are used, not tabs. Spurious whitespace avoided.
Spaces around arithmetic operators encouraged.
Generally, relax and try to stick to the style apparent in a file
you are editing. Put big formatting changes in separate commits.

Haddocks are provided for all module headers and for all functions and types
from major modules, in particular the modules that are interfaces
for a whole directory of modules. Apart of that, only very important
functions and types are distinguished by having a haddock.
If minor ones have comments, they should not be haddocks
and they are permitted to describe implementation details and be out of date.
Prefer assertions to comments, unless too verbose.


Further information
-------------------

For more information, visit the wiki[4]
and see [PLAYING.md](https://github.com/AllureOfTheStars/Allure/blob/master/GameDefinition/PLAYING.md),
[CREDITS](https://github.com/AllureOfTheStars/Allure/blob/master/CREDITS)
and [COPYLEFT](https://github.com/AllureOfTheStars/Allure/blob/master/COPYLEFT).

Have fun!


Copyright
---------

Copyright (c) 2008--2011 Andres Loeh

Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)

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

Exceptions and detailed copyright information is contained in file COPYLEFT.


[1]: http://www.haskell.org/
[2]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[3]: http://hackage.haskell.org/package/Allure
[4]: https://github.com/AllureOfTheStars/Allure/wiki
[5]: http://github.com/AllureOfTheStars/Allure
[6]: http://allureofthestars.com
[7]: http://www.haskell.org/platform


[10]: http://github.com/LambdaHack/LambdaHack
[11]: https://github.com/AllureOfTheStars/Allure/releases
[15]: https://github.com/ghcjs/ghcjs
[16]: https://www.npmjs.com/package/google-closure-compiler
[18]: https://ci.appveyor.com/project/Mikolaj/allure/build/artifacts
