Allure of the Stars
===================

[![Build Status](https://secure.travis-ci.org/AllureOfTheStars/Allure.svg?master)](https://travis-ci.org/AllureOfTheStars/Allure)
[![Hackage](https://img.shields.io/hackage/v/Allure.svg)](https://hackage.haskell.org/package/Allure)
[![Join the chat at Discord](https://img.shields.io/discord/688792755564052486.svg?label=chat%20on%20Discord&logo=discord&logoColor=ffffff&color=7389D8&labelColor=6A7EC2)](https://discord.gg/87Ghnws)
[![Join the chat at Matrix](https://img.shields.io/matrix/lambdahack:mozilla.org.svg?label=chat%20on%20Matrix&logo=matrix&server_fqdn=mozilla.modular.im)](https://matrix.to/#/!HnbpAHMjOGHlYtrASl:mozilla.org?via=mozilla.modular.im)

Allure of the Stars is a near-future Sci-Fi roguelike[2]
and tactical squad combat game. Binaries and the game manual
are available at the homepage[6]. You can also try the game out
in the browser at http://allureofthestars.com/play.

![gameplay screenshot](https://raw.githubusercontent.com/AllureOfTheStars/media/master/screenshot/allureofthestars.com.second.attempt.gif)

Not a single image in this game. You have to imagine everything
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
Please offer feedback to mikolaj.konarski@funktory.com or, preferably,
on any of the public forums.


Game installation from binary archives
--------------------------------------

The game runs rather slowly in the browser (fastest on Chrome) and you are
limited to the square font for all purposes, though it's scalable.
Also, savefiles are prone to corruption on the browser,
e.g., when it's closed while the game is still saving progress
(which takes a long time). Hence, after trying out the game,
you may prefer to use a native binary for your architecture, if it exists.

Pre-compiled game binaries are available through the release page[11]
(and, for Windows, dev versions continuously from AppVeyor[18]).
To use a pre-compiled binary archive, unpack it and run the executable
in the unpacked directory or use program shortcuts from the installer,
if available. On Linux, make sure you have the SDL2 libraries installed
on your system (e.g., libsdl2-2.0-0 and libsdl2-ttf-2.0-0 on Ubuntu).
For Windows (XP no longer supported), the SDL2 and all other needed libraries
are included in the game's binary archive.

Max OS X binaries for the few most popular OS versions are accessible
from Homebrew via `brew install allureofthestars`,
which also takes care of all dependencies.


Screen and keyboard configuration
---------------------------------

The game UI can be configured via a config file.
The default settings, the same that are built into the binary,
are on github at [GameDefinition/config.ui.default](https://github.com/AllureOfTheStars/Allure/blob/master/GameDefinition/config.ui.default).
When the game is run for the first time, or whenever the settings file
is deleted, the file is written to the default user data location,
which is `~/.Allure/` on Linux,
`C:\Users\<username>\AppData\Roaming\Allure\`
(or `C:\Documents And Settings\user\Application Data\Allure\`
or something else altogether) on Windows
and `Inspect/Application/Local Storage` under RMB menu
when run inside the Chrome browser.
If the user config file is outdated or corrupted, it's automatically
moved away together with old savefiles, which guarantees that the new
default config file is ultimately put in its place.

Screen fonts and, consequently, window size can be changed by editing
the config file in the user data folder. The default bitmap font
`16x16xw.bdf` used for the game map covers most national characters
in the Latin alphabet (e.g. to give custom names to player characters)
and results in a game window of exactly 720p HD dimensions. The `8x8xb.fnt`
bitmap font results in a tiny window and covers latin-1 characters only.
The config file parameter `allFontsScale` permits further window size
adjustments, automatically switching to the scalable `16x16xw.woff`
version of the game map font. Config file option `chosenFontset` governs
not only the main game map font, but also the shape of the rectangular fonts,
if any, in which longer texts are overlaid over the map.

For high resolution displays and/or if fullscreen mode is requested
in the configuration file, `allFontsScale` needs to be set.
E.g., scale 3 works for 4K displays. Otherwise, the letters may be
too small or, in fullscreen or on retina displays in OS X,
the screen may be automatically scaled as a whole, not each letter
separately, softening letter edges of the square fonts that should
be pixel-perfect and crisp.

If you don't have a numeric keypad, you can use the left-hand movement
key setup (axwdqezc) or Vi editor keys (aka roguelike keys) or mouse.
If numeric keypad doesn't work, toggling the Num Lock key sometimes helps.
If running with the Shift key and keypad keys doesn't work,
try the Control key instead. The game is fully playable with mouse only,
as well as with keyboard only, but the most efficient combination
may be mouse for menus, go-to, inspecting the map, aiming at distant
positions and keyboard for everything else.

If you are using a terminal frontend, e.g. the best supported vty frontend,
then numeric keypad (especially keypad `*` and `/`) may not work correctly,
depending on versions of libraries, terminfo and terminal emulators.
Toggling the Num Lock key may help or make issues worse. As a workaround,
in the vty frontend, numbers are used for movement, which sadly prevents
the number keys from selecting heroes. The commands that require pressing
Control and Shift together won't work either, but fortunately they are
not crucial to gameplay.

Some effort went into making the vty frontend usable with screen readers,
but without feedback it's hard to say how accessible that setup is.
As a side effect of screen reader support, there is no aiming line
nor path in vty frontend and some of map position highlighting
is performed using the terminal cursor. Screen readers may also work
better with animations turned off, using `--noAnim` or the corresponding
config file or main game menu options.


Compiling native binary from source
-----------------------------------

The recommended frontend is based on SDL2, so you need the SDL2 libraries
for your OS. On Linux, remember to install the -dev versions as well,
e.g., libsdl2-dev and libsdl2-ttf-dev on Ubuntu Linux 16.04.
Other frontends are compiled similarly, but compilation to JavaScript
for the browser is more complicated and requires the ghcjs[15] compiler
and optionally the Google Closure Compiler[16].

The latest official version of the game can be downloaded,
compiled for SDL2 and installed automatically using the Cabal tool,
which is already a part of your OS distribution, or available within
The Haskell Platform[7]. Get the game from Hackage[3] as follows

    cabal update
    cabal run Allure

For a newer, unofficial version, clone the game source from github[5],
clone a matching LambdaHack library snapshot into ../LambdaHack
and run `cabal run` from the main directory.
Alternatively, if you'd like to develop in this codebase,
the following speeds up the turn-around a lot

    cp cabal.project.local.development cabal.project.local
    cabal install cabal-plan

and then compile with

    cabal build .

and run the game with

    make play

There is a built-in black and white line terminal frontend, suitable
for teletype terminals or a keyboard and a printer (but it's going to use
a lot of paper, unless you disable animations with `--noAnim`). It is used
in CI and for some tests and benchmarks defined in Makefile. To compile
with one of the less rudimentary terminal frontends (in which case you are
on your own regarding font choice and color setup and you won't have
the spiffy colorful squares outlining special positions that exist in SDL2
frontend, but only crude cursor highlights), use Cabal flags, e.g,
to switch to the vty console frontend optimized for screen readers, run

    cabal run -fvty Allure


Testing and debugging
---------------------

The [Makefile](https://github.com/AllureOfTheStars/Allure/blob/master/Makefile)
contains many sample test commands.
Numerous tests that use the screensaver game modes (AI vs. AI)
and the teletype frontend are gathered in `make test`.
Of these, travis runs `test-travis` on each push to github.
Test commands with prefix `frontend` start AI vs. AI games with
the standard, user-friendly frontend and auto-locating the game binary.

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
from major modules, in particular for the modules that are interfaces
for a whole directory of modules. Apart of that, only very important
functions and types are distinguished by having a haddock.
If minor ones have comments, they should not be haddocks
and they are permitted to describe implementation details and be out of date.
Prefer assertions instead of comments, unless too verbose.

The 'pointman' from game manual and UI is called 'leader' in the source code
and there are a few more mismatches, though the source code naming
and the UI naming should each be consistent in separation.
If the UI names stick, perhaps source code will be renamed at some point.

This codebase is an experiment in extensive use of states without lens.
So far, it works, doesn't result in much larger files or lots
of repetition and has the added benefits that newcomers don't need
to learn any lens library. Record punning, etc., definitely helps.


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

Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)

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
If not, see <https://www.gnu.org/licenses/>.

Exceptions and detailed copyright information is contained in file COPYLEFT.


[1]: https://www.haskell.org/
[2]: http://roguebasin.roguelikedevelopment.org/index.php?title=Berlin_Interpretation
[3]: https://hackage.haskell.org/package/Allure
[4]: https://github.com/AllureOfTheStars/Allure/wiki
[5]: https://github.com/AllureOfTheStars/Allure
[6]: http://allureofthestars.com
[7]: https://www.haskell.org/platform


[10]: https://github.com/LambdaHack/LambdaHack
[11]: https://github.com/AllureOfTheStars/Allure/releases
[15]: https://github.com/ghcjs/ghcjs
[16]: https://www.npmjs.com/package/google-closure-compiler
[18]: https://ci.appveyor.com/project/Mikolaj/allure/build/artifacts
