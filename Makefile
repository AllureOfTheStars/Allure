# Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
# This file is a part of the computer game Allure of the Stars
# and is released under the terms of the GNU Affero General Public License.
# For license and copyright information, see the file LICENSE.
#

play:
	dist/build/Allure/Allure --dbgMsgSer --dumpInitRngs

configure-debug:
	cabal configure --enable-profiling --profiling-detail=all-functions -fwith_expensive_assertions --disable-optimization

configure-prof:
	cabal configure --enable-profiling --profiling-detail=exported-functions -frelease

ghcjs-configure:
	cabal configure --disable-library-profiling --disable-profiling --ghcjs --ghcjs-option=-dedupe -f-release

prof-ghcjs:
	cabal configure --enable-profiling --ghc-option=-fprof-auto-exported --ghcjs --ghcjs-option=-dedupe -frelease

chrome-prof:
	google-chrome --no-sandbox --js-flags="--logfile=%t.log --prof" dist/build/Allure/Allure.jsexe/index.html

minific:
	java -jar ~/Downloads/closure-compiler.jar dist/build/Allure/Allure.jsexe/all.js --compilation_level=ADVANCED_OPTIMIZATIONS > ~/Downloads/all.js


frontendRaid:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode raid

frontendBrawl:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode brawl

frontendShootout:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode shootout

frontendEscape:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 3 --dumpInitRngs --automateAll --gameMode escape

frontendZoo:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode zoo

frontendAmbush:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode ambush

frontendExploration:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 1 --dumpInitRngs --automateAll --gameMode exploration

frontendSafari:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 2 --dumpInitRngs --automateAll --gameMode safari

frontendSafariSurvival:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode "safari survival"

frontendBattle:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode battle

frontendBattleSurvival:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 5 --dumpInitRngs --automateAll --gameMode "battle survival"

frontendDefense:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix test --newGame 9 --dumpInitRngs --automateAll --gameMode defense


benchMemoryAnim:
	dist/build/Allure/Allure --dbgMsgSer --newGame 1 --maxFps 100000 --benchmark --stopAfterFrames 33000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 120 --setMainRng 47 --frontendNull --noAnim +RTS -s -A1M -RTS

benchBattle:
	dist/build/Allure/Allure --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchAnimBattle:
	dist/build/Allure/Allure --dbgMsgSer --newGame 3 --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchFrontendBattle:
	dist/build/Allure/Allure --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 1500 --automateAll --keepAutomated --gameMode battle --setDungeonRng 7 --setMainRng 7

benchExploration:
	dist/build/Allure/Allure --dbgMsgSer --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

benchFrontendExploration:
	dist/build/Allure/Allure --dbgMsgSer --newGame 1 --noAnim --maxFps 100000 --benchmark --stopAfterFrames 7000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

benchNull: benchBattle benchAnimBattle benchExploration

bench:  benchBattle benchAnimBattle benchFrontendBattle benchExploration benchFrontendExploration

nativeBenchExploration:
	dist/build/Allure/Allure                   --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

nativeBenchBattle:
	dist/build/Allure/Allure                   --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nativeBench: nativeBenchBattle nativeBenchExploration

nodeBenchExploration:
	node dist/build/Allure/Allure.jsexe/all.js --dbgMsgSer --newGame 2 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 2000 --automateAll --keepAutomated --gameMode exploration --setDungeonRng 0 --setMainRng 0

nodeBenchBattle:
	node dist/build/Allure/Allure.jsexe/all.js --dbgMsgSer --newGame 3 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 1000 --automateAll --keepAutomated --gameMode battle --setDungeonRng 0 --setMainRng 0

nodeBench: nodeBenchBattle nodeBenchExploration


test-travis-short: test-short

test-travis-medium: test-short test-medium benchNull

test: test-short test-medium benchNull

test-short: test-short-new test-short-load

test-medium: testRaid-medium testBrawl-medium testShootout-medium testEscape-medium testZoo-medium testAmbush-medium testExploration-medium testSafari-medium testSafariSurvival-medium testBattle-medium testBattleSurvival-medium

testRaid-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode raid 2> /tmp/teletypetest.log

testBrawl-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode brawl 2> /tmp/teletypetest.log

testShootout-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode shootout 2> /tmp/teletypetest.log

testEscape-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 3 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 40 --dumpInitRngs --automateAll --keepAutomated --gameMode escape 2> /tmp/teletypetest.log

testZoo-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 2 --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode zoo 2> /tmp/teletypetest.log

testAmbush-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode ambush 2> /tmp/teletypetest.log

testExploration-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 1 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 200 --dumpInitRngs --automateAll --keepAutomated --gameMode exploration 2> /tmp/teletypetest.log

testSafari-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 2 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 100 --dumpInitRngs --automateAll --keepAutomated --gameMode safari 2> /tmp/teletypetest.log

testSafariSurvival-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 8 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" 2> /tmp/teletypetest.log

testBattle-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 3 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 20 --dumpInitRngs --automateAll --keepAutomated --gameMode battle 2> /tmp/teletypetest.log

testBattleSurvival-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 7 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 60 --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" 2> /tmp/teletypetest.log

testDefense-medium:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 9 --noAnim --maxFps 100000 --frontendTeletype --benchmark --stopAfterSeconds 500 --dumpInitRngs --automateAll --keepAutomated --gameMode defense 2> /tmp/teletypetest.log

test-short-new:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootout --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix exploration --dumpInitRngs --automateAll --keepAutomated --gameMode exploration --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --newGame 5 --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log

test-short-load:
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix raid --dumpInitRngs --automateAll --keepAutomated --gameMode raid --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix brawl --dumpInitRngs --automateAll --keepAutomated --gameMode brawl --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix shootout --dumpInitRngs --automateAll --keepAutomated --gameMode shootouti --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix escape --dumpInitRngs --automateAll --keepAutomated --gameMode escape --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix zoo --dumpInitRngs --automateAll --keepAutomated --gameMode zoo --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix ambush --dumpInitRngs --automateAll --keepAutomated --gameMode ambush --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix exploration --dumpInitRngs --automateAll --keepAutomated --gameMode exploration --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix safari --dumpInitRngs --automateAll --keepAutomated --gameMode safari --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix safariSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "safari survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix battle --dumpInitRngs --automateAll --keepAutomated --gameMode battle --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix battleSurvival --dumpInitRngs --automateAll --keepAutomated --gameMode "battle survival" --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log
	dist/build/Allure/Allure --dbgMsgSer --boostRandomItem --savePrefix defense --dumpInitRngs --automateAll --keepAutomated --gameMode defense --frontendTeletype --stopAfterSeconds 2 2> /tmp/teletypetest.log


build-binary:
	cabal configure -frelease --prefix=/
	cabal build Allure
	rm -rf /tmp/Allure_x_ubuntu-12.04-amd64.tar.gz
	rm -rf /tmp/AllureOfTheStarsInstall
	rm -rf /tmp/AllureOfTheStars
	mkdir -p /tmp/AllureOfTheStars/GameDefinition
	cabal copy --destdir=/tmp/AllureOfTheStarsInstall
	cp /tmp/AllureOfTheStarsInstall/bin/Allure /tmp/AllureOfTheStars
	cp GameDefinition/PLAYING.md /tmp/AllureOfTheStars/GameDefinition
	cp GameDefinition/scores /tmp/AllureOfTheStars/GameDefinition
	cp GameDefinition/config.ui.default /tmp/AllureOfTheStars/GameDefinition
	cp CHANGELOG.md /tmp/AllureOfTheStars
	cp CREDITS /tmp/AllureOfTheStars
	cp LICENSE /tmp/AllureOfTheStars
	cp README.md /tmp/AllureOfTheStars
	tar -czf /tmp/Allure_x_ubuntu-12.04-amd64.tar.gz -C /tmp AllureOfTheStars

build-binary-i386:
	cabal configure -frelease --prefix=/ --ghc-option="-optc-m32" --ghc-option="-opta-m32" --ghc-option="-optl-m32" --ld-option="-melf_i386"
	cabal build Allure
	rm -rf /tmp/Allure_x_ubuntu-12.04-i386.tar.gz
	rm -rf /tmp/AllureOfTheStarsInstall
	rm -rf /tmp/AllureOfTheStars
	mkdir -p /tmp/AllureOfTheStars/GameDefinition
	cabal copy --destdir=/tmp/AllureOfTheStarsInstall
	cp /tmp/AllureOfTheStarsInstall/bin/Allure /tmp/AllureOfTheStars
	cp GameDefinition/PLAYING.md /tmp/AllureOfTheStars/GameDefinition
	cp GameDefinition/scores /tmp/AllureOfTheStars/GameDefinition
	cp GameDefinition/config.ui.default /tmp/AllureOfTheStars/GameDefinition
	cp CHANGELOG.md /tmp/AllureOfTheStars
	cp CREDITS /tmp/AllureOfTheStars
	cp LICENSE /tmp/AllureOfTheStars
	cp README.md /tmp/AllureOfTheStars
	tar -czf /tmp/Allure_x_ubuntu-12.04-i386.tar.gz -C /tmp AllureOfTheStars

# TODO: figure out why this must be so different from Linux
build-binary-windows-i386:
	wine cabal configure -frelease
	wine cabal build exe:Allure
	rm -rf /tmp/Allure_x_windows-i386.zip
	rm -rf /tmp/AllureOfTheStarsInstall
	rm -rf /tmp/AllureOfTheStars
	mkdir -p /tmp/AllureOfTheStars/GameDefinition
	wine cabal copy --destdir=Z:/tmp/AllureOfTheStarsInstall
	cp /tmp/AllureOfTheStarsInstall/users/mikolaj/Application\ Data/cabal/bin/Allure.exe /tmp/AllureOfTheStars
	cp GameDefinition/PLAYING.md /tmp/AllureOfTheStars/GameDefinition
	cp GameDefinition/scores /tmp/AllureOfTheStars/GameDefinition
	cp GameDefinition/config.ui.default /tmp/AllureOfTheStars/GameDefinition
	cp CHANGELOG.md /tmp/AllureOfTheStars
	cp CREDITS /tmp/AllureOfTheStars
	cp LICENSE /tmp/AllureOfTheStars
	cp README.md /tmp/AllureOfTheStars
	cp /home/mikolaj/.wine/drive_c/users/mikolaj/gtk/bin/zlib1.dll /tmp/AllureOfTheStars
	wine Z:/home/mikolaj/.local/share/wineprefixes/7zip/drive_c/Program\ Files/7-Zip/7z.exe a -ssc -sfx Z:/tmp/Allure_x_windows-i386.exe Z:/tmp/AllureOfTheStars
