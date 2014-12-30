# Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
# This file is a part of the computer game Allure of the Stars
# and is released under the terms of the GNU Affero General Public License.
# For license and copyright information, see the file LICENSE.
#

# All xc* tests assume a profiling build (for stack traces).
# See the install-debug target below or .travis.yml.prof.

install-debug:
	cabal install --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization

configure-debug:
	cabal configure --enable-library-profiling --enable-executable-profiling --ghc-options="-fprof-auto-calls" --disable-optimization


xcplay:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --dumpInitRngs

xcfrontendCampaign:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode campaign --difficulty 2

xcfrontendSkirmish:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode skirmish

xcfrontendAmbush:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode ambush

xcfrontendBattle:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode battle --difficulty 2

xcfrontendSafari:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode safari --difficulty 2

xcfrontendDefense:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode defense --difficulty 8


play:
	dist/build/Allure/Allure --dbgMsgSer --dumpInitRngs

frontendCampaign:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode campaign --difficulty 2

frontendSkirmish:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode skirmish

frontendAmbush:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode ambush

frontendBattle:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode battle --difficulty 2

frontendSafari:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode safari --difficulty 2

frontendDefense:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 60 --dumpInitRngs --automateAll --gameMode defense --difficulty 8

benchCampaign:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchBattle:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchFrontendCampaign:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --benchmark --stopAfter 60 --automateAll --gameMode campaign --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchFrontendBattle:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --benchmark --stopAfter 60 --automateAll --gameMode battle --difficulty 2 --setDungeonRng 42 --setMainRng 42 +RTS -N1 -RTS

benchNull: benchCampaign benchBattle

bench: benchCampaign benchFrontendCampaign benchBattle benchFrontendBattle


test-travis-short: test-short

test-travis-medium: test-short test-medium

test-travis-medium-no-safari: test-short test-medium-no-safari

test-travis-long: test-short test-long

test-travis-long-no-safari: test-short test-long-no-safari

test: test-short test-medium test-long

test-short: test-short-new test-short-load

test-medium: testCampaign-medium testSkirmish-medium testAmbush-medium testBattle-medium testSafari-medium testPvP-medium testCoop-medium testDefense-medium

test-medium-no-safari: testCampaign-medium testSkirmish-medium testAmbush-medium testBattle-medium testPvP-medium testCoop-medium testDefense-medium

test-long: testCampaign-long testSkirmish-medium testAmbush-medium testBattle-long testSafari-long testPvP-medium testDefense-long

test-long-no-safari: testCampaign-long testSkirmish-medium testAmbush-medium testBattle-long testPvP-medium testDefense-long

testCampaign-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 500 --dumpInitRngs --automateAll --gameMode campaign --difficulty 2 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode campaign --difficulty 2 > /tmp/stdtest.log

testSkirmish-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

testSkirmish-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode skirmish > /tmp/stdtest.log

testAmbush-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode ambush > /tmp/stdtest.log

testAmbush-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode ambush > /tmp/stdtest.log

testBattle-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 200 --dumpInitRngs --automateAll --gameMode battle --difficulty 2 > /tmp/stdtest.log

testBattle-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 100 --dumpInitRngs --automateAll --gameMode battle --difficulty 2 > /tmp/stdtest.log

testSafari-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 500 --dumpInitRngs --automateAll --gameMode safari --difficulty 2 > /tmp/stdtest.log

testSafari-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 400 --dumpInitRngs --automateAll --gameMode safari --difficulty 2 > /tmp/stdtest.log

testPvP-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 60 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

testPvP-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 30 --dumpInitRngs --automateAll --gameMode PvP > /tmp/stdtest.log

testCoop-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 500 --dumpInitRngs --automateAll --gameMode Coop --difficulty 2 > /tmp/stdtest.log

testCoop-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 300 --dumpInitRngs --automateAll --gameMode Coop --difficulty 2 > /tmp/stdtest.log

testDefense-long:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 500 --dumpInitRngs --automateAll --gameMode defense --difficulty 8 > /tmp/stdtest.log

testDefense-medium:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix test --newGame --noDelay --noAnim --maxFps 100000 --frontendStd --benchmark --stopAfter 300 --dumpInitRngs --automateAll --gameMode defense --difficulty 8 > /tmp/stdtest.log

test-short-new:
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix campaign --dumpInitRngs --automateAll --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix skirmish --dumpInitRngs --automateAll --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix ambush --dumpInitRngs --automateAll --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix battle --dumpInitRngs --automateAll --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix safari --dumpInitRngs --automateAll --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix PvP --dumpInitRngs --automateAll --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix Coop --dumpInitRngs --automateAll --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix defense --dumpInitRngs --automateAll --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log

test-short-load:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix campaign --dumpInitRngs --automateAll --gameMode campaign --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --savePrefix skirmish --dumpInitRngs --automateAll --gameMode skirmish --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --savePrefix ambush --dumpInitRngs --automateAll --gameMode ambush --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --savePrefix battle --dumpInitRngs --automateAll --gameMode battle --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --savePrefix safari --dumpInitRngs --automateAll --gameMode safari --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --savePrefix PvP --dumpInitRngs --automateAll --gameMode PvP --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --savePrefix Coop --dumpInitRngs --automateAll --gameMode Coop --frontendStd --stopAfter 2 > /tmp/stdtest.log
	dist/build/Allure/Allure --dbgMsgSer --savePrefix defense --dumpInitRngs --automateAll --gameMode defense --frontendStd --stopAfter 2 > /tmp/stdtest.log


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

# TODO: figure out, whey this must be so different from Linux
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


# The rest of the makefile is unmaintained at the moment.

default : dist/setup-config
	runghc Setup build

dist/setup-config : Allure.cabal
	runghc Setup configure --user

clean :
	runghc Setup clean

ghci :
	ghci -XCPP -idist/build/autogen:GameDefinition
