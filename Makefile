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
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer

xcpeekCampaign:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign

xcpeekSkirmish:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish

xcfrontendCampaign:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testCampaign --difficulty 1

xcfrontendSkirmish:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testSkirmish

xcfrontendBattle:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testBattle --difficulty 1

xcfrontendPvP:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testPvP --fovMode Shadow

xcfrontendCoop:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testCoop --difficulty 1 --fovMode Permissive

xcfrontendDefense:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testDefense --difficulty 9

xcbenchCampaign:
	time dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --frontendStd --stopAfter 60 --gameMode testCampaign --difficulty 1 --setDungeonRng 42 --setMainRng 42 > /dev/null

xcbenchBattle:
	time dist/build/LambdaHack/LambdaHack +RTS -xc -RTS --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --frontendStd --stopAfter 60 --gameMode testBattle --difficulty 1 --setDungeonRng 42 --setMainRng 42 > /dev/null


xctest-travis: xctest-short xctest-medium

xctest-travis-long: xctest-short xctest-long

xctest: xctest-short xctest-medium xctest-long

xctest-short: xctest-short-new xctest-short-load

xctest-medium: xctestCampaign-medium xctestSkirmish-medium xctestBattle-medium xctestPvP-medium xctestCoop-medium xctestDefense-medium

xctest-long: xctestCampaign-long xctestCoop-long xctestDefense-long

xctestCampaign-long:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testCampaign --difficulty 1 > /tmp/stdtest.log

xctestCampaign-medium:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testCampaign --difficulty 1 > /tmp/stdtest.log

xctestSkirmish-long:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testSkirmish > /tmp/stdtest.log

xctestSkirmish-medium:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testSkirmish > /tmp/stdtest.log

xctestBattle-long:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testBattle --difficulty 1 > /tmp/stdtest.log

xctestBattle-medium:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testBattle --difficulty 1 > /tmp/stdtest.log

xctestPvP-long:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testPvP > /tmp/stdtest.log

xctestPvP-medium:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testPvP > /tmp/stdtest.log

xctestCoop-long:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testCoop --difficulty 1 > /tmp/stdtest.log

xctestCoop-medium:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testCoop --difficulty 1 > /tmp/stdtest.log

xctestDefense-long:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testDefense --difficulty 9 > /tmp/stdtest.log

xctestDefense-medium:
	dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testDefense --difficulty 9 > /tmp/stdtest.log

xctest-short-new:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix battle --gameMode battle --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --newGame --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log

xctest-short-load:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix battle --gameMode battle --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure +RTS -xc -RTS --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log


play:
	dist/build/Allure/Allure --dbgMsgSer

peekCampaign:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign

peekSkirmish:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish

frontendCampaign:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testCampaign --difficulty 1

frontendSkirmish:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testSkirmish

frontendBattle:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testBattle --difficulty 1

frontendPvP:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testPvP --fovMode Shadow

frontendCoop:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testCoop --difficulty 1 --fovMode Permissive

frontendDefense:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testDefense --difficulty 9

benchCampaign:
	time dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --frontendStd --stopAfter 60 --gameMode testCampaign --difficulty 1 --setDungeonRng 42 --setMainRng 42 > /dev/null

benchBattle:
	time dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --frontendStd --stopAfter 60 --gameMode testBattle --difficulty 1 --setDungeonRng 42 --setMainRng 42 > /dev/null


test-travis: test-short test-medium

test-travis-long: test-short test-long

test: test-short test-medium test-long

test-short: test-short-new test-short-load

test-medium: testCampaign-medium testSkirmish-medium testBattle-medium testPvP-medium testCoop-medium testDefense-medium

test-long: testCampaign-long testCoop-long testDefense-long

testCampaign-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testCampaign --difficulty 1 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testCampaign --difficulty 1 > /tmp/stdtest.log

testSkirmish-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testSkirmish > /tmp/stdtest.log

testSkirmish-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testSkirmish > /tmp/stdtest.log

testBattle-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testBattle --difficulty 1 > /tmp/stdtest.log

testBattle-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testBattle --difficulty 1 > /tmp/stdtest.log

testPvP-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testPvP > /tmp/stdtest.log

testPvP-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testPvP > /tmp/stdtest.log

testCoop-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testCoop --difficulty 1 > /tmp/stdtest.log

testCoop-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testCoop --difficulty 1 > /tmp/stdtest.log

testDefense-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 600 --gameMode testDefense --difficulty 9 > /tmp/stdtest.log

testDefense-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --frontendStd --dumpInitRngs --stopAfter 300 --gameMode testDefense --difficulty 9 > /tmp/stdtest.log

test-short-new:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix battle --gameMode battle --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log

test-short-load:
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix campaign --gameMode campaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix skirmish --gameMode skirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix battle --gameMode battle --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix PvP --gameMode PvP --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix Coop --gameMode Coop --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix defense --gameMode defense --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log
	while true; do echo ' '; echo '.'; sleep 1; done | dist/build/Allure/Allure --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --dumpInitRngs --stopAfter 0 > /tmp/stdtest.log


# The rest of the makefile is unmaintained at the moment.

default : dist/setup-config
	runghc Setup build

dist/setup-config : Allure.cabal
	runghc Setup configure --user

clean :
	runghc Setup clean

ghci :
	ghci -XCPP -idist/build/autogen:GameDefinition
