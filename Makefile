# Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
# This file is a part of the computer game Allure of the Stars
# and is released under the terms of the GNU Affero General Public License.
# For license and copyright information, see the file LICENSE.
#

test: test-short test-medium test-long

test-long: testCampaign-long testCoop-long testDefense-long

test-medium: testCampaign-medium testCoop-medium testDefense-medium

testCampaign-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode screensaver --frontendStd --stopAfter 500 > /tmp/stdtest.log

testCampaign-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --savePrefix test --gameMode screensaver --frontendStd --stopAfter 60 > /tmp/stdtest.log

frontendCampaign:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode screensaver

testCoop-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix test --gameMode testCoop --frontendStd --stopAfter 500 > /tmp/stdtest.log

testCoop-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noDelay --noAnim --maxFps 100000 --fovMode Shadow --savePrefix test --gameMode testCoop --frontendStd --stopAfter 60 > /tmp/stdtest.log

frontendCoop:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 180 --fovMode Permissive --savePrefix test --gameMode testCoop

testDefense-long:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --noAnim --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --stopAfter 500 > /tmp/stdtest.log

testDefense-medium:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 100000 --savePrefix test --gameMode testDefense --frontendStd --stopAfter 60 > /tmp/stdtest.log

frontendDefense:
	dist/build/Allure/Allure --dbgMsgSer --newGame --noMore --maxFps 45 --savePrefix test --gameMode testDefense

test-short: test-short-new test-short-load

test-short-new:
	yes . | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix campaign --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix skirmish --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix PvP --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix Coop --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix defense --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --newGame --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log

test-short-load:
	yes . | dist/build/Allure/Allure --dbgMsgSer --savePrefix campaign --gameMode campaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --savePrefix skirmish --gameMode skirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --savePrefix PvP --gameMode PvP --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --savePrefix Coop --gameMode Coop --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --savePrefix defense --gameMode defense --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign --frontendStd --stopAfter 0 > /tmp/stdtest.log
	yes . | dist/build/Allure/Allure --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish --frontendStd --stopAfter 0 > /tmp/stdtest.log

test-travis: test-short test-medium

peekCampaign:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix peekCampaign --gameMode peekCampaign

peekSkirmish:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix peekSkirmish --gameMode peekSkirmish


# The rest of the makefile is unmaintained at the moment.

default : dist/setup-config
	runghc Setup build

dist/setup-config : Allure.cabal
	runghc Setup configure --user

clean :
	runghc Setup clean

ghci :
	ghci -XCPP -idist/build/autogen:src
