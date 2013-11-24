# Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
# This file is a part of the computer game Allure of the Stars
# and is released under the terms of the GNU Affero General Public License.
# For license and copyright information, see the file LICENSE.
#

test:
	dist/build/Allure/Allure --noMore --noDelay --noAnim --maxFps 100000 --dbgMsgSer --savePrefix screensaver --gameMode screensaver --frontendStd --stopAfter 500 > /tmp/stdtest.log

test-frontend:
	dist/build/Allure/Allure --noMore --maxFps 45 --dbgMsgSer --savePrefix screensaver --gameMode screensaver

test-travis:
	dist/build/Allure/Allure --noMore --noDelay --noAnim --maxFps 100000 --savePrefix screensaver --gameMode screensaver --frontendStd --stopAfter 60 > /dev/null

testCoop:
	dist/build/Allure/Allure --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --dbgMsgSer --savePrefix testCoop --gameMode testCoop --frontendStd --stopAfter 500 > /tmp/stdtest.log

testCoop-frontend:
	dist/build/Allure/Allure --noMore --maxFps 180 --dbgMsgSer --fovMode Permissive --savePrefix testCoop --gameMode testCoop

testCoop-travis:
	dist/build/Allure/Allure --noMore --noDelay --noAnim --maxFps 100000 --fovMode Permissive --savePrefix testCoop --gameMode testCoop --frontendStd --stopAfter 60 > /dev/null

testDefense:
	dist/build/Allure/Allure --noMore --maxFps 100000 --dbgMsgSer --savePrefix testDefense --gameMode testDefense --frontendStd --stopAfter 500 > /tmp/stdtest.log

testDefense-frontend:
	dist/build/Allure/Allure --noMore --maxFps 45 --dbgMsgSer --savePrefix testDefense --gameMode testDefense

testDefense-travis:
	dist/build/Allure/Allure --noMore --maxFps 100000 --savePrefix testDefense --gameMode testDefense --frontendStd --stopAfter 60 > /dev/null

travisTests: test-travis testCoop-travis testDefense-travis

testPeek-play:
	dist/build/Allure/Allure --dbgMsgSer --savePrefix peek --gameMode peek


# The rest of the makefile is unmaintained at the moment.

default : dist/setup-config
	runghc Setup build

dist/setup-config : Allure.cabal
	runghc Setup configure --user

clean :
	runghc Setup clean

ghci :
	ghci -XCPP -idist/build/autogen:src
