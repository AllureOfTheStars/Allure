-- Copyright (c) 2008--2011 Andres Loeh, 2010--2014 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The main source code file of Allure of the Stars.
module Main ( main ) where

import System.Environment (getArgs)

import TieKnot

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
main :: IO ()
main = do
  args <- getArgs
  tieKnot args
