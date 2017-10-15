-- Copyright (c) 2008--2011 Andres Loeh, 2010--2017 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The main source code file of Allure of the Stars.
-- Module "TieKnot" is separated to make it usable in tests.
module Main
  ( main
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import Control.Concurrent.Async
import qualified Control.Exception as Ex
import qualified Options.Applicative as OA
import System.Exit

import Game.LambdaHack.Server (debugModeSerPI)
import TieKnot

-- | Tie the LambdaHack engine client, server and frontend code
-- with the game-specific content definitions, and run the game.
main :: IO ()
main = do
  debugModeSer <- OA.execParser debugModeSerPI
  -- Avoid the bound thread that would slow down the communication.
  a <- async $ tieKnot debugModeSer
  ex <- waitCatch a
  case ex of
    Right () -> return ()
    Left e -> case Ex.fromException e of
      Just ExitSuccess ->
        exitSuccess  -- we are in the main thread, so it really exits
      _ -> Ex.throwIO e
