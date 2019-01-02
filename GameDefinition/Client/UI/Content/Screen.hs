-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2019 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
{-# LANGUAGE TemplateHaskell #-}
-- | The default screen layout and features definition.
module Client.UI.Content.Screen
  ( standardLayoutAndFeatures
  ) where

import Prelude ()

import Game.LambdaHack.Common.Prelude

import qualified Data.EnumMap.Strict as EM
import           Language.Haskell.TH.Syntax
import           System.IO

import Game.LambdaHack.Client.UI.Content.Screen

-- | Description of default screen layout and features.
standardLayoutAndFeatures :: ScreenContent
standardLayoutAndFeatures = ScreenContent
  { rwidth = 80
  , rheight = 45
  -- ASCII art for the main menu. Only pure 7-bit ASCII characters are allowed,
  -- except for character 183 ('·'), which is rendered as very tiny middle dot.
  -- The encoding should be utf-8-unix.
  -- When displayed in the main menu screen, the picture is overwritten
  -- with game and engine version strings and keybindings.
  -- The keybindings overwrite places marked with left curly brace signs.
  -- This sign is forbidden anywhere else in the picture.
  -- The picture and the whole main menu is displayed dull white on black.
  -- The glyphs, or at least the character cells, are perfect squares.
  -- The picture for Allure should be exactly 45 rows by 80 columns.
  , rmainMenuArt = $(do
      let path = "GameDefinition/MainMenu.ascii"
      qAddDependentFile path
      x <- qRunIO $ do
        handle <- openFile path ReadMode
        hSetEncoding handle utf8
        hGetContents handle
      lift x)
  , rintroScreen = $(do
      let path = "GameDefinition/PLAYING.md"
      qAddDependentFile path
      x <- qRunIO $ do
        handle <- openFile path ReadMode
        hSetEncoding handle utf8
        hGetContents handle
      let paragraphs :: [String] -> [String] -> [[String]]
          paragraphs [] rows = [reverse rows]
          paragraphs (l : ls) rows = if null l
                                     then reverse rows : paragraphs ls []
                                     else paragraphs ls (l : rows)
          intro = case paragraphs (lines x) [] of
            _title : _blurb : par1 : par2 : par3 : _rest ->
              ["", "", ""] ++ par1
              ++ [""] ++ par2
              ++ [""] ++ par3 ++ ["", "", "", "", ""]
            _ -> error "not enough paragraphs in intro screen text"
      lift intro)
  , rmoveKeysScreen = $(do
      let path = "GameDefinition/MoveKey.txt"
      qAddDependentFile path
      x <- qRunIO $ do
        handle <- openFile path ReadMode
        hSetEncoding handle utf8
        hGetContents handle
      lift $ lines x)
  , rapplyVerbMap =
      EM.fromList [('!', "imbibe"), (',', "eat"), ('?', "activate")]
  }
