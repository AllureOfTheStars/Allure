-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
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

import Game.LambdaHack.Core.Prelude

import qualified Data.EnumMap.Strict as EM
import           Data.FileEmbed (embedDir)
import           Language.Haskell.TH.Syntax
import           System.IO

import Game.LambdaHack.Client.UI.Content.Screen

-- | Description of default screen layout and features.
standardLayoutAndFeatures :: ScreenContent
standardLayoutAndFeatures = ScreenContent
  { rwidth = 80
  , rheight = 45
  , rwebAddress = "http://allureofthestars.com"
  , rintroScreen = $(do
      let path = "GameDefinition/PLAYING.md"
      qAddDependentFile path
      x <- qRunIO $ do
        handle <- openFile path ReadMode
        hSetEncoding handle utf8
        hGetContents handle
      let paragraphs :: [String] -> [String] -> [[String]]
          paragraphs [] rows = [reverse rows]
          paragraphs (l@"" : ls) rows = case (rows, ls) of
            (('=':'=' : _) : _, _) ->  -- A title. No new paragraph.
              paragraphs ls (l : rows)
            (('-':'-' : _) : _, _) ->  -- A title. No new paragraph.
              paragraphs ls (l : rows)
            ((' ':' ':' ':' ' : _) : _, (' ':' ':' ':' ' : _) : _) ->
              -- At least four spaces before and after; probably a code block.
              paragraphs ls (l : rows)
            _ -> reverse rows : paragraphs ls []
          paragraphs (l : ls) rows = paragraphs ls (l : rows)
          intro = case paragraphs (lines x) [] of
            _titleAndBlurb : par1 : par2 : par3 : rest ->
              (par1 ++ [""] ++ par2 ++ [""] ++ par3, rest)
            _ -> error "not enough paragraphs in intro screen text"
      lift intro)
  , rapplyVerbMap =
      EM.fromList [('!', "imbibe"), (',', "eat"), ('?', "activate")]
  , rFontFiles =
-- Checking USE_SDL would be more accurate, but would require complicating
-- .cabal file and slightly larger vty executable is not a problem.
#ifdef USE_JSFILE
      []
#else
      $(embedDir "GameDefinition/fonts")
#endif
  }
