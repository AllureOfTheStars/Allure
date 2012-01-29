{-# LANGUAGE CPP, QuasiQuotes #-}
-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | The default configurations file included via CPP as a Haskell string.
module ConfigDefault ( configDefault ) where

import Multiline

-- Consider code.haskell.org/~dons/code/compiled-constants (dead link, BTW?)
-- as soon as the config file grows very big.

-- | The string containing the default configuration
-- included from file config.default.
-- Warning: cabal does not detect that the default config is changed,
-- so touching this file is needed to reinclude config and recompile.
configDefault :: String
configDefault = [multiline|

#include "../config.default"

|]
