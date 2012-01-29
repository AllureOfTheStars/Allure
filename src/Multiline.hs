-- Copyright (c) 2008--2011 Andres Loeh, 2010--2012 Mikolaj Konarski
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
-- | Template Haskell machinery for quoting multiline strings.
module Multiline (multiline) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TQ

-- | Handle multiline verbatim string expressions.
multiline :: TQ.QuasiQuoter
multiline  = TQ.QuasiQuoter (\x -> (TH.litE . TH.stringL) x)
               undefined undefined undefined
