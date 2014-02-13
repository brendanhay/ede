-- Module      : Text.EDE.Internal.Checker.Context
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker.Context
    ( Context
    , lookup
    ) where

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as Map
import qualified Prelude
import           Prelude                 hiding (lookup)
import           Text.EDE.Internal.Types

data Context = Context

empty

lookup = undefined
-- lookupType :: Eq n => Bound n -> Context n -> Maybe (Type n)
-- lookupType u ctx
--  = case u of
--         UPrim{}         -> Nothing
--         UName n         -> goName n    (contextElems ctx)
--         UIx   ix        -> goIx   ix 0 (contextElems ctx)
--  where
--         goName _n []    = Nothing
--         goName n  (ElemType (BName n' t) : ls)
--          | n == n'      = Just t
--          | otherwise    = goName n ls
--         goName  n (_ : ls)
--          = goName n ls


--         goIx _ix _d []  = Nothing
--         goIx ix d  (ElemType (BAnon t) : ls)
--          | ix == d      = Just t
--          | otherwise    = goIx   ix (d + 1) ls
--         goIx ix d  (_ : ls)
--          = goIx ix d ls

mark
markContext ctx
 = let  p       = contextGenPos ctx
        pos     = Pos p
   in   ( ctx   { contextGenPos = p + 1
                , contextElems  = ElemPos pos : contextElems ctx }
        , pos )


push :: pushType


-- | Pop elements from a context to get back to the given position.
popToPos :: Pos -> Context n -> Context n
popToPos pos ctx
 = ctx { contextElems = go $ contextElems ctx }
 where
        go []                  = []

        go (ElemPos pos' : ls)
         | pos' == pos          = ls
         | otherwise            = go ls

        go (_ : ls)             = go ls


-- -- | Given a bound level-0 (value) variable, lookup its type (level-1) 
-- --   from the context.
-- lookupType :: Eq n => Bound n -> Context n -> Maybe (Type n)
-- lookupType u ctx
--  = case u of
--         UPrim{}         -> Nothing
--         UName n         -> goName n    (contextElems ctx)
--         UIx   ix        -> goIx   ix 0 (contextElems ctx)
--  where
--         goName _n []    = Nothing
--         goName n  (ElemType (BName n' t) : ls)
--          | n == n'      = Just t
--          | otherwise    = goName n ls
--         goName  n (_ : ls)
--          = goName n ls


--         goIx _ix _d []  = Nothing
--         goIx ix d  (ElemType (BAnon t) : ls)
--          | ix == d      = Just t
--          | otherwise    = goIx   ix (d + 1) ls
--         goIx ix d  (_ : ls)
--          = goIx ix d ls

-- -- | See if this type variable is in the context.
-- memberType :: Eq n => Bound n -> Context n -> Bool
-- memberType u ctx = isJust $ lookupType u ctx
