-- Module      : Text.EDE.Internal.Checker
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Text.EDE.Internal.Checker where

import           Control.Monad
import           Data.Monoid
import           Text.EDE.Internal.Checker.Context (Context)
import qualified Text.EDE.Internal.Checker.Context as Ctx
import           Text.EDE.Internal.Checker.Env     (Env)
import qualified Text.EDE.Internal.Checker.Env     as Env
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Types

data Error a
    = ErrorUndefinedVar a Bound
    | ErrorLetMistmatch a (Exp a) Bind Type
      deriving (Show)

data Result a = Result
    { resTraces :: [String]
    , resResult :: Either String (Exp a, Type)
    }

runChecker :: Exp a -> Env -> Result (Ann a)
runChecker x env = Result undefined res
  where
    (_, res) = runCheck (mempty, 0, 0) $ undefined -- do
--        (ann, t, ctx) <- typeCheckExp x env Ctx.empty

    -- check :: Exp a
    --       -> Env
    --       -> Context
    --       -> CheckM a String (Exp (Ann a), Type, Context)

    check (EVar a u) env ctx
        | Just t <- Ctx.lookup u ctx = returnX a (\z -> EVar z u) t ctx
        | Just t <- Env.lookup u env = returnX a (\z -> EVar z u) t ctx
        | otherwise                  = throw $ ErrorUndefinedVar a u

    check (ELit a l) env ctx
        | LText _ <- l = TText
        | LBool _ <- l = TBool
        | LNum  _ <- l = TNum

    check l@(ELet a b x) env ctx = do
        (bdy, bdyt, ctx1) <- check x env ctx

        let ann = annotation l
            bt  = typeOfBind b

        -- Ensure the binding matches the reconstructed type.
        when (not $ isBot bt) $
             if equivT bt bdyt
                 then return ()
                 else throw $ ErrorLetMistmatch ann l b bdyt

        let bind        = replaceTypeOfBind bdyt b
            (ctx2, pos) = markContext ctx1
            ctx3        = pushType bind ctx2

            res = applyContext ctx3 bdyt
            cut = popToPos pos ctx3

        returnX a (\z -> ELet z bind bdy) res cut

--     check (EApp a x y)
--         |

--     check (ECond a alts)
--         |

--     check (ECase a x alts)
--         |

--     check (ELoop a b x malt)
--         |

--     check (EIncl a b mbdy)
--         |

-- returnX :: a -> (Ann a -> Exp (Ann a)) -> Type -> Context -> Check a
returnX a f t ctx = return (f $ Ann t a, t, ctx)

-- -- -- | Helper function for building the return value of checkExpM'
-- -- --   It builts the AnTEC annotation and attaches it to the new AST node,
-- -- --   as well as returning the current effect and closure in the appropriate
-- -- --   form as part of the tuple. 
-- -- returnX :: Ord n 
-- --         => a                            -- ^ Annotation for the returned expression.
-- --         -> (AnTEC a n 
-- --                 -> Exp (AnTEC a n) n)   -- ^ Fn to build the returned expression.
-- --         -> Type n                       -- ^ Type of expression.
-- --         -> TypeSum n                    -- ^ Effect sum of expression.
-- --         -> Set (TaggedClosure n)        -- ^ Closure of expression.
-- --         -> Context n                    -- ^ Input context.
-- --         -> CheckM a n 
-- --                 ( Exp (AnTEC a n) n     -- Annotated, checked expression.
-- --                 , Type n                -- Type of expression.       (id to above)
-- --                 , TypeSum n             -- Effect sum of expression. (id to above)
-- --                 , Set (TaggedClosure n) -- Closure of expression.    (id to above)
-- --                 , Context n)            -- Output context.

-- -- returnX !a !f !t !es !cs !ctx
-- --  = let  e       = TSum es
-- --         c       = closureOfTaggedSet cs
-- --    in   return  (f (AnTEC t e c a)
-- --                 , t, es, cs, ctx)
