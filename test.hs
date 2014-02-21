module Test where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                      hiding (Result)
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as Map
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as LText
import qualified Data.Text.Lazy.Builder          as LText
import qualified Data.Text.Lazy.IO               as LText

import           Text.EDE.Internal.Checker
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Types

e0 = ELet "id" (ELam "x" (EVar "x")) (EVar "id")
e1 = ELet "id" (ELam "x" (EVar "x")) (EApp (EVar "id") (EVar "id"))
e2 = ELet "id" (ELam "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EVar "id") (EVar "id"))
e3 = ELet "id" (ELam "x" (ELet "y" (EVar "x") (EVar "y"))) (EApp (EApp (EVar "id") (EVar "id")) (ELit (LNum 2)))
e4 = ELet "id" (ELam "x" (EApp (EVar "x") (EVar "x"))) (EVar "id")
e5 = ELam "m"  (ELet "y" (EVar "m") (ELet "x" (EApp (EVar "y") (ELit (LBool True))) (EVar "x")))

e6 = EApp (EVar "y") (EVar "+")
