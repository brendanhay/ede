module Test where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                      hiding (Result)
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as LText
import qualified Data.Text.Lazy.Builder          as LText
import qualified Data.Text.Lazy.IO               as LText
import           Text.EDE.Internal.Checker
import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Types


tyvarA :: TVar
tyvarA = TBound "a"

initTypeEnv :: [(Name, Sigma)]
initTypeEnv =
    [ ("+",     TCon TNum --> TCon TNum --> TCon TNum)
    , ("if",    TForAll [tyvarA] (TCon TBool --> TVar tyvarA --> TVar tyvarA))
    , ("true",  TCon TBool)
    , ("false", TCon TBool)
    ]

run = evalCheck initTypeEnv (typecheck x)
  where
    x = ELet (0 :: Int) "compose"
            (ELam 0 "f"
                (ELam 0 "g"
                     (ELam 0 "x"
                          (EApp 0 (EVar 0 "f") (EApp 0 (EVar 0 "f") (EVar 0 "x"))))))
            (EVar 0 "compose")




-- Right (TForAll [TBound {tvarName = "a"},TBound {tvarName = "b"}] (TFun (TFun (TVar (TBound {tvarName = "b"})) (TVar (TBound {tvarName = "b"}))) (TFun (TVar (TBound {tvarName = "a"})) (TFun (TVar (TBound {tvarName = "b"})) (TVar (TBound {tvarName = "b"}))))))
-- Right (TForAll [] (TFun (TFun (TMeta (TM 5)) (TMeta (TM 5))) (TFun (TMeta (TM 1)) (TFun (TMeta (TM 5)) (TMeta (TM 5))))))
