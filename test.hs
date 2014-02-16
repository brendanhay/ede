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

import           Text.EDE.Internal.Checker.Monad
import           Text.EDE.Internal.Types

e0 = ELet 0 "id" (ELam 0 "x" (EVar 0 "x")) (EVar 0 "id")
e1 = ELet 0 "id" (ELam 0 "x" (EVar 0 "x")) (EApp 0 (EVar 0 "id") (EVar 0 "id"))
e2 = ELet 0 "id" (ELam 0 "x" (ELet 0 "y" (EVar 0 "x") (EVar 0 "y"))) (EApp 0 (EVar 0 "id") (EVar 0 "id"))
e3 = ELet 0 "id" (ELam 0 "x" (ELet 0 "y" (EVar 0 "x") (EVar 0 "y"))) (EApp 0 (EApp 0 (EVar 0 "id") (EVar 0 "id")) (ELit 0 (LNum 2)))
e4 = ELet 0 "id" (ELam 0 "x" (EApp 0 (EVar 0 "x") (EVar 0 "x"))) (EVar 0 "id")
e5 = ELam 0 "m"  (ELet 0 "y" (EVar 0 "m") (ELet 0 "x" (EApp 0 (EVar 0 "y") (ELit 0 (LBool True))) (EVar 0 "x")))

test :: Exp Integer -> IO ()
test e = do
    (res, _) <- runTI (typeInference Map.empty e)
    case res of
        Left  err -> putStrLn $ "error: " ++ err
        Right x   -> putStrLn $ show e ++ " :: " ++ show x
