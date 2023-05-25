module TestFunctions.QDefs0 where

import Language.Haskell.TH

data Nat = Zero | Succ Nat

-- used in QTestFunctionDefs1.qnFib
qNAdd :: Q [Dec]
qNAdd = [d| nAdd :: Nat -> Nat -> Nat
            nAdd Zero     x = x
            nAdd (Succ a) x = Succ (nAdd a x)   |]
