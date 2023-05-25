module TestFunctions.QDefs1 where

import Language.Haskell.TH

import TestFunctions.QDefs0

$qNAdd

qNFib :: Q [Dec]
qNFib = [d| nFib :: Nat -> Nat
            nFib Zero               = Zero
            nFib (Succ Zero)        = Succ Zero
            nFib (Succ (Succ n))    = nAdd (nFib (Succ n)) (nFib n) |]
