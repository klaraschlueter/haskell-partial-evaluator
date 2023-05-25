module QDefsFib2 where

import QDefsFib0
import QDefsFib1
import Language.Haskell.TH

$qNFib
$qNInt

qNFoo :: Q [Dec]
qNFoo = [d| nFoo :: Nat -> Nat -> Nat
            nFoo a b = nAdd (nFib a) (nFib b) |]
