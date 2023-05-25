module QDefsMM2 where

import Language.Haskell.TH
import QDefsMM1

$qMmColumn

qMm :: Q [Dec]
qMm = [d|   mm :: ([[Double]], [[Double]]) -> [[Double]]
            mm (_,        []         ) = []
            mm (rowVecs,  (c:columns)) = mmColumn rowVecs c : (mm (rowVecs, columns)) |]
