module QDefsMM1 where

import Language.Haskell.TH
import QDefsMM0

$qMmEntry

qMmColumn :: Q [Dec]
qMmColumn = [d| mmColumn :: [[Double]] -> [Double] -> [Double]
                mmColumn []         column = []
                mmColumn (r:rows)   column = mmEntry r column : (mmColumn rows column)  |]
