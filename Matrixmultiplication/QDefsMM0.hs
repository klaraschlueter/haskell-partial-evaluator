module QDefsMM0 where

import Language.Haskell.TH

qMmEntry :: Q [Dec]
qMmEntry = [d|  mmEntry :: [Double] -> [Double] -> Double
                mmEntry []      []          = 0
                mmEntry (r:row) (c:column)  = r*c + (mmEntry row column)
                mmEntry _       _           = error $ "Number of columns of the first matrix must be "
                                                   ++ "equal to number of rows in the second matrix!"    |]
