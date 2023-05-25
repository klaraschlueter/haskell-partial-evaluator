module FibTestModule where

import Language.Haskell.TH
import FunctionsFib
import QDefsFib0
import QDefsFib1
import QDefsFib2
import Source.PartialEvaluator

mixedFib = $(qmix     [| nFib |]
                      [| Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                        (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                         Zero)))))))))) ))))))))) |]
                      functions)


fib = nFib (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
           (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
            Zero)))))))))) ))))))))))
