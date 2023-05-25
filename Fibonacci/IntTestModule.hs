module IntTestModule where

import Language.Haskell.TH
import FunctionsFib
import QDefsFib0
import QDefsFib1
import QDefsFib2
import Source.PartialEvaluator

mixedInt = $(qmix   [| nInt |]
                    (qmix     [| nFib |]
                              [| Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                                (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                                 Zero)))))))))) ))))))))) |]
                               functions)
                     functions)

int = nInt (nFib (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                 (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                  Zero)))))))))) )))))))))))
