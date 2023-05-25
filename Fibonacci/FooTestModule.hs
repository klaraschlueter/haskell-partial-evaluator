module FooTestModule where

import Language.Haskell.TH
import QDefsFib0
import QDefsFib1
import QDefsFib2
import FunctionsFib
import Source.PartialEvaluator

mixedFoo = $(qmix   [| nFoo |]
                    [| Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                      (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
                       Zero )))))))))) ))))))))) |]
                    functions)
      
foo = nFoo (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
           (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ
            Zero )))))))))) ))))))))))
