module FunctionsFib where

import Language.Haskell.TH
import QDefsFib0
import QDefsFib1
import QDefsFib2
import TestFunctions.Build
import Source.PartialEvaluator

$qNFoo

functions = consDec qNAdd 'nAdd
          $ consDec qNFib 'nFib
          $ consDec qNInt 'nInt
          $ consDec qNFoo 'nFoo
            nilDec
