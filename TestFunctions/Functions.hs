module TestFunctions.Functions where

import Language.Haskell.TH

import TestFunctions.Build
import TestFunctions.QDefs2
import TestFunctions.QDefs1
import TestFunctions.QDefs0

$qPower

$qNFoo

$qL

$qHeight

functions :: Q [(Name, Dec)]
functions = consDec qPower  'power
          $ consDec qNFib   'nFib
          $ consDec qNAdd   'nAdd
          $ consDec qNFoo   'nFoo
          $ consDec qL      'l
          $ consDec qHeight 'height
            nilDec
