module TestModuleMM where

import FunctionsMM
import PairMM
import Source.PartialEvaluator

mixed a b c d = $( qmix [| mm |] qPair functions )
