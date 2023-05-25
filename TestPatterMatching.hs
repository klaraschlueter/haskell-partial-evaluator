{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestPatternMatching where

import Test.Framework
import TestFunctions.Functions
import HelperTests

import Source.PartialEvaluator

import Language.Haskell.TH


main = htfMain htf_thisModulesTests

test_staticMatch_literal :: IO ()
test_staticMatch_literal = do
    let literal1 = IntegerL 1
        literal2 = StringL "two"
    assertEqual (Just []) (staticMatch (LitP literal1) (LitE literal1))
    assertEqual (Nothing) (staticMatch (LitP literal1) (LitE literal2))

test_staticMatch_variable :: IO ()
test_staticMatch_variable = do
    let inputExp = LitE $ IntegerL 2
    assertEqual (Just [(name, inputExp)]) (staticMatch (VarP name) inputExp)
