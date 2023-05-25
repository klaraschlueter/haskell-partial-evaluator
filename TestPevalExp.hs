{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestPevalExp where

import Test.Framework

import Source.PartialEvaluator

import Language.Haskell.TH

main = htfMain htf_thisModulesTests

emptyMapper :: [(Name, Exp)]
emptyMapper = []

emptyFunctions :: [(Name, Dec)]
emptyFunctions = []

test_TupE :: IO ()
test_TupE = do
    let x = mkName "x"
        literal = LitE $ StringL "literal (static)"
        dynActual = pevalExp (TupE [(VarE x), literal]) emptyMapper emptyFunctions
        dynExpected = (False, TupE [(VarE x), literal])
        statActual = pevalExp (TupE [literal, literal]) emptyMapper emptyFunctions
        statExpected = (True, TupE [literal, literal])
    assertEqual dynExpected dynActual
    assertEqual statExpected statActual

test_ListE :: IO ()
test_ListE = do
    let x = mkName "x"
        literal = LitE $ StringL "literal (static)"
        dynActual = pevalExp (ListE [(VarE x), literal]) emptyMapper emptyFunctions
        dynExpected = (False, ListE [(VarE x), literal])
        statActual = pevalExp (ListE [literal, literal]) emptyMapper emptyFunctions
        statExpected = (True, ListE [literal, literal])
    assertEqual dynExpected dynActual
    assertEqual statExpected statActual
