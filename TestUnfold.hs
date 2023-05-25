{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestUnfold where

import Test.Framework

import Source.PartialEvaluator
import HelperTests

import Language.Haskell.TH

main = htfMain htf_thisModulesTests

-- LamE -------------------------------------------------------------------------------------------------

test_LamE_noStatic_EXCEPTION :: IO ()
test_LamE_noStatic_EXCEPTION = do
    let pat = VarP name
        arg = (False, VarE name)
        dummy = LitE $ IntegerL 4
    assertEqual (False, dummy) (unfold (LamE [pat, pat] dummy) [arg, arg] functions)

-- As we are testing directly what is happening in the function unfolding, and just assume all
-- called methods work, we don't need any functions for further unfolding.
functions = []
