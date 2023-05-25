{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TestEntry where

import Test.Framework

import TestFunctions.Functions
import Source.PartialEvaluator
import HelperTests

import Language.Haskell.TH

main = htfMain htf_thisModulesTests

-- LamE -------------------------------------------------------------------------------------------------

test_LamE_replaceBody :: IO ()
test_LamE_replaceBody = assertEqual 3 $(qmix [| (\ x -> x) |] [| 3 |] functions)

prop_LamE_returnRightArityFunction :: Integer -> Bool
prop_LamE_returnRightArityFunction k = $(qmix [| (\ x y -> x) |] [| 3 |] functions) k == 3
                                    && $(qmix [| (\ x y -> y) |] [| 3 |] functions) k == k

test_LamE_notMatchingArgument_EXCEPTION :: IO ()
test_LamE_notMatchingArgument_EXCEPTION = assertQEqual [| "exception" |]
                                                       (qmix [| (\ 1 x y -> x) |] [| 8 |] functions)
                                                  
-- VarE -------------------------------------------------------------------------------------------------
