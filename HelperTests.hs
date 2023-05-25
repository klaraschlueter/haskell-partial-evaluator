{-# OPTIONS_GHC -F -pgmF htfpp #-}

module HelperTests where

import Test.Framework
import Language.Haskell.TH

-- Helper for asserting equality for expressions wrapped in the Q monad.
assertQEqual :: Q Exp -> Q Exp -> IO ()
assertQEqual expected actual = do
    a <- runQ actual
    e <- runQ expected
    assertEqual e a

-- Asserting that a boolean is True.
assertTrue :: Bool -> IO ()
assertTrue b = assertEqual True b

-- To avoid a stage error... tbd
v = "variable"
name = 'v
dummy :: Q Dec
dummy = return $ FunD name [] 

