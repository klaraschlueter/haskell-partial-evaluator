{-# OPTIONS_GHC -F -pgmF htfpp #-}

module IntegrationTests where

import Test.Framework
import TestFunctions.Functions
import TestFunctions.QDefs0 -- necessary for using the data type Nat
import TestFunctions.QDefs1 -- necessary for using the function nAdd
import TestFunctions.QDefs2 -- necessary for using the function nFib

import Source.PartialEvaluator
import Language.Haskell.TH

main = htfMain htf_thisModulesTests

-- Power -------------------------------------------------------------------------------------------------
-- commented out because of force evaluation problem (loop while compiling)

--prop_powerPevalOnce :: Integer -> Integer -> Bool
--prop_powerPevalOnce x n = $( qpeval [| power |] [| x |] functions ) n == power x n

--prop_powerPevalTwice :: Integer -> Integer -> Bool
--prop_powerPevalTwice x n = $( qpeval (qpeval [| power |] [| x |] functions) [| n |] functions ) == power x n

-- List recursion: function last -------------------------------------------------------------------------

test_last = assertEqual (           l       [True, True, False]              )
                       $( qmix [| l |] [| [True, True, False] |] functions )

-- Data type tree (constructor taking three arguments) ---------------------------------------------------

test_height = assertEqual (           height     ( Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5)) )            )
                         $( qmix [| height |] [| Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5)) |] functions )

test_heightLeaf = assertEqual (           height     ( Leaf 0 )            )
                             $( qmix [| height |] [| Leaf 0 |] functions )

-- Nat as own data type ----------------------------------------------------------------------------------

instance Show Nat where
    
    show Zero       = "Zero"
    show (Succ n)   = "Succ " ++ (show n)

instance Eq Nat where

    Zero    == Zero     = True
    Succ x  == Succ y   = x == y
    _       == _        = False

instance Num Nat where

    (+) = nAdd

    x - Zero            = x
    (Succ a) - (Succ b) = a - b
    Zero - (Succ a)     = error $ "The substraction of a natural number a from a natural number b isn't "
                               ++ "natural if a > b."

    Zero     * x    = Zero
    (Succ n) * x    = x + (x * n)

    negate _ = error "The negation of a natural number isn't natural."

    abs x = x

    signum Zero = Zero
    signum _    = Succ Zero

    fromInteger 0   = Zero
    fromInteger n
        | n > 0     = Succ (fromInteger (n-1))
        | n < 0     = error "Can't convert a negative integer to a natural number!"

ownToInteger :: Nat -> Integer
ownToInteger Zero      = 0
ownToInteger (Succ n)  = 1 + (ownToInteger n)

test_nAddTo0 = do
    let pevaledNAddTo0 = ownToInteger . $( qmix [| nAdd |] [| Zero |] functions ) . fromIntegral
        nAddTo0        = ownToInteger .  (nAdd Zero)                                . fromIntegral
        testNumbers    = [0..5]
    assertEqual (map pevaledNAddTo0 testNumbers) (map nAddTo0 testNumbers)

test_nAddTo1 = do
    let pevaledNAddTo1 = ownToInteger . $( qmix [| nAdd |] [| Succ Zero |] functions ) . fromIntegral
        nAddTo1        = ownToInteger .  (nAdd (Succ Zero))                              . fromIntegral
        testNumbers    = [0..5]
    assertEqual (map pevaledNAddTo1 testNumbers) (map nAddTo1 testNumbers)

test_nAddTo5 = do
    let pevaledNAddTo5 = ownToInteger
                       . $( qmix [| nAdd |] [| Succ (Succ (Succ (Succ (Succ Zero)))) |] functions )
                       . fromIntegral
        nAddTo5        = ownToInteger
                       . (nAdd (Succ (Succ (Succ (Succ (Succ Zero))))))
                       . fromIntegral
        testNumbers    = [0..5]
    assertEqual (map pevaledNAddTo5 testNumbers) (map nAddTo5 testNumbers)

test_nFib = do
    assertEqual $(qmix [| nFib |] [| Succ (Succ (Succ (Succ Zero))) |] functions)
                 (          nFib      (Succ (Succ (Succ (Succ Zero))))            )

mixedFoo = $(qmix [| nFoo |]
                    [| Succ (Succ (Succ (Succ (Succ     (Succ (Succ (Succ (Succ (Succ
                      (Succ (Succ (Succ (Succ (Succ     (Succ (Succ (Succ (Succ (Succ
                       Zero ))))) )))))   ))))) ))))|]
                    functions )

-- While -------------------------------------------------------------------------------------------------
-- To be added when Interp.hs is under controll ----------------------------------------------------------

-- parseProgram ::  String -> Either ParseError Stmt
-- interp :: (MonadState Env m, MonadError String m, MonadIO m) =>
--                  Stmt -> m ()
-- eval :: (MonadState Env m, MonadError String m, MonadIO m) =>
--                  Exp -> m Integer

-- fib example from sheet 13: only parsed, not interpreted, 
--test_parseFib = assertEqual $( qpeval [| parseProgram |] [| fibCode |] functions )
--                             (           parseProgram       fibCode              )

-- Because interp returns m (), I use eval, which returns m Integer. As eval only takes Exp, not Stmt, I have to unwrap before using it.                             
--test_evalParseExp = do
  --  let x = "print ((((3 + 4) * 5) - 3) / 8);"
    --actual <-     (runExceptT $ evalStateT $( qpeval [| eval |] [| (let Right (Print exp) = $( qpeval [| parseProgram |] [| x |] functions ) in exp) |] functions ) [])
--    expected <-   (runExceptT $ evalStateT  (           eval       (let Right (Print exp) =              parseProgram       x                in exp)              ) [])
  --  assertEqual actual expected
    
--test_evalParseExpWithVs = do
  --  let x = "print (a * b);"
    --    env = [("a", 2), ("b",5)]
--    actual <-     (runExceptT $ evalStateT $( qpeval [| eval |] [| (let Right (Print exp) = $( qpeval [| parseProgram |] [| x |] functions ) in exp) |] functions ) env)
  --  expected <-   (runExceptT $ evalStateT  (           eval       (let Right (Print exp) =              parseProgram       x                in exp)              ) env)
    --assertEqual actual expected

-- Lambda Expressions ------------------------------------------------------------------------------------


