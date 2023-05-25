module TestFunctions.QDefs2 where

import Language.Haskell.TH

import TestFunctions.QDefs0
import TestFunctions.QDefs1

qPower :: Q [Dec]
qPower = [d|    power :: Integer -> Integer -> Double
                power x 0   = 1.0
                power x n   = (fromIntegral x) * (power x (n-1))    |]
-- commented out for compilation reasons until pevaluation of guards is implemented.
--                power x n
--                    | n < 0 = 1.0 / (power x (-n))
--                    | n > 0 = (fromIntegral x) * (power x (n-1))    |]

$qNFib

qNFoo :: Q [Dec]
qNFoo = [d| nFoo :: Nat -> Nat -> Nat
            nFoo a b = nAdd (nFib a) (nFib b)   |]

qL :: Q [Dec]
qL = [d|    l :: [a] -> a
            l [x]    = x
            l (x:xs) = l xs   |]

-- Tree data structure ------------------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) a (Tree a)

instance (Eq a) => Eq (Tree a) where

    Leaf a1         == Leaf a2          = a1 == a2
    Node l1 n1 r1   == Node l2 n2 r2    = l1 == l2 && n1 == n2 && r1 == r2

qHeight :: Q [Dec]
qHeight = [d|   height :: Tree a -> Integer
                height (Leaf _)     = 1
                height (Node l m r) = 1 + max (height l) (height r) |]

---------------------------------------- old --------------------------------------------

qThreeClausesFunction :: Q [Dec]
qThreeClausesFunction = [d| threeClausesFunction :: Int -> Int -> Int
                            threeClausesFunction 0 y = 1
                            threeClausesFunction x 0 = 0
                            threeClausesFunction x y = y `div` x        |]

qOneArgFunction :: Q [Dec]
qOneArgFunction = [d|   oneArgFunction :: Int -> Int
                        oneArgFunction 0 = 0
                        oneArgFunction 1 = 1
                        oneArgFunction x = 2            |]

qDiffPatNumFunction :: Q [Dec]
qDiffPatNumFunction = [d|       diffPatNumFunction :: Int -> Int -> Int
                                diffPatNumFunction x 1   = 1
                                diffPatNumFunction x y z = y            |]
