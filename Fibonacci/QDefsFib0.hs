module QDefsFib0 where

import Language.Haskell.TH

data Nat = Zero | Succ Nat deriving Show

qNAdd = [d| nAdd :: Nat -> Nat -> Nat
            nAdd Zero       y = y
            nAdd (Succ x)   y = Succ (nAdd x y) |]

qNInt = [d| nInt :: Nat -> Integer
            nInt Zero       = 0
            nInt (Succ x)   = 1 + (nInt x)      |]
