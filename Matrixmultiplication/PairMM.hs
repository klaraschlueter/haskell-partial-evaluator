module PairMM where

import Language.Haskell.TH
import FunctionsMM

nameA = mkName "a"
nameB = mkName "b"
nameC = mkName "c"
nameD = mkName "d"

qPair = do
    valueMatrix         <- [| [[1,1], [-1,2]] |]
    let variableMatrix = ListE [ListE [VarE nameA, VarE nameC], ListE [VarE nameB, VarE nameD]]
    return (TupE [valueMatrix, variableMatrix])

-- aktueller Stand der Dinge: mit ddump-simpl scheint der Compiler weder 0*x=0, noch 1*x=x noch
-- 0+x=x auszuräumen, und außerdem a:[b] nicht zu [a,b] umzuwandeln. Ersteres könnte eine comprss
-- Funktion übernehmen, zweiteres nicht so einfach.
