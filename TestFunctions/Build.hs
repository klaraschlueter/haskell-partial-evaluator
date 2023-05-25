module TestFunctions.Build where

import Language.Haskell.TH

-- consDec :: Q (declaration of function) -> [| name of function (call to) |] -> existing list -> function : list
consDec :: Q [Dec] -> Name -> Q [(Name, Dec)] -> Q [(Name, Dec)]
consDec qf name qlist = do
    list                    <- qlist
    [signgature, function]  <- qf
    let renamedFunction     = renameDec function name
        pair                = (name, renamedFunction)
        newList             = pair:list
    return newList

nilDec :: Q [(Name, Dec)]
nilDec = return []

renameDec :: Dec -> Name -> Dec
renameDec (FunD oldN clauses) newN = FunD newN (map (renameInClause oldN newN) clauses)

renameInDec :: Name -> Name -> Dec -> Dec
renameInDec oldN newN (FunD name clauses) = FunD name (map (renameInClause oldN newN) clauses)

renameInClause :: Name -> Name -> Clause -> Clause
renameInClause oldN newN (Clause ps body ds) = Clause ps (renameInBody oldN newN body) ds

renameInBody :: Name -> Name -> Body -> Body
renameInBody oldN newN (NormalB exp) = NormalB (renameInExp oldN newN exp)

renameInExp :: Name -> Name -> Exp -> Exp
renameInExp oldN newN (VarE name) 
    | name == oldN                          = VarE newN
    | otherwise                             = VarE name
renameInExp oldN newN (ConE name)           = ConE name
renameInExp oldN newN (LitE lit)            = LitE lit
renameInExp oldN newN (AppE e1 e2)          = AppE (renameInExp oldN newN e1) (renameInExp oldN newN e2)
renameInExp oldN newN (InfixE mE1 op mE2)   = let newME1 = renameInMaybeExp oldN newN mE1
                                                  newME2 = renameInMaybeExp oldN newN mE2
                                                  newOp  = renameInExp oldN newN op
                                              in InfixE newME1 newOp newME2
renameInExp oldN newN (UInfixE e1 op e2)    = let newE1 = renameInExp oldN newN e1
                                                  newE2 = renameInExp oldN newN e2
                                                  newOp = renameInExp oldN newN op
                                              in UInfixE newE1 newOp newE2
renameInExp oldN newN (ParensE e)           = ParensE $ renameInExp oldN newN e
renameInExp oldN newN (LamE ps e)           = LamE ps $ renameInExp oldN newN e
renameInExp oldN newN (LamCaseE ms)         = undefined
renameInExp oldN newN (TupE es)             = TupE $ map (renameInExp oldN newN) es
renameInExp oldN newN (UnboxedTupE es)      = undefined
renameInExp oldN newN (CondE i t e)         = let newI = renameInExp oldN newN i
                                                  newT = renameInExp oldN newN t
                                                  newE = renameInExp oldN newN e
                                              in CondE newI newT newE
renameInExp oldN newN (MultiIfE _)          = undefined
renameInExp oldN newN (LetE decs e)         = LetE (map (renameInDec oldN newN) decs) (renameInExp oldN newN e)
renameInExp oldN newN (ListE es)            = ListE (map (renameInExp oldN newN) es)


renameInMaybeExp :: Name -> Name -> Maybe Exp -> Maybe Exp
renameInMaybeExp oldN newN (Just e) = Just (renameInExp oldN newN e)
renameInMaybeExp _    _     Nothing = Nothing
