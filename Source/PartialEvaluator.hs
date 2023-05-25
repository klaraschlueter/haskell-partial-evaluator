module Source.PartialEvaluator where

import Language.Haskell.TH
import Control.Monad

--  - manage monad
qmix :: Q Exp -> Q Exp -> Q [(Name, Dec)] -> Q Exp
qmix qf qx qfunctions = do
        f <- qf
        x <- qx
        functions <- qfunctions
        return $ mix f x functions

--  - compute mapper variable -> expression
-- (- dazu muss VarE f auch geholt werden)
--  - count number of arguments n of given function and return something that takes n-1 arguments!
mix :: Exp -> Exp -> [(Name, Dec)] -> Exp

mix (VarE f)                      x functions = case lookup f functions of
                            Nothing             -> error $ "I don't know the implementation of " ++ (show f)
-- erstmal alles nur so, dass es genau im Testfall funktioniert. Und sehr hässlich. Aber zum Testen.
                            Just declaration    -> let FunD name (c:clauses) = declaration
                                                       Clause pats body decs = c
                                                       (static, residual) = pevalDec declaration args functions
                                                       names = map (\n -> mkName ('n': (show n))) [1..(length pats - 1)]
                                                       args = (True, x) : (map (\n -> (False, VarE n)) names)
                                                   in if null names then residual
                                                      else LamE (map VarP names) residual

mix (AppE e1 e2)                  x functions = undefined -- problem: f may take more arguments. We have to check this here,
                                                            -- because we want to return a function with n-1 args.  

   where    applyArgs :: Exp -> [(Bool, Exp)] -> (Bool, Exp)
            applyArgs (AppE f a) args = applyArgs f ((True, a) : args)
            applyArgs  f         args = unfold f ((True, x) : args) functions

mix (InfixE Nothing op Nothing)   x functions = undefined

mix (InfixE Nothing op e)         x functions = undefined

mix (InfixE e op Nothing)         x functions = undefined

mix (LamE (p:pats) e)             x functions = case staticMatch p x of
    Just mapper                                 -> compress $ LamE pats $ residual $ pevalExp e mapper functions
    Nothing                                     -> error "Attempt to apply not matching argument to lambda"

mix (LetE decs e)                 x functions = undefined -- (for example (let f x = x*x in f) 8)

-- Keine Ahnung von LamCaseE, UnboxedTupE, CompE, ArithSeqE, RecConE, RecUpdE, StaticE. In den Rest kann man nichts einsetzen, deshalb:
mix other                         x functions = error $ "Expression\n " ++ (show other) ++ "\n does not "
                                                       ++ "represent a function!"

-- mixDec :: dec -> [(static, argument)] -> functions -> (static, residual dec)
-- - reduce clauses (mix, check static arguments, ...)
pevalDec :: Dec -> [(Bool, Exp)] -> [(Name, Dec)] -> (Bool, Exp)
pevalDec (FunD name clauses) args functions = case remaining clauses of

                -- arguments do not match the pattern of any clause: error.
                []                          ->  error $ "Non exhaustive patterns in function " ++ show name

                -- only one static clause remains: the clauses body can be returned as it is.
                [(True, resC)]              ->  let static = True
                                                    Clause _ (NormalB residual) _ = resC

                                                in (static, residual)

                -- only one dynamic clause remains: rename dynamic arguments to pats with LetE. 
                [(False, resC)]             ->  let static = False
                                                    residual = compress $ LetE decsForLet body

                                                    Clause pats (NormalB body) resDecs = resC
                                                    exps = residuals dynamicArgs
                                                    decsForLet = resDecs ++ (renamer pats exps)

                                                in (static, residual)

                -- several clauses remain: combine remaining clauses to a dec and return as LetE dec name.
                cs                          ->  let static = False
                                                    residual = reconstructAppE decExp dynamicArgs

                                                    resClauses = residuals cs
                                                    resName = pevalName name
                                                    dec = FunD resName resClauses
                                                    decExp = LetE [dec] (VarE resName)
                                               in (static, residual)

-- very, very high coupling between pevalDec and pevalClause, very, very bad.
-- Achtung, GuardedB fehlt noch komplett.

    where   remaining :: [Clause] -> [(Bool, Clause)]
            remaining []        = []
            remaining (c:cs)    = case pevalClause c args functions of
                Nothing         -> remaining cs
                Just resClause  -> if (allStatic args) -- nochmal überdenken: wird das bei jedem rekursiven Aufruf
                                                       -- neu berechnet?
                                   then [resClause]
                                   else resClause : remaining cs

            dynamicArgs = filter (not.fst) args

            pevalName :: Name -> Name
            pevalName name = mkName $ "pevaled" ++ (nameBase name)

            renamer :: [Pat] -> [Exp] -> [Dec]
            renamer ps es = filter necessaryRename $ zipWith valDec ps es

            necessaryRename :: Dec -> Bool
            necessaryRename (ValD (VarP np) (NormalB (VarE ne)) _)    = np /= ne
            necessaryRename _                                         = True

-- pevalClause :: clause -> [(static, argument)] -> functions -> (static, residual clause)
--(- does the pattern match the signature?)
-- - do the single patterns match the arguments?
-- - handle decs: - add them to functions for pevaluation of the body.
--                - check if still needed and eventually remove them from the clause for the residual clause.
pevalClause :: Clause -> [(Bool, Exp)] -> [(Name, Dec)] -> Maybe (Bool, Clause)
pevalClause (Clause pats body decs) args functions = case patternMatch pats args of
                        Nothing                    -> Nothing
                        Just (mapper, newNames)    -> let (statB, resB) = pevalBody body mapper functions
                                                      in Just (statB, residualClause resB)

--          schon klar, das ist überhaupt nicht schön, weil man zweimal rekursiv durch pats und args geht
--          (patternMatch und dynamicPats), aber erstmal funktionierts.
    where   residualClause :: Body -> Clause
            residualClause body = Clause (dynamicPats pats args) body decs

            dynamicPats :: [Pat] -> [(Bool, Exp)] -> [Pat]
            dynamicPats []      []                = []
            dynamicPats (p:ps) ((static, x):args) = let recursive = dynamicPats ps args
                                                        in  if static then recursive
                                                            else p:recursive

-- pevalBody :: body -> [(replace, by)] -> functions -> (static, residual body)
pevalBody :: Body -> [(Name, Exp)] -> [(Name, Dec)] -> (Bool, Body)
pevalBody (NormalB body) mapper functions      = let (static, residualBody) = pevalExp body mapper functions
                                                 in  (static, NormalB residualBody)
pevalBody (GuardedB guards) mapper functions    = undefined -- force evaluation to use guards!


-- pevalGuard :: guard -> [(replace, by)] -> functions -> (static, residual guard)
-- - The guard does only represent the boolean expression before "="!
-- - No renaming necessairy: if there is at least one guard whith a dynamic residual guard, the function
--   including the AppEs has to be reconstructed, so the pattern handle the argument names like before.
pevalGuard :: Guard -> [(Name, Exp)] -> [(Name, Dec)] -> (Bool, Guard)
pevalGuard (NormalG boolExp) mapper functions    = let (static, resBool) = pevalExp boolExp mapper functions
                                                   in  (static, NormalG resBool)
pevalGuard (PatG stmt)       mapper functions    = undefined



-- pevalExp :: exp to peval -> [(replace, by)] -> functions -> (static, residual exp)
pevalExp :: Exp -> [(Name, Exp)] -> [(Name, Dec)] -> (Bool, Exp)

pevalExp (VarE x)           mapper functions    = case lookup x mapper of
        Just e                                  -> (True, e)
        Nothing                                 -> (False, VarE x)

pevalExp (ConE x)           mapper functions    = (True, ConE x)

pevalExp (LitE l)           mapper functions    = (True, LitE l)

pevalExp (AppE e1 e2)       mapper functions    = helper (AppE e1 e2) []
    --      helper :: AppE -> arguments already found -> (static, returned Exp)
    where   helper :: Exp -> [(Bool, Exp)] -> (Bool, Exp)
            helper (AppE fun arg)   prevArgs    =   let a@(staticA, resA) = pevalExp arg mapper functions
                                                    in  helper fun (a : prevArgs)
            helper (ConE name)          args    =   let static = allStatic args
                                                        residual = reconstructAppE (ConE name) args
                                                    in (static, residual)
            helper fun                  args    =   if existStatic args
                                                        then unfold fun args functions
                                                    else (False, reconstructAppE fun args)

pevalExp (InfixE (Just e1) f (Just e2)) mapper functions = (staticE1 && staticE2, InfixE (Just resE1) f (Just resE2))
    where   (staticE1, resE1) = pevalExp e1 mapper functions
            (staticE2, resE2) = pevalExp e2 mapper functions -- TEMPORARILY, only works for primitive operators
pevalExp (UInfixE e1 f e2)  mapper functions    = undefined

pevalExp (ParensE e)        mapper functions    =   let (static, processed) = pevalExp e mapper functions
                                                    in  (static, ParensE processed)
                  
pevalExp (LamE pats e)      mapper functions    =   let (statE, resE) = pevalExp e mapper functions
                                                    in (statE, LamE pats resE) 
pevalExp (LamCaseE ms)      mapper functions    = undefined
pevalExp (TupE es)          mapper functions    =   let pevaledEs = map recursiveCall es
                                                        recursiveCall e = pevalExp e mapper functions
                                                        static = allStatic pevaledEs
                                                        residual = TupE (residuals pevaledEs)
                                                    in (static, residual)
pevalExp (UnboxedTupE es)   mapper functions    = undefined

pevalExp (CondE i t e)      mapper functions    = (staticI && staticT && staticE, CondE resI resT resE) 
    where (staticI, resI) = pevalExp i mapper functions
          (staticT, resT) = pevalExp t mapper functions
          (staticE, resE) = pevalExp e mapper functions
 
pevalExp (MultiIfE gs)      mapper functions    = undefined
pevalExp (LetE decs e)      mapper functions    = undefined
pevalExp (CaseE e ms)       mapper functions    = undefined
pevalExp (DoE stmts)        mapper functions    = undefined
pevalExp (CompE stmts)      mapper functions    = undefined
pevalExp (ArithSeqE r)      mapper functions    = undefined
pevalExp (ListE [])         mapper functions    = (True, ListE [])
pevalExp (ListE es)         mapper functions    =   let pevaledEs = map (\e -> pevalExp e mapper functions) es
                                                        static = allStatic pevaledEs
                                                        residual = ListE (residuals pevaledEs)
                                                    in (static, residual)

pevalExp (SigE _ _)         mapper functions    = error "I can not pevaluate a signature, sorry..."
pevalExp (RecConE n fes)    mapper functions    = undefined
pevalExp (RecUpdE e fes)    mapper functions    = undefined
pevalExp (StaticE e)        mapper functions    = undefined
pevalExp (UnboundVarE x)    mapper functions    = (False, UnboundVarE x)


-- unfold :: function -> [(static, argument)] -> functions -> (static, result)
unfold :: Exp -> [(Bool, Exp)] -> [(Name, Dec)] -> (Bool, Exp)
unfold (LamE pats e)    args functions  = case patternMatch pats args of
            Just (mapper, newNames)     -> let (statE, resE) = pevalExp e mapper functions
                                               residual = if statE then resE else compress $ LetE newNames resE
                                           in (statE, residual) 
            Nothing                     -> error "Non-exhaustive patterns in lambda"
unfold (VarE f)         args functions  = case lookup f functions of
            Just declaration            -> pevalDec declaration args functions
            Nothing                     -> let static = False
                                               residual = reconstructAppE (VarE f) args
                                           in (static, residual)


-- staticMatch :: pattern -> static expression -> Maybe mapper
staticMatch :: Pat -> Exp -> Maybe [(Name, Exp)]

staticMatch (LitP lp) (LitE le)
    | lp == le                      = Just []
    | otherwise                     = Nothing

staticMatch (VarP v)   exp          = Just [(v, exp)]

staticMatch (TupP ps)  (TupE es)    = foldr1 (\ a b -> unionMaybe a b (++)) $ zipWith staticMatch ps es 
staticMatch (UnboxedTupP _)     _   = undefined

staticMatch (ConP pName pArgs)  e
    | pName == '[]                  = if e == (ListE []) then Just [] else Nothing
    | otherwise                     = staticMatchCon (reverse pArgs) e 

    where   staticMatchCon :: [Pat] -> Exp -> Maybe [(Name, Exp)]
            staticMatchCon []       (ConE eName)
                | pName == eName                    = Just []
                | otherwise                         = Nothing
            staticMatchCon (p:ps) (AppE cExp arg)   = unionMaybe (staticMatch p arg) (staticMatchCon ps cExp) (++)
            staticMatchCon _    _                   = Nothing

staticMatch (InfixP p1 f p2)    e
    | f == '(:)                     = case e of
                ListE (e1:e2)   -> unionMaybe (staticMatch p1 e1) (staticMatch p2 (ListE e2)) (++)
                _               -> Nothing
    | otherwise                     = undefined
staticMatch (UInfixP _ _ _)    _    = undefined -- Where is the difference between InfixP and UInfixP?

staticMatch (ParensP p)         e   = staticMatch p e
staticMatch  p         (ParensE e)  = staticMatch p e

staticMatch (TildeP _)          _   = undefined
staticMatch (BangP _)           _   = undefined

staticMatch (AsP name pat)      e   = unionMaybe (Just (name, e)) (staticMatch pat e) (:)

staticMatch (WildP)             _   = Just []

staticMatch (RecP _ _)          _   = undefined

staticMatch (ListP [])     (ListE [])       = Just []
staticMatch (ListP (p:ps)) (ListE (e:es))   = unionMaybe (staticMatch p e) (staticMatch (ListP ps) (ListE es)) (++)

staticMatch (SigP _ _)          _   = undefined -- error "Signatur?? No no."
staticMatch (ViewP _ _)         _   = undefined

staticMatch _                   _   = Nothing


-- Helper -- TODO auslagern

-- reconstructAppE :: "function" or similar -> [(static, argument)] -> AppE ... AppE fun arg ... arg
reconstructAppE :: Exp -> [(Bool, Exp)] -> Exp
reconstructAppE e []        = e
reconstructAppE e (a:args)  =   let (_, arg) = a
                                in  reconstructAppE (AppE e arg) args

unionMaybe :: Maybe a -> Maybe b -> (a -> b -> c) -> Maybe c
unionMaybe (Just u) (Just v) f = Just (f u v)
unionMaybe  _        _       _ = Nothing

valDec :: Pat -> Exp -> Dec
valDec p e = ValD p (NormalB e) []

static :: (Bool, a) -> Bool
static = fst

residual :: (Bool, a) -> a
residual = snd

allStatic :: [(Bool, a)] -> Bool
allStatic = (foldr1 (&&)) . (map fst)

existStatic :: [(Bool, a)] -> Bool
existStatic = (foldr1 (||)) . (map fst)

residuals :: [(Bool, a)] -> [a]
residuals = map snd


patternMatch :: [Pat] -> [(Bool, Exp)] -> Maybe ([(Name, Exp)], [Dec])

patternMatch  []       []      = Just ([], [])
patternMatch (p:pats) (a:args) = unionMaybe processedHead processedTail tupConcat
         where   processedHead = processArg p a
                 processedTail = patternMatch pats args
                 tupConcat (map1, decs1) (map2, decs2) = (map1 ++ map2, decs1 ++ decs2)
patternMatch  _        _       = Nothing

processArg :: Pat -> (Bool, Exp) -> Maybe ([(Name, Exp)], [Dec])
processArg pat (static, arg) = if static
                                        then case staticMatch pat arg of
                                            Just mapper -> Just (mapper, [])
                                            Nothing     -> Nothing
                                    else Just ([], [valDec pat arg])


compress :: Exp -> Exp
compress (LetE [] x) = x
compress (LamE [] x) = x
compress  other      = other
