{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Defines the inline binding refactoring that removes a value binding and replaces all occurences
-- with an expression equivalent to the body of the binding.
module Language.Haskell.Tools.Refactor.Builtin.InlineBinding
  (inlineBinding, tryItOut, inlineBindingRefactoring) where

import Control.Monad.State
import Control.Reference
import Data.Generics.Uniplate.Data ()
import Data.Generics.Uniplate.Operations (Uniplate(..), Biplate(..))
import Data.List (nub)
import Data.Maybe (Maybe(..), catMaybes)

import Name as GHC (NamedThing(..), Name, occNameString)
import SrcLoc as GHC (SrcSpan(..), RealSrcSpan)

import Language.Haskell.Tools.Refactor as AST

inlineBindingRefactoring :: RefactoringChoice
inlineBindingRefactoring = SelectionRefactoring "InlineBinding" inlineBinding

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor inlineBinding


inlineBinding :: RealSrcSpan -> Refactoring
inlineBinding span namedMod@(_,mod) mods
  = let topLevel :: Simple Traversal Module DeclList
        topLevel = nodesContaining span
        local :: Simple Traversal Module LocalBindList
        local = nodesContaining span
        exprs :: Simple Traversal Module Expr
        exprs = nodesContaining span
        elemAccess :: (BindingElem d) => AnnList d -> Maybe ValueBind
        elemAccess = getValBindInList span
        removed = catMaybes $ map elemAccess (mod ^? topLevel) ++ map elemAccess (mod ^? local)
     in case reverse removed of
          [] -> refactError "No binding is selected."
          removedBinding:_ ->
           let [removedBindingName] = nub $ catMaybes $ map semanticsName (removedBinding ^? bindingName)
            in if | any (containInlined removedBindingName) mods
                    -> refactError "Cannot inline the definition, it is used in other modules."
                  | _:_ <- mod ^? modHead & annJust & mhExports & annJust & biplateRef
                                          & filtered (\n -> semanticsName (n :: QualifiedName) == Just removedBindingName)
                    -> refactError "Cannot inline the definition, it is present in the export list."
                  | otherwise -> localRefactoring (inlineBinding' topLevel local exprs removedBinding removedBindingName) namedMod mods

-- | Performs the inline binding on a single module.
inlineBinding' :: Simple Traversal Module DeclList -> Simple Traversal Module LocalBindList 
                    -> Simple Traversal Module Expr -> ValueBind -> GHC.Name -> LocalRefactoring
inlineBinding' topLevelRef localRef exprRef removedBinding removedBindingName mod
  = do replacement <- createReplacement removedBinding
       let RealSrcSpan bindingSpan = getRange removedBinding
       mod' <- removeBindingAndSig topLevelRef localRef exprRef removedBindingName mod
       (mod'', used) <- runStateT (descendBiM (replaceInvocations bindingSpan removedBindingName replacement) mod') False
       if not used
         then refactError "The selected definition is not used, it can be safely deleted."
         else return mod''

-- | True if the given module contains the name of the inlined definition.
containInlined :: GHC.Name -> ModuleDom -> Bool
containInlined name (_,mod)
  = any (\qn -> semanticsName qn == Just name) $ (mod ^? biplateRef :: [QualifiedName])

-- | Removes the inlined binding and the accompanying type and fixity signatures.
removeBindingAndSig :: Simple Traversal Module DeclList -> Simple Traversal Module LocalBindList
                         -> Simple Traversal Module Expr -> GHC.Name -> LocalRefactoring
removeBindingAndSig topLevelRef localRef exprRef name
  = (return . removeEmptyBnds (topLevelRef & annList & declValBind &+& localRef & annList & localVal) exprRef)
      <=< (topLevelRef !~ removeBindingAndSig' name)
      <=< (localRef !~ removeBindingAndSig' name)

removeBindingAndSig' :: (SourceInfoTraversal d, BindingElem d) 
                     => GHC.Name -> AnnList d -> LocalRefactor (AnnList d)
removeBindingAndSig' name ls = do
   bnds <- mapM notThatBindOrSig (ls ^? annList)
   return $ (annList .- removeNameFromSigBind) (filterListIndexed (\i _ -> bnds !! i) ls)
  where notThatBindOrSig e
          | Just sb <- e ^? sigBind = return $ nub (map semanticsName (sb ^? tsName & annList & simpleName)) /= [Just name]
          | Just vb <- e ^? valBind = do
             let isThat = nub (map semanticsName (vb ^? bindingName)) == [Just name]
             when isThat (void $ accessRhs !| checkForRecursion name $ vb)
             return $ not isThat
          | Just fs <- e ^? fixitySig = return $ nub (map semanticsName (fs ^? fixityOperators & annList & operatorName)) /= [Just name]
          | otherwise = return True

        removeNameFromSigBind
          = (sigBind & tsName .- filterList (\n -> semanticsName (n ^. simpleName) /= Just name))
             . (fixitySig & fixityOperators .- filterList (\n -> semanticsName (n ^. operatorName) /= Just name))

        accessRhs = valBindRhs
                      &+& valBindLocals & accessLocalRhs
                      &+& funBindMatches & annList & matchRhs
                      &+& funBindMatches & annList & (matchRhs &+& matchBinds & accessLocalRhs)
        accessLocalRhs = annJust & localBinds & annList & localVal & accessRhs

-- | Check the extracted bindings right-hand-side for possible recursion
checkForRecursion :: GHC.Name -> Rhs -> LocalRefactor ()
checkForRecursion n = void . (biplateRef !| checkNameForRecursion n)

checkNameForRecursion :: GHC.Name -> AST.Name -> LocalRefactor ()
checkNameForRecursion name n
  | semanticsName (n ^. simpleName) == Just name
  = refactError $ "Cannot inline definitions containing direct recursion. Recursive call at: "
                    ++ shortShowSpanWithFile (getRange n)
  | otherwise = return ()

-- | As a top-down transformation, replaces the occurrences of the binding with generated expressions. This method passes
-- the captured arguments of the function call to generate simpler results.
replaceInvocations :: RealSrcSpan -> GHC.Name -> ([[GHC.Name]] -> [Expr] -> Expr) -> Expr 
                        -> StateT Bool LocalRefactor Expr
replaceInvocations bindingRange name replacement expr
  | (Var n, args) <- splitApps expr
  , semanticsName (n ^. simpleName) == Just name
  = do put True
       replacement (map (map (^. _1)) $ semanticsScope expr)
         <$> mapM (descendM (replaceInvocations bindingRange name replacement)) args
  | otherwise
  = descendM (replaceInvocations bindingRange name replacement) expr

-- | Splits an application into function and arguments. Works also for operators.
splitApps :: Expr -> (Expr, [Expr])
splitApps (App f a) = case splitApps f of (fun, args) -> (fun, args ++ [a])
splitApps (InfixApp l (NormalOp qn) r) = (mkVar (mkParenName qn), [l,r])
splitApps (InfixApp l (BacktickOp qn) r) = (mkVar (mkNormalName qn), [l,r])
splitApps (Paren expr) = splitApps expr
splitApps expr = (expr, [])

-- | Rejoins the function and the arguments as an expression.
joinApps :: Expr -> [Expr] -> Expr
joinApps f [] = f
joinApps f args = parenIfNeeded (foldl mkApp f args)

-- | Create an expression that is equivalent to calling the given bind.
createReplacement :: ValueBind -> LocalRefactor ([[GHC.Name]] -> [Expr] -> Expr)
createReplacement (SimpleBind (VarPat _) (UnguardedRhs e) locals)
  = return $ \_ args -> joinApps (parenIfNeeded $ wrapLocals locals e) args
createReplacement (SimpleBind _ _ _)
  = refactError "Cannot inline, illegal simple bind. Only variable left-hand sides and unguarded right-hand sides are accepted."
createReplacement (FunctionBind (AnnList [Match lhs (UnguardedRhs expr) locals]))
  = return $ \_ args -> let (argReplacement, matchedPats, appliedArgs) = matchArguments (getArgsOf lhs) args
                         in joinApps (parenIfNeeded (createLambda matchedPats (wrapLocals locals (replaceExprs argReplacement expr)))) appliedArgs
  where getArgsOf (MatchLhs _ (AnnList args)) = args
        getArgsOf (InfixLhs lhs _ rhs (AnnList more)) = lhs:rhs:more
createReplacement (FunctionBind matches)
                                                 -- function bind has at least one match
  = return $ \sc args -> let numArgs = getArgNum (head (matches ^? annList & matchLhs)) - length args
                             newArgs = take numArgs $ map mkName $ filter notInScope $ map (("x" ++ ) . show @Int) [1..]
                             notInScope str = not $ any (any ((== str) . occNameString . getOccName)) sc
                          in parenIfNeeded $ createLambda (map mkVarPat newArgs)
                                           $ mkCase (mkTuple $ map mkVar newArgs ++ args)
                                           $ map replaceMatch (matches ^? annList)
  where getArgNum (MatchLhs _ (AnnList args)) = length args
        getArgNum (InfixLhs _ _ _ (AnnList more)) = length more + 2

-- | Replaces names with expressions according to a mapping.
replaceExprs :: [(GHC.Name, Expr)] -> Expr -> Expr
replaceExprs [] = id
replaceExprs replaces = (uniplateRef .-) $ \case
    Var n | Just name <- semanticsName (n ^. simpleName)
          , Just replace <- lookup name replaces
          -> replace
    e -> e

-- | Matches a pattern list with an expression list and generates bindings. Matches until an argument cannot be matched.
matchArguments :: [Pattern] -> [Expr] -> ([(GHC.Name, Expr)], [Pattern], [Expr])
matchArguments (ParenPat p : pats) exprs = matchArguments (p:pats) exprs
matchArguments (p:pats) (e:exprs)
  | Just replacement <- staticPatternMatch p e
  = case matchArguments pats exprs of (replacements, patterns, expressions) -> (replacement ++ replacements, patterns, expressions)
  | otherwise
  = ([], p:pats, e:exprs)
matchArguments pats [] = ([], pats, [])
matchArguments [] exprs = ([], [], exprs)

-- | Matches a pattern with an expression. Generates a mapping of names to expressions.
staticPatternMatch :: Pattern -> Expr -> Maybe [(GHC.Name, Expr)]
staticPatternMatch (VarPat n) e
  | Just name <- semanticsName $ n ^. simpleName
  = Just [(name, e)]
staticPatternMatch (AppPat n (AnnList args)) e
  | (Var n', exprs) <- splitApps e
  , length args == length exprs
      && semanticsName (n ^. simpleName) == semanticsName (n' ^. simpleName)
  , Just subs <- sequence $ zipWith staticPatternMatch args exprs
  = Just $ concat subs
staticPatternMatch (TuplePat (AnnList pats)) (Tuple (AnnList args))
  | length pats == length args
  , Just subs <- sequence $ zipWith staticPatternMatch pats args
  = Just $ concat subs
staticPatternMatch _ _ = Nothing

replaceMatch :: Match -> Alt
replaceMatch (Match lhs rhs locals) = mkAlt (toPattern lhs) (toAltRhs rhs) (locals ^? annJust)
  where toPattern (MatchLhs _ (AnnList pats)) = mkTuplePat pats
        toPattern (InfixLhs lhs _ rhs (AnnList more)) = mkTuplePat (lhs:rhs:more)

        toAltRhs (UnguardedRhs expr) = mkCaseRhs expr
        toAltRhs (GuardedRhss (AnnList rhss)) = mkGuardedCaseRhss (map toAltGuardedRhs rhss)

        toAltGuardedRhs (GuardedRhs (AnnList guards) expr) = mkGuardedCaseRhs guards expr

wrapLocals :: MaybeLocalBinds -> Expr -> Expr
wrapLocals bnds = case bnds ^? annJust & localBinds & annList of
                    [] -> id
                    localBinds -> mkLet localBinds

-- | True for patterns that need to be parenthesized if in a lambda
compositePat :: Pattern -> Bool
compositePat (AppPat {}) = True
compositePat (InfixAppPat {}) = True
compositePat (TypeSigPat {}) = True
compositePat (ViewPat {}) = True
compositePat _ = False

parenIfNeeded :: Expr -> Expr
parenIfNeeded e = if compositeExprs e then mkParen e else e

-- | True for expresssions that need to be parenthesized if in application
compositeExprs :: Expr -> Bool
compositeExprs (App {}) = True
compositeExprs (InfixApp {}) = True
compositeExprs (Lambda {}) = True
compositeExprs (Let {}) = True
compositeExprs (If {}) = True
compositeExprs (Case {}) = True
compositeExprs (Do {}) = True
compositeExprs _ = False

createLambda :: [Pattern] -> Expr -> Expr
createLambda [] = id
createLambda pats = mkLambda (map (\p -> if compositePat p then mkParenPat p else p) pats)
