{-# LANGUAGE RankNTypes
           , ConstraintKinds
           , FlexibleContexts
           , TypeFamilies
           #-}
module Language.Haskell.Tools.Refactor.Predefined.InlineBinding (inlineBinding, InlineBindingDomain) where

import Control.Reference
import Control.Monad.Writer hiding (Alt)
import Control.Monad.State
import Data.Maybe
import Data.List (nub)

import SrcLoc as GHC
import Name as GHC

import Language.Haskell.Tools.Refactor as AST
import Language.Haskell.Tools.AST as AST

tryItOut moduleName sp = tryRefactor (localRefactoring $ inlineBinding (readSrcSpan (toFileName "." moduleName) sp)) moduleName

type InlineBindingDomain dom = ( HasNameInfo dom, HasDefiningInfo dom, HasScopeInfo dom )

-- TODO: check that the name is not used outside of the module

inlineBinding :: InlineBindingDomain dom => RealSrcSpan -> LocalRefactoring dom
inlineBinding span mod = inlineBinding' (nodesContaining span) (nodesContaining span) (getValBindInList span) mod

inlineBinding' :: InlineBindingDomain dom 
                    => Simple Traversal (Module dom) (DeclList dom) 
                    -> Simple Traversal (Module dom) (LocalBindList dom) 
                    -> (forall d . (BindingElem d) => AnnList d dom -> Maybe (ValueBind dom))
                    -> LocalRefactoring dom
inlineBinding' topLevelRef localRef elemAccess mod
  = let removed = catMaybes $ map elemAccess (mod ^? topLevelRef) ++ map elemAccess (mod ^? localRef)
     in case removed of 
          [] -> refactError "No binding is selected."
          removedBinding:_ -> 
            do replacement <- createReplacement removedBinding
               let [removedBindingName] = nub $ catMaybes $ map semanticsName (removedBinding ^? bindingName)
                   mod' = removeBindingAndSig topLevelRef localRef removedBindingName mod
                   mod'' = biplateRef .- replaceInvocations removedBindingName replacement $ mod'
               return mod''

-- | Removes the inlined binding
removeBindingAndSig :: InlineBindingDomain dom 
                         => Simple Traversal (Module dom) (DeclList dom) 
                         -> Simple Traversal (Module dom) (LocalBindList dom) 
                         -> GHC.Name -> AST.Module dom
                         -> AST.Module dom
removeBindingAndSig topLevelRef localRef name
  = (topLevelRef .- removeBindingAndSig' name) . (localRef .- removeBindingAndSig' name)

removeBindingAndSig' :: (InlineBindingDomain dom, BindingElem d) => GHC.Name -> AnnList d dom -> AnnList d dom
removeBindingAndSig' name = (annList .- removeNameFromSigBind) . filterList notThatBindOrSig
  where notThatBindOrSig e 
          | Just sb <- e ^? sigBind = nub (map semanticsName (sb ^? tsName & annList & simpleName)) /= [Just name]
          | Just vb <- e ^? valBind = nub (map semanticsName (vb ^? bindingName)) /= [Just name]
          -- TODO: also remove from fixity signatures
          | otherwise               = True
        
        removeNameFromSigBind d | Just sb <- d ^? sigBind 
          = createTypeSig $ tsName .- filterList (\n -> semanticsName (n ^. simpleName) /= Just name) $ sb
          | otherwise = d

replaceInvocations :: InlineBindingDomain dom => GHC.Name -> (Int -> Expr dom) -> Expr dom -> Expr dom
replaceInvocations name replacement (Var n) | semanticsName (n ^. simpleName) == Just name  
  = replacement 0
-- TODO: replace applied
replaceInvocations _ _ other = other

createReplacement :: ValueBind dom -> LocalRefactor dom (Int -> Expr dom)
createReplacement (SimpleBind (VarPat _) (UnguardedRhs e) locals) 
  = return $ const (wrapLocals locals e)
createReplacement (SimpleBind _ _ _)
  = refactError "Cannot inline, illegal simple bind. Only variable left-hand sides and unguarded right-hand sides are accepted."
createReplacement (FunctionBind matches) 
  -- TODO: no case x, y if the variables aren't actually pattern matched 
  = return $ const (mkLambda (map mkVarPat newArgs) $ mkCase (mkTuple $ map mkVar newArgs) $ map replaceMatch (matches ^? annList))
  where numArgs = getArgNum (head (matches ^? annList & matchLhs))
        getArgNum (MatchLhs n (AnnList args)) = length args
        getArgNum (InfixLhs _ _ _ (AnnList more)) = length more + 2
        
        newArgs = map (mkName . ("x" ++ ) . show) [1..numArgs]

replaceMatch :: Match dom -> Alt dom
replaceMatch (Match lhs rhs locals) = mkAlt (toPattern lhs) (toAltRhs rhs) (locals ^? annJust)
  where toPattern (MatchLhs _ (AnnList pats)) = mkTuplePat pats
        toPattern (InfixLhs lhs _ rhs (AnnList more)) = mkTuplePat (lhs:rhs:more)

        toAltRhs (UnguardedRhs expr) = mkCaseRhs expr
        toAltRhs (GuardedRhss (AnnList rhss)) = mkGuardedCaseRhss (map toAltGuardedRhs rhss)

        toAltGuardedRhs (GuardedRhs (AnnList guards) expr) = mkGuardedCaseRhs guards expr

wrapLocals :: MaybeLocalBinds dom -> Expr dom -> Expr dom
wrapLocals bnds = case bnds ^? annJust & localBinds & annList of 
                    [] -> id
                    localBinds -> mkLet localBinds 