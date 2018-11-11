{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import qualified Name as GHC (Name)
import SrcLoc

import Control.Reference
import Data.Function (($), (.), on)
import Data.Generics.Uniplate.Operations (Biplate(..))
import Data.List (elem, any, minimumBy)
import Data.Maybe (Maybe(..), isJust)

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.AST.Representation.Binds (ULocalBind, UValueBind)
import Language.Haskell.Tools.AST.Representation.Decls (UDeclHead, UDecl)
import Language.Haskell.Tools.AST.Representation.Modules (UImportDecl)
import Language.Haskell.Tools.AST.Representation.Names (UQualifiedName)
import Language.Haskell.Tools.AST.Representation.Patterns (UPattern)
import Language.Haskell.Tools.AST.Representation.Types (UType(..))
import Language.Haskell.Tools.AST.SemaInfoTypes (Scope)

-- | Does the import declaration import only the explicitly listed elements?
importIsExact :: Ann UImportDecl dom stage -> Bool
importIsExact = isJust . (^? importSpec&annJust&importSpecList)

-- | Does the import declaration import all elements that are not excluded explicitly?
importIsHiding :: Ann UImportDecl dom stage -> Bool
importIsHiding = isJust . (^? importSpec&annJust&importSpecHiding)

-- | Accesses the name of a function or value binding
bindingName :: Simple Traversal (Ann UValueBind dom stage) (Ann UQualifiedName dom stage)
bindingName = (valBindPat&patternName&simpleName
                        &+& funBindMatches&annList&matchLhs
                              &(matchLhsName&simpleName &+& matchLhsOperator&operatorName))

-- | Accesses that name of a declaration through the declaration head.
declHeadNames :: Simple Traversal (Ann UDeclHead dom stage) (Ann UQualifiedName dom stage)
declHeadNames = (dhName&simpleName &+& dhBody&declHeadNames &+& dhAppFun&declHeadNames &+& dhOperator&operatorName)

-- | A reference to access type arguments to a type constructor call that may be universally qualified
-- or parenthesized.
typeParams :: Simple Traversal (Ann UType dom stage) (Ann UType dom stage)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (UTyFun p r)) = Ann a <$> (UTyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (UTyForall vs t)) = Ann a <$> (UTyForall vs <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (UTyCtx ctx t)) = Ann a <$> (UTyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (UTyParen t)) = Ann a <$> (UTyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t

valBindPats :: Simple Traversal (Ann UValueBind dom stage) (Ann UPattern dom stage)
valBindPats = valBindPat &+& funBindMatches & annList & matchLhs & (matchLhsArgs & annList &+& matchLhsLhs &+& matchLhsRhs)

-- | Access the semantic information of an AST node.
semantics :: Simple Lens (Ann elem dom stage) (SemanticInfo dom elem)
semantics = annotation&semanticInfo

-- | Get all nodes that contain a given source range
nodesContaining :: (HasRange (inner dom stage), Biplate (node dom stage) (inner dom stage))
                => RealSrcSpan -> Simple Traversal (node dom stage) (inner dom stage)
nodesContaining rng = biplateRef & filtered (isInside rng)

-- | Return true if the node contains a given range
isInside :: HasRange (inner dom stage) => RealSrcSpan -> inner dom stage -> Bool
isInside rng nd = case getRange nd of RealSrcSpan sp -> sp `containsSpan` rng
                                      _              -> False

-- | Get all nodes that are contained in a given source range
nodesContained :: (HasRange (inner dom stage), Biplate (node dom stage) (inner dom stage))
                    => RealSrcSpan -> Simple Traversal (node dom stage) (inner dom stage)
nodesContained rng = biplateRef & filtered (isContained rng)

-- | Return true if the node contains a given range
isContained :: HasRange (inner dom stage) => RealSrcSpan -> inner dom stage -> Bool
isContained rng nd = case getRange nd of RealSrcSpan sp -> rng `containsSpan` sp
                                         _              -> False

-- | Get the nodes that have exactly the given range
nodesWithRange :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage)
               => RealSrcSpan -> Simple Traversal (Ann node dom stage) (Ann inner dom stage)
nodesWithRange rng = biplateRef & filtered (hasRange rng)
  where -- True, if the node has the given range
        hasRange :: SourceInfo stage => RealSrcSpan -> Ann inner dom stage -> Bool
        hasRange rng node = case getRange node of RealSrcSpan sp -> sp == rng
                                                  _              -> False

-- | Get the shortest source range that contains the given
getNodeContaining :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage, HasRange (Ann inner dom stage))
                  => RealSrcSpan -> Ann node dom stage -> Maybe (Ann inner dom stage)
getNodeContaining sp node = case node ^? nodesContaining sp of
  [] -> Nothing
  results -> Just $ minimumBy (compareRangeLength `on` getRange) results

-- | Compares two source spans based on their lengths. Can only used for NESTED spans.
compareRangeLength :: SrcSpan -> SrcSpan -> Ordering
compareRangeLength (RealSrcSpan sp1) (RealSrcSpan sp2)
  = (lineDiff sp1 `compare` lineDiff sp2) `mappend` (colDiff sp1 `compare` colDiff sp2)
  where lineDiff sp = srcLocLine (realSrcSpanStart sp) - srcLocLine (realSrcSpanEnd sp)
        colDiff sp = srcLocCol (realSrcSpanStart sp) - srcLocCol (realSrcSpanEnd sp)

-- | A class to access the names of named elements. Have to locate where does the AST element store its name.
-- The returned name will be the one that was marked isDefining.
class NamedElement elem where
  elementName :: Simple Traversal (Ann elem dom st) (Ann UQualifiedName dom st)

instance NamedElement UDecl where
  elementName = (declHead & declHeadNames)
                  &+& (declTypeFamily & tfHead & declHeadNames)
                  &+& (declValBind & bindingName)
                  &+& (declName & simpleName)
                  &+& (declTypeSig & tsName & annList & simpleName)
                  &+& (declPatSyn & patLhs & (patName & simpleName &+& patSynOp & operatorName))

instance NamedElement ULocalBind where
  elementName = localVal&bindingName
                  &+& localSig&tsName&annList&simpleName
                  &+& localFixity&fixityOperators&annList&operatorName

inScope :: GHC.Name -> Scope -> Bool
inScope n sc = any ((n `elem`) . map (^. _1)) sc

-- * Pattern synonyms for annotated lists and maybes

pattern AnnList :: [Ann elem dom stage] -> AnnListG elem dom stage
pattern AnnList elems <- AnnListG _ elems

pattern AnnNothing :: AnnMaybeG elem dom stage
pattern AnnNothing <- AnnMaybeG _ Nothing

pattern AnnJust :: Ann elem dom stage -> AnnMaybeG elem dom stage
pattern AnnJust elem <- AnnMaybeG _ (Just elem)
