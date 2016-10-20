{-# LANGUAGE FlexibleContexts 
           , LambdaCase 
           , RankNTypes 
           , ScopedTypeVariables
           , TypeFamilies
           , FlexibleInstances
           , UndecidableInstances
           #-}

-- | Helper functions for using the AST.
module Language.Haskell.Tools.AST.Helpers where

import SrcLoc
import qualified Name as GHC

import Control.Reference
import Control.Monad
import Data.List
import Data.Maybe
import Data.Function hiding ((&))
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Names
import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.AST.SemaInfoClasses

import Debug.Trace

ordByOccurrence :: Ann UQualifiedName dom stage -> Ann UQualifiedName dom stage -> Ordering
ordByOccurrence = compare `on` nameElements

-- | The occurrence of the name.
nameString :: Ann UQualifiedName dom stage -> String
nameString = intercalate "." . nameElements

-- | The qualifiers and the unqualified name
nameElements :: Ann UQualifiedName dom stage -> [String]
nameElements n = (n ^? qualifiers&annList&simpleNameStr) 
                    ++ [n ^. unqualifiedName&simpleNameStr]

-- | The qualifier of the name
nameQualifier :: Ann UQualifiedName dom stage -> [String]
nameQualifier n = n ^? qualifiers&annList&simpleNameStr
         
-- | Does the import declaration import only the explicitly listed elements?
importIsExact :: Ann UImportDecl dom stage -> Bool
importIsExact = isJust . (^? importSpec&annJust&importSpecList)  
  
-- | Does the import declaration has a 'hiding' clause?
importIsHiding :: Ann UImportDecl dom stage -> Bool
importIsHiding = isJust . (^? importSpec&annJust&importSpecHiding)
       
-- | All elements that are explicitly listed to be imported in the import declaration
importExacts :: Simple Traversal (Ann UImportDecl dom stage) (Ann UIESpec dom stage)
importExacts = importSpec&annJust&importSpecList&annList

-- | All elements that are hidden in an import
importHidings :: Simple Traversal (Ann UImportDecl dom stage) (Ann UIESpec dom stage)
importHidings = importSpec&annJust&importSpecList&annList
         
-- | Possible qualifiers to use imported definitions         
importQualifiers :: Ann UImportDecl dom stage -> [[String]]
importQualifiers imp 
  = (if isAnnNothing (imp ^. importQualified) then [[]] else [])
      ++ [imp ^? importAs&annJust&importRename&moduleNameString]
        
bindingName :: (SemanticInfo dom UQualifiedName ~ ni) => Simple Traversal (Ann UValueBind dom stage) ni
bindingName = (valBindPat&patternName&simpleName 
                        &+& funBindMatches&annList&matchLhs
                              &(matchLhsName&simpleName &+& matchLhsOperator&operatorName))
                     &semantics
                     
declHeadNames :: Simple Traversal (Ann UDeclHead dom stage) (Ann UQualifiedName dom stage)
declHeadNames = (dhName&simpleName &+& dhBody&declHeadNames &+& dhAppFun&declHeadNames &+& dhOperator&operatorName)

               
typeParams :: Simple Traversal (Ann UType dom stage) (Ann UType dom stage)
typeParams = fromTraversal typeParamsTrav
  where typeParamsTrav f (Ann a (UTyFun p r)) = Ann a <$> (UTyFun <$> f p <*> typeParamsTrav f r)
        typeParamsTrav f (Ann a (UTyForall vs t)) = Ann a <$> (UTyForall vs <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (UTyCtx ctx t)) = Ann a <$> (UTyCtx ctx <$> typeParamsTrav f t)
        typeParamsTrav f (Ann a (UTyParen t)) = Ann a <$> (UTyParen <$> typeParamsTrav f t)
        typeParamsTrav f t = f t
        

-- | Access the semantic information of an AST node.
semantics :: Simple Lens (Ann elem dom stage) (SemanticInfo dom elem)
semantics = annotation&semanticInfo

dhNames :: (SemanticInfo dom UQualifiedName ~ k) => Simple Traversal (Ann UDeclHead dom stage) k
dhNames = declHeadNames & semantics

-- | Get all nodes that contain a given source range
nodesContaining :: (HasRange (inner dom stage), Biplate (node dom stage) (inner dom stage), SourceInfo stage) 
                => RealSrcSpan -> Simple Traversal (node dom stage) (inner dom stage)
nodesContaining rng = biplateRef & filtered (isInside rng) 

-- | Return true if the node contains a given range
isInside :: HasRange (inner dom stage) => RealSrcSpan -> inner dom stage -> Bool
isInside rng nd = case getRange nd of RealSrcSpan sp -> sp `containsSpan` rng
                                      _              -> False

-- | Get all nodes that are contained in a given source range
nodesContained :: (HasRange (inner dom stage), Biplate (node dom stage) (inner dom stage), SourceInfo stage) 
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
                    
-- | True, if the node has the given range                     
hasRange :: SourceInfo stage => RealSrcSpan -> Ann inner dom stage -> Bool
hasRange rng node = case getRange node of RealSrcSpan sp -> sp == rng
                                          _              -> False

-- | Get the shortest source range that contains the given 
getNodeContaining :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage, HasRange (Ann inner dom stage)) 
                  => RealSrcSpan -> Ann node dom stage -> Maybe (Ann inner dom stage)
getNodeContaining sp node = case node ^? nodesContaining sp of
  [] -> Nothing
  results -> Just $ minimumBy (compareRangeLength `on` getRange) results

-- | Compares two NESTED source spans based on their lengths
compareRangeLength :: SrcSpan -> SrcSpan -> Ordering
compareRangeLength (RealSrcSpan sp1) (RealSrcSpan sp2)
  = (lineDiff sp1 `compare` lineDiff sp2) `mappend` (colDiff sp1 `compare` colDiff sp2)
  where lineDiff sp = srcLocLine (realSrcSpanStart sp) - srcLocLine (realSrcSpanEnd sp)
        colDiff sp = srcLocCol (realSrcSpanStart sp) - srcLocCol (realSrcSpanEnd sp)

getNode :: (Biplate (Ann node dom stage) (Ann inner dom stage), SourceInfo stage) 
        => RealSrcSpan -> Ann node dom stage -> Ann inner dom stage
getNode sp node = case node ^? nodesWithRange sp of
  [] -> error "getNode: The node cannot be found"
  [n] -> n
  _ -> error "getNode: Multiple nodes"

-- | A class to access the names of named elements. Have to locate where does the AST element store its name.
-- The returned name will be the one that was marked isDefining.
class NamedElement elem where
  elementName :: elem -> [GHC.Name]

instance HasNameInfo dom => NamedElement (Ann UDecl dom st) where
  elementName d = catMaybes names
    where names = map semanticsName (d ^? declHead & dhNames) 
                    ++ map semanticsName (d ^? declTypeFamily & tfHead & dhNames)
                    ++ map semanticsName (d ^? declValBind & bindingName)
                    ++ map semanticsName (d ^? declName & simpleName & semantics)
                    ++ map semanticsName (d ^? declPatSyn & patLhs & (patName & simpleName &+& patSynOp & operatorName) & semantics)
