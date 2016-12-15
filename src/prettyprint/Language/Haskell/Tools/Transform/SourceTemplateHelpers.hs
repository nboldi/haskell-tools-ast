{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           , FlexibleInstances
           #-}
-- | Helper functions for working with source templates
module Language.Haskell.Tools.Transform.SourceTemplateHelpers where

import SrcLoc
import Data.String
import Data.List
import Control.Reference
import Data.Function (on)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform.SourceTemplate

type ASTElement node dom = Ann node dom SrcTemplateStage
type ASTOptional node dom = AnnMaybeG node dom SrcTemplateStage
type ASTMulti node dom = AnnListG node dom SrcTemplateStage

instance IsString (SpanInfo SrcTemplateStage) where
  fromString s = SourceTemplateNode noSrcSpan [TextElem s] 0 Nothing
<<<<<<< HEAD

-- * Basic elements
=======
>>>>>>> master
    
child :: SpanInfo SrcTemplateStage
child = SourceTemplateNode noSrcSpan [ChildElem] 0 Nothing

opt :: OptionalInfo SrcTemplateStage
opt = SourceTemplateOpt noSrcSpan "" "" 0 Nothing
<<<<<<< HEAD

list :: ListInfo SrcTemplateStage
list = SourceTemplateList noSrcSpan "" "" "" False [] 0 Nothing

-- * Modifiers

class AfterBefore i where
  -- | Put the given string before the element if it is not empty
  after :: String -> i -> i
  -- | The given string should follow the element if it is not empty
  followedBy :: String -> i -> i

instance AfterBefore (ListInfo SrcTemplateStage) where
  after str = srcTmpListBefore .= str
  followedBy str = srcTmpListAfter .= str

instance AfterBefore (OptionalInfo SrcTemplateStage) where
  after str = srcTmpOptBefore .= str
  followedBy str = srcTmpOptAfter .= str

class RelativeIndent i where
  -- | The element should be indented relatively to its parent
  relativeIndented :: Int -> i -> i

instance RelativeIndent (SpanInfo SrcTemplateStage) where
  relativeIndented i = srcTmpRelPos .= Just i

instance RelativeIndent (ListInfo SrcTemplateStage) where
  relativeIndented i = srcTmpListRelPos .= Just i

instance RelativeIndent (OptionalInfo SrcTemplateStage) where
  relativeIndented i = srcTmpOptRelPos .= Just i


class MinimumIndent i where
  -- | The elements should be indented at least to the given number of spaces
  minimumIndented :: Int -> i -> i

instance MinimumIndent (SpanInfo SrcTemplateStage) where
  minimumIndented i = sourceTemplateMinimalIndent .= i

instance MinimumIndent (ListInfo SrcTemplateStage) where
  minimumIndented i = srcTmpListMinimalIndent .= i

instance MinimumIndent (OptionalInfo SrcTemplateStage) where
  minimumIndented i = srcTmpOptMinimalIndent .= i

-- | The elements of the list should be separated by the given string by default (might be overridden)
separatedBy :: String -> ListInfo SrcTemplateStage -> ListInfo SrcTemplateStage
separatedBy sep = srcTmpDefaultSeparator .= sep

-- | The elements of the list should be indented on the same column
indented :: ListInfo SrcTemplateStage -> ListInfo SrcTemplateStage
indented = (srcTmpIndented .= True) . (srcTmpDefaultSeparator .= "\n")
=======

optBefore :: String -> OptionalInfo SrcTemplateStage
optBefore s = SourceTemplateOpt noSrcSpan s "" 0 Nothing

optAfter :: String -> OptionalInfo SrcTemplateStage
optAfter s = SourceTemplateOpt noSrcSpan "" s 0 Nothing

optBeforeAfter :: String -> String -> OptionalInfo SrcTemplateStage
optBeforeAfter bef aft = SourceTemplateOpt noSrcSpan bef aft 0 Nothing

indentRelative :: Int -> OptionalInfo SrcTemplateStage -> OptionalInfo SrcTemplateStage
indentRelative i = srcTmpOptRelPos .= Just i

list :: ListInfo SrcTemplateStage
list = SourceTemplateList noSrcSpan "" "" "" False [] 0 Nothing

indentedList :: ListInfo SrcTemplateStage
indentedList = SourceTemplateList noSrcSpan "" "" "\n" True [] 0 Nothing

indentedListBefore :: String -> ListInfo SrcTemplateStage
indentedListBefore bef = SourceTemplateList noSrcSpan bef "" "\n" True [] 0 Nothing

indentedListAfter :: String -> ListInfo SrcTemplateStage
indentedListAfter aft = SourceTemplateList noSrcSpan "" aft "\n" True [] 0 Nothing

listSep :: String -> ListInfo SrcTemplateStage
listSep s = SourceTemplateList noSrcSpan "" "" s False [] 0 Nothing

listSepBefore :: String -> String -> ListInfo SrcTemplateStage
listSepBefore s bef = SourceTemplateList noSrcSpan bef "" s False [] 0 Nothing

listSepAfter :: String -> String -> ListInfo SrcTemplateStage
listSepAfter s aft = SourceTemplateList noSrcSpan "" aft s False [] 0 Nothing

listSepBeforeAfter :: String -> String -> String -> ListInfo SrcTemplateStage
listSepBeforeAfter s bef aft = SourceTemplateList noSrcSpan bef aft s False [] 0 Nothing
>>>>>>> master

-- | Concatenates two source templates to produce a new template with all child elements.
(<>) :: SpanInfo SrcTemplateStage -> SpanInfo SrcTemplateStage -> SpanInfo SrcTemplateStage
SourceTemplateNode sp1 el1 _ _ <> SourceTemplateNode sp2 el2 _ _ = SourceTemplateNode (combineSrcSpans sp1 sp2) (el1 ++ el2) 0 Nothing
