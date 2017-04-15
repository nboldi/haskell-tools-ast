{-# LANGUAGE LambdaCase, TupleSections, AllowAmbiguousTypes #-}
module Language.Haskell.Tools.Refactor.ChangeAST (removeChild, removeSeparator) where

import Control.Reference
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform
import Language.Haskell.Tools.Refactor.RefactorBase
import SrcLoc

import Debug.Trace

removeSeparator :: ([SourceTemplateTextElem], SrcSpan) -> LocalRefactor dom ()
removeSeparator (txts, range) = tell $ (if not (null staying) then map (Right . (range,)) staying else [])
  where staying = map (^. sourceTemplateText) . filter isStayingText $ txts

removeChild :: (SourceInfoTraversal e) => e dom SrcTemplateStage -> LocalRefactor dom ()
removeChild e = tell $ map Right $ keptText e

keptText :: SourceInfoTraversal e => e dom SrcTemplateStage -> [(SrcSpan,String)]
keptText = execWriter . sourceInfoTraverse (SourceInfoTrf
  (\ni -> mapM_ writeStaying (mapMaybe (\case (TextElem elems range) -> Just (elems,range)
                                              _ -> Nothing)
                                       (ni ^? sourceTemplateNodeElems & traversal)) >> return ni)
  (\ni -> mapM_ writeStaying (ni ^. srcTmpSeparators) >> return ni)
  pure)

writeStaying :: ([SourceTemplateTextElem], SrcSpan) -> Writer [(SrcSpan,String)] ()
writeStaying (txts, range) = if not (null staying) then tell (map (range,) staying) else return ()
  where staying = map (^. sourceTemplateText) . filter isStayingText $ txts
