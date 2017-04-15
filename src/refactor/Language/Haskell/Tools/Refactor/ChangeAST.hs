{-# LANGUAGE LambdaCase, TupleSections #-}
module Language.Haskell.Tools.Refactor.ChangeAST (removeChild, removeSeparator) where

import Control.Reference
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform
import SrcLoc

removeSeparator :: SourceInfoTraversal p => ([SourceTemplateTextElem], SrcSpan) -> p dom SrcTemplateStage -> p dom SrcTemplateStage
removeSeparator (txts, range) = insertText (if not (null staying) then map (range,) staying else [])
  where staying = map (^. sourceTemplateText) . filter isStayingText $ txts

removeChild :: (SourceInfoTraversal e, SourceInfoTraversal p) => e dom SrcTemplateStage -> p dom SrcTemplateStage -> p dom SrcTemplateStage
removeChild e = insertText (keptText e)

insertText :: SourceInfoTraversal p => [(SrcSpan,String)] -> p dom SrcTemplateStage -> p dom SrcTemplateStage
insertText inserted p = evalState (sourceInfoTraverse (SourceInfoTrf
  (sourceTemplateNodeElems & traversal !~ takeWhatPrecedesElem)
  (srcTmpSeparators & traversal !~ takeWhatPrecedesSep)
  pure) p) inserted

takeWhatPrecedesSep :: ([SourceTemplateTextElem], SrcSpan) -> State [(SrcSpan,String)] ([SourceTemplateTextElem], SrcSpan)
takeWhatPrecedesSep e@(_,span) = _1 !~ takeWhatPrecedes span $ e

takeWhatPrecedesElem :: SourceTemplateElem -> State [(SrcSpan,String)] SourceTemplateElem
takeWhatPrecedesElem e@(TextElem _ span) = sourceTemplateTextElem !~ takeWhatPrecedes span $ e
takeWhatPrecedesElem e = return e

takeWhatPrecedes :: SrcSpan -> [SourceTemplateTextElem] -> State [(SrcSpan,String)] [SourceTemplateTextElem]
takeWhatPrecedes span elems = do
  (toInsert,remaining) <- gets (partition ((>= srcSpanEnd span) . srcSpanStart . fst))
  put remaining
  return (map (StayingText . snd) toInsert ++ elems)

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
