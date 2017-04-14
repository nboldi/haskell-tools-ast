{-# LANGUAGE LambdaCase
           , FlexibleContexts
           #-}
-- | This module converts range templates into source templates.
-- Basically it reads the source file and attaches parts of the source file to the AST elements that have the range of the given source code fragment.
module Language.Haskell.Tools.Transform.RangeTemplateToSourceTemplate where

import Control.Monad.Identity
import Control.Monad.State
import Control.Reference
import Data.Map as Map
import Data.Ord (Ord(..), Ordering(..))
import Data.Set as Set
import Data.List
import FastString (mkFastString)
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform.RangeTemplate
import Language.Haskell.Tools.Transform.SourceTemplate
import SrcLoc
import StringBuffer (StringBuffer, nextChar, atEnd)

rangeToSource :: SourceInfoTraversal node => StringBuffer -> Ann node dom RngTemplateStage
                                                          -> Ann node dom SrcTemplateStage
rangeToSource srcInput tree = let locIndices = getLocIndices tree
                                  srcMap = mapLocIndices srcInput locIndices
                               in applyFragments (Map.elems srcMap) tree

-- maps could be strict

-- | Assigns an index (in the order they are used) for each range
getLocIndices :: SourceInfoTraversal e => Ann e dom RngTemplateStage -> Set (RealSrcLoc, Int)
getLocIndices = snd . flip execState (0, Set.empty) .
  sourceInfoTraverseDown (SourceInfoTrf
      (\ni -> do { mapM_ (\el -> case getRangeElemSpan el of Just sp -> modify (insertElem sp); _ -> return ()) (ni ^. rngTemplateNodeElems); return ni })
      (\ni -> do { mapM_ (modify . insertElem) (ni ^. rngTmpSeparators); return ni })
      pure )
    (return ()) (return ())
  where insertElem sp (i,m) = (i+1, Set.insert (realSrcSpanEnd sp, i) m)

-- | Partitions the source file in the order where the parts are used in the AST
mapLocIndices :: Ord k => StringBuffer -> Set (RealSrcLoc, k) -> Map k String
mapLocIndices inp = (^. _1) . Set.foldl (\(new, str, pos) (sp, k) -> let (rem, val, newPos) = takeSpan str pos sp
                                                                      in (Map.insert k (reverse val) new, rem, newPos))
                                        (Map.empty, inp, mkRealSrcLoc (mkFastString "") 1 1)
  where takeSpan :: StringBuffer -> RealSrcLoc -> RealSrcLoc -> (StringBuffer, String, RealSrcLoc)
        takeSpan str pos end = takeSpan' end (str,"", pos)

        takeSpan' :: RealSrcLoc -> (StringBuffer, String, RealSrcLoc) -> (StringBuffer, String, RealSrcLoc)
        takeSpan' end (sb, taken, pos) | (srcLocLine pos `compare` srcLocLine end) `thenCmp` (srcLocCol pos `compare` srcLocCol end) == LT && not (atEnd sb)
          = let (c,rem) = nextChar sb in takeSpan' end (rem, c:taken, advanceSrcLoc pos c)
        takeSpan' _ (rem, taken, pos) = (rem, taken, pos)

        thenCmp EQ o2 = o2
        thenCmp o1 _  = o1

-- | Replaces the ranges in the AST with the source file parts
applyFragments :: SourceInfoTraversal node => [String] -> Ann node dom RngTemplateStage
                                                       -> Ann node dom SrcTemplateStage
applyFragments srcs = flip evalState srcs
  . sourceInfoTraverseDown (SourceInfoTrf
     (\ni -> do template <- mapM getTextFor (ni ^. rngTemplateNodeElems)
                return $ SourceTemplateNode (RealSrcSpan $ ni ^. rngTemplateNodeRange) (concat template) 0 Nothing)
     (\(RangeTemplateList rng bef aft sep indented seps)
         -> do (own, rest) <- splitAt (length seps) <$> get
               put rest
               return (SourceTemplateList (RealSrcSpan rng) bef aft sep indented (Prelude.map ((:[]) . NormalText) own) 0 Nothing))
     (\(RangeTemplateOpt rng bef aft) -> return (SourceTemplateOpt (RealSrcSpan rng) bef aft 0 Nothing)))
     (return ()) (return ())
  where getTextFor RangeChildElem = return [ChildElem]
        getTextFor (RangeElem _) = do (src:rest) <- get
                                      put rest
                                      return [TextElem [NormalText src]]

extractStayingElems :: SourceInfoTraversal node => Ann node dom SrcTemplateStage -> Ann node dom SrcTemplateStage
extractStayingElems = runIdentity . sourceInfoTraverse (SourceInfoTrf
    (sourceTemplateNodeElems & traversal & sourceTemplateTextElem !- breakStaying)
    (srcTmpSeparators & traversal !- breakStaying)
    pure)

    where breakStaying :: [SourceTemplateTextElem] -> [SourceTemplateTextElem]
          breakStaying = concat . Prelude.map (\(NormalText s) -> toTxtElems s)

          toTxtElems :: String -> [SourceTemplateTextElem]
          toTxtElems = extractStaying . lines
          extractStaying lines = Prelude.foldr appendTxt []
                                   $ Prelude.map (\ln -> if "#" `isPrefixOf` ln then StayingText ln else NormalText ln) lines
          appendTxt (NormalText n1) (NormalText n2 : rest) = NormalText (n1 ++ '\n':n2) : rest
          appendTxt (StayingText n1) (StayingText n2 : rest) = StayingText (n1 ++ '\n':n2) : rest
          appendTxt e (next:ls) = case reverse (e ^. sourceTemplateText) of
                                              -- fix '\r' characters that are separated from '\n'
                                    '\r':_ -> (sourceTemplateText .- init $ e) : (sourceTemplateText .- ('\r':) $ next) : ls
                                    _      -> e : next : ls
          appendTxt e ls = e : ls
