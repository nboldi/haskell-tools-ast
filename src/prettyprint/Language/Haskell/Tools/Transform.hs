-- | A module for preparing the representation of the AST for pretty printing.
module Language.Haskell.Tools.Transform
  ( prepareAST, prepareASTCpp
  -- comment handling
  , placeComments, getNormalComments, getPragmaComments
  -- generating source templates
  , child, opt, list, after, followedBy, relativeIndented, minimumIndented, separatedBy, indented, (<>)
  -- references on source templates
  , sourceTemplateNodeRange, sourceTemplateNodeElems
  , sourceTemplateListRange, srcTmpListBefore, srcTmpListAfter, srcTmpDefaultSeparator, srcTmpIndented, srcTmpSeparators
  , sourceTemplateOptRange, srcTmpOptBefore, srcTmpOptAfter
  -- parts of the transformation, used for debugging purposes
  , rangeToSource, fixRanges, cutUpRanges, getLocIndices, mapLocIndices, fixMainRange, fixCPPSpans
  ) where

import Language.Haskell.Tools.Transform.FixCPPSpans (fixCPPSpans)
import Language.Haskell.Tools.Transform.PlaceComments (getNormalComments, getPragmaComments, placeComments)
import Language.Haskell.Tools.Transform.RangeTemplate ()
import Language.Haskell.Tools.Transform.RangeTemplateToSourceTemplate (rangeToSource, getLocIndices, mapLocIndices)
import Language.Haskell.Tools.Transform.RangeToRangeTemplate (cutUpRanges, fixRanges)
import Language.Haskell.Tools.Transform.SourceTemplate
import Language.Haskell.Tools.Transform.SourceTemplateHelpers

import FastString (mkFastString)
import Language.Haskell.Tools.AST
import SrcLoc
import StringBuffer (StringBuffer, nextChar, atEnd)

-- | Prepares the AST for pretty printing
prepareAST :: StringBuffer -> Ann UModule dom RangeStage -> Ann UModule dom SrcTemplateStage
prepareAST srcBuffer = rangeToSource srcBuffer . cutUpRanges . fixRanges

prepareASTCpp :: StringBuffer -> Ann UModule dom RangeStage -> Ann UModule dom SrcTemplateStage
prepareASTCpp srcBuffer = fixCPPSpans . rangeToSource srcBuffer . cutUpRanges . fixRanges . fixMainRange srcBuffer

fixMainRange :: StringBuffer -> Ann UModule dom RangeStage -> Ann UModule dom RangeStage
fixMainRange buffer mod = setRange (mkSrcSpan (srcSpanStart $ getRange mod) (RealSrcLoc (endPos startPos buffer))) mod
  where startPos = mkRealSrcLoc (mkFastString "") 1 1
        endPos pos buf | atEnd buf = pos
        endPos pos buf = let (ch,buf') = nextChar buf in endPos (advanceSrcLoc pos ch) buf'
