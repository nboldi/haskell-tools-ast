{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Language.Haskell.Tools.Transform.FixCPPSpans where

import qualified Data.Text as Txt
import Control.Monad.State
import Control.Reference

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Transform.SourceTemplate

import Debug.Trace

fixCPPSpans :: Ann UModule dom SrcTemplateStage -> Ann UModule dom SrcTemplateStage
-- the state will be used to pass the removed separators to the correct child
fixCPPSpans = flip evalState [] . (sourceInfoTraverseDown (SourceInfoTrf trfElem trfList pure) (modify ([]:)) (modify tail))
  where trfList = srcTmpSeparators&traversal !~ removeClosing
        trfElem elem = do st <- get
                          case st of
                            -- add the moved element to the end
                            []:(sep:rest):stack -> do put $ []:rest:stack
                                                      return $ sourceTemplateNodeElems .- (++ [TextElem sep]) $ elem
                            _ -> return elem

        removeClosing :: String -> State [[String]] String
        removeClosing sep = let (bef,rest) = Txt.breakOnEnd "#endif" (Txt.pack sep)
                             in do modify (\(act:rest) -> (act ++ [Txt.unpack bef]) : rest)
                                   return $ Txt.unpack rest
