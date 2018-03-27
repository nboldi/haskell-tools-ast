module Language.Haskell.Tools.Refactor.Builtin.FindUsages where
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.JumpToDefinition

import Control.Monad.Writer
import Control.Reference
import Data.Aeson
import FastString
import qualified Name as GHC (Name)
import Control.Monad.State
import SrcLoc as GHC

type SrcArrMonad = StateT QualifiedName QueryMonad [SrcSpan]

getUsagesQuery :: QueryChoice
getUsagesQuery = LocationQuery "GetUsages" getUsages

getUsages :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
getUsages sp (_,mod) mods
  = case selectedName of [n] -> do ctors <- getName n
                                   sources <- evalStateT (findNames qualifiedNames ctors) n
                                   if sources == []
                                        then queryError "No found usages"
                                        else return $ toJSON (map wrapOutSource sources)
                         []  -> queryError "No name is selected."
                         _   -> queryError "Multiple names are selected."
  where
    selectedName :: [QualifiedName]
    selectedName = mod ^? nodesContaining sp
    
    qualifiedNames :: [QualifiedName]
    qualifiedNames = (mod ^? biplateRef) ++ (getQualifiedNames mods)

wrapOutSource :: SrcSpan -> (String, Int)
wrapOutSource sp = (getFile sp, getLineUsage sp)

getFile :: SrcSpan -> String
getFile srcSp
 |Just fileName <- srcSpanFileName_maybe srcSp = unpackFS fileName

getLineUsage :: SrcSpan -> Int
getLineUsage (RealSrcSpan sp) = srcSpanStartLine sp

findNames :: [QualifiedName] -> GHC.Name -> SrcArrMonad
findNames [] _ = lift $ queryError "Definition didn't founded"
findNames x name = return $ map (getRange) (filter (isProperName name) x)

isProperName ::  GHC.Name -> QualifiedName -> Bool
isProperName ghcName qualif
 |Just name <- semanticsName qualif = name == ghcName
 |otherwise = False
