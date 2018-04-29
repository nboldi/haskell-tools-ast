module Language.Haskell.Tools.Refactor.Builtin.JumpToDefinition where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint (prettyPrint)

import Control.Monad.Writer
import Control.Reference
import Data.Aeson
import FastString
import qualified Name as GHC (Name)
import Control.Monad.State
import SrcLoc as GHC
import Debug.Trace (trace)

type SrcMonad = StateT QualifiedName QueryMonad SrcSpan

getDefinitionQuery :: QueryChoice
getDefinitionQuery = LocationQuery "JumpToDefinition" getMatches

getMatches :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
getMatches sp (_,mod) mods
  = case selectedName of [n] -> do ctors <- getName n
                                   source <- evalStateT (findName qualifiedNames ctors) n
                                   fileName <- getFileName source
                                   line <- getLineDefinition source
                                   return $ toJSON (fileName, line)
                         []  -> queryError "No name is selected."
                         _   -> queryError "Multiple names are selected."
  where

    selectedName :: [QualifiedName]
    selectedName = mod ^? nodesContaining sp

    qualifiedNames :: [QualifiedName]
    qualifiedNames = (mod ^? biplateRef) ++ (getQualifiedNames mods)

getQualifiedNames :: [ModuleDom] -> [QualifiedName]
getQualifiedNames [] = []
getQualifiedNames (mod:xs) = qualifiedNames ++ (getQualifiedNames xs)
 where
  qualifiedNames :: [QualifiedName]
  qualifiedNames = (snd mod) ^? biplateRef

findName :: [QualifiedName] -> GHC.Name -> SrcMonad
findName [] _ = lift $ queryError "Definition didn't founded"
findName (x:xs) n
 |isfoundedName x n == True = return (getRange x)
 |otherwise = findName xs n


isfoundedName :: QualifiedName -> GHC.Name -> Bool
isfoundedName firstName qMonad
 |(isEqualGhcName firstName  qMonad) && semanticsDefining firstName == True = True
 |otherwise = False

getName :: QualifiedName -> QueryMonad GHC.Name
getName t
 |Just m <- semanticsName t = return m
 |Nothing <- semanticsName t = queryError (noSuccessMsg t)

getFileName :: SrcSpan -> QueryMonad String
getFileName srcSp
 |Just fileName <- srcSpanFileName_maybe srcSp = return (unpackFS fileName)
 |Nothing <- srcSpanFileName_maybe srcSp = queryError "Problem with getFileName method"

getLineDefinition :: SrcSpan -> QueryMonad Int
getLineDefinition (RealSrcSpan sp) = return (srcSpanStartLine sp)
getLineDefinition _ = queryError "Problem with getLineDefinition method"

isEqualGhcName :: QualifiedName -> GHC.Name -> Bool
isEqualGhcName qualif ghcName
 |Just name <- semanticsName qualif = name == ghcName
 |otherwise = False

noSuccessMsg :: QualifiedName -> String
noSuccessMsg t = "Cannot find name" ++ (prettyPrint t)
