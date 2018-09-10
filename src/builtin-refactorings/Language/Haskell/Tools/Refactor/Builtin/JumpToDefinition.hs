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

--Result of this is that if user starts JumpToDefinition command
--the getDefinition function will be invoked
getDefinitionQuery :: QueryChoice
getDefinitionQuery = LocationQuery "JumpToDefinition" getDefinition

--Finds the QualifiedName in the given sourcerange
--Collect the QualifiedName in moduls.
--Returns the filename and the line in which the definition of the QualifiedName in the given sourcerange.
--The returning is wrapped to JSON Value
getDefinition :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad QueryValue
getDefinition sp (_,mod) mods
  = case selectedName of [n] -> do ctors <- getName n
                                   source <- evalStateT (findName qualifiedNames ctors) n
                                   fileName <- getFileName source
                                   line <- getLineDefinition source
                                   return $ GeneralQuery $ toJSON (fileName, line)
                         []  -> queryError "No name is selected."
                         _   -> queryError "Multiple names are selected."
  where
    selectedName :: [QualifiedName]
    selectedName = mod ^? nodesContaining sp
    qualifiedNames :: [QualifiedName]
    qualifiedNames = (mod ^? biplateRef) ++ (getQualifiedNames mods)

-- Returns the list of QualifiedNames which can be found in moduls
--Parameter: the list of moduls in which we would like to find QualifiedNames
getQualifiedNames :: [ModuleDom] -> [QualifiedName]
getQualifiedNames [] = []
getQualifiedNames (mod:xs) = qualifiedNames ++ (getQualifiedNames xs)
 where
  qualifiedNames :: [QualifiedName]
  qualifiedNames = (snd mod) ^? biplateRef

--Returns the range of the definition if it can be found in sourcecode or queryerror
findName :: [QualifiedName] -> GHC.Name -> SrcMonad
findName [] _ = lift $ queryError "Definition cannot be found"
findName (x:xs) n
 | isfoundedName x n == True = return (getRange x)
 | otherwise = findName xs n

--Returns true if the queried GHC.Name of QualifiedName is equal to the GHC.Name parameter 
--and the isDefine flag is true
--Parameters: firstName: the QualifiedName we examine
--ghcName: the GHC.Name of the QualifiedName which range is in the selected position
isfoundedName :: QualifiedName -> GHC.Name -> Bool
isfoundedName firstName ghcName
 | (isEqualGhcName firstName  ghcName) && semanticsDefining firstName == True = True
 | otherwise = False

--Returns the queried GHC.Name for QualifiedName
--Parameter: the QualifiedName from which we would like to query
getName :: QualifiedName -> QueryMonad GHC.Name
getName t
 | Just m <- semanticsName t = return m
 | Nothing <- semanticsName t = queryError (noSuccessMsg t)

--Returns the filename in QueryMonad in which the source can be found
getFileName :: SrcSpan -> QueryMonad String
getFileName srcSp
 |Just fileName <- srcSpanFileName_maybe srcSp = return (unpackFS fileName)
 |Nothing <- srcSpanFileName_maybe srcSp = queryError "Problem with getFileName method"

--Returns the line in QueryMonad in which the SrcSpan parameter can be found or queryError.
--The counting of lines starts from 1.
getLineDefinition :: SrcSpan -> QueryMonad Int
getLineDefinition (RealSrcSpan sp) = return (srcSpanStartLine sp)
getLineDefinition _ = queryError "Problem with getLineDefinition method"

--Returns true if the GHC.Name which queried to QualifiedName parameter
--is equal to GHC.Name parameter otherwise returns false.
isEqualGhcName :: QualifiedName -> GHC.Name -> Bool
isEqualGhcName qualif ghcName
 | Just name <- semanticsName qualif = name == ghcName
 | otherwise = False

--Returns an error message
noSuccessMsg :: QualifiedName -> String
noSuccessMsg t = "Cannot find name" ++ (prettyPrint t)
