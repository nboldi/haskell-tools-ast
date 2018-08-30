module Language.Haskell.Tools.Refactor.Builtin.FindUsages where
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.JumpToDefinition(getName)

import Control.Reference
import Data.Aeson
import FastString
import qualified Name as GHC (Name)
import SrcLoc as GHC
import Data.List

--The result of this is if the user runs the GetUsages command
-- the getUsages function will be invoked.
getUsagesQuery :: QueryChoice
getUsagesQuery = LocationQuery "GetUsages" getUsages

--Finds the QualifiedName in the selected range.
--Collect the QualifiedNames in moduls and separate them by moduls.
--Returns the position of usages of the QualifiedName in QueryMonad JSON Value.
getUsages :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
getUsages sp modul@(_,mod) mods
  = case selectedName of [n] -> do ctors <- getName n
                                   if separateMods qualifiedNames ctors == []
                                        then queryError "No usages found"
                                        else return $ toJSON $ nub $ map wrapOutSource (separateMods qualifiedNames ctors)
                         []  -> queryError "No name is selected."
                         _   -> queryError "Multiple names are selected."
  where
    selectedName :: [QualifiedName]
    selectedName = mod ^? nodesContaining sp
    qualifiedNames :: [(ModuleDom, [QualifiedName])]
    qualifiedNames = [(modul,qualif)] ++ (getQualifAndMods mods)
    qualif :: [QualifiedName]
    qualif = (mod ^? biplateRef)

--Returns list of tuples which first element is modul, second element is the list of QualifiedName in the modul.
--Parameter list of ModulDoms from which we would like to get the appeared QualifiedNames.
getQualifAndMods :: [ModuleDom] -> [(ModuleDom, [QualifiedName])]
getQualifAndMods [] = []
getQualifAndMods (mod:xs) = qualifiedNames ++ (getQualifAndMods xs)
 where
  qualifiedNames :: [(ModuleDom, [QualifiedName])]
  qualifiedNames = [(mod,(unnamed ^? biplateRef))]
  unnamed = snd mod

--Returns tuple which contains the filename and line to the given sourcerange.
wrapOutSource :: SrcSpan -> (String, Int)
wrapOutSource sp = (getFile sp, getLineUsage sp)

getRealSp :: SrcSpan -> RealSrcSpan
getRealSp (RealSrcSpan sp) = sp

--Returns filename to the given sourcerange.
getFile :: SrcSpan -> String
getFile srcSp
 |Just fileName <- srcSpanFileName_maybe srcSp = unpackFS fileName

--Returns the line to the given sourcerange
getLineUsage :: SrcSpan -> Int
getLineUsage (RealSrcSpan sp) = srcSpanStartLine sp

--Filters the usage places in moduls and returns the sourceranges of them.
--This is not contains the place of the definition and the appearance in function or fixity signature.
separateMods :: [(ModuleDom, [QualifiedName])] -> GHC.Name -> [SrcSpan]
separateMods [] _ = []
separateMods (x:xs) name = (findNames (fst x) (snd x) name) ++ (separateMods xs name)

--Returns the usages of the GHC.Name parameter in a modul.
--Parameters:
--mod:ModuleDom: is module in which the QualifiedName are.
--[QualifiedName]: QualifiedNames in the modul.
--name:GHC.Name: is name of which we would like to find usages.
findNames :: ModuleDom -> [QualifiedName] -> GHC.Name -> [SrcSpan]
findNames _ [] _ = []
findNames mod (x:xs) name
 |isProperName name x && semanticsDefining x == False && isNotTypSig mod x = (getRange x) : (findNames mod xs name)
 |otherwise = findNames mod xs name

--Returns true if the QualifiedName is not part of TypeSignature and not part of FixitySignature
--Parameters:
--mod:ModuleDom: The modul in which the QualifiedName can be found.
--x:QualifiedName from which we would like to decide.
isNotTypSig :: ModuleDom -> QualifiedName -> Bool
isNotTypSig mod x
 |typeSignatures == [] && decl == [] = True
 |otherwise = False
  where
    typeSignatures :: [TypeSignature]
    typeSignatures = (snd mod) ^? nodesContaining (getRealSp (getRange x))
    decl :: [FixitySignature]
    decl = (snd mod) ^? nodesContaining (getRealSp (getRange x))

--Returns true if the ghcName parameter and the queried GHC.Name from QualifiedName is equal.
--Otherwise returns false.
isProperName ::  GHC.Name -> QualifiedName -> Bool
isProperName ghcName qualif
 |Just name <- semanticsName qualif = name == ghcName
 |otherwise = False
