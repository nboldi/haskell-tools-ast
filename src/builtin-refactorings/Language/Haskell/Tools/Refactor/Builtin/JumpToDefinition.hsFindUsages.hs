module Language.Haskell.Tools.Refactor.Builtin.FindUsages where
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.JumpToDefinition(getName)

import Control.Reference
import Data.Aeson
import FastString
import qualified Name as GHC (Name)
import SrcLoc as GHC
import Data.List


getUsagesQuery :: QueryChoice
getUsagesQuery = LocationQuery "GetUsages" getUsages

getUsages :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
getUsages sp modul@(_,mod) mods
  = case selectedName of [n] -> do ctors <- getName n
                                   if separateMods qualifiedNames ctors == []
                                        then queryError "No found usages"
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

getQualifAndMods :: [ModuleDom] -> [(ModuleDom, [QualifiedName])]
getQualifAndMods [] = []
getQualifAndMods (mod:xs) = qualifiedNames ++ (getQualifAndMods xs)
 where
  qualifiedNames :: [(ModuleDom, [QualifiedName])]
  qualifiedNames = [(mod,(unnamed ^? biplateRef))]
  unnamed = snd mod

wrapOutSource :: SrcSpan -> (String, Int)
wrapOutSource sp = (getFile sp, getLineUsage sp)

getRealSp :: SrcSpan -> RealSrcSpan
getRealSp (RealSrcSpan sp) = sp

getFile :: SrcSpan -> String
getFile srcSp
 |Just fileName <- srcSpanFileName_maybe srcSp = unpackFS fileName

getLineUsage :: SrcSpan -> Int
getLineUsage (RealSrcSpan sp) = srcSpanStartLine sp

separateMods :: [(ModuleDom, [QualifiedName])] -> GHC.Name -> [SrcSpan]
separateMods [] _ = []
separateMods (x:xs) name = (findNames (fst x) (snd x) name) ++ (separateMods xs name)

findNames :: ModuleDom -> [QualifiedName] -> GHC.Name -> [SrcSpan]
findNames _ [] _ = []
findNames mod (x:xs) name
 |isProperName name x && semanticsDefining x == False && isNotTypSig mod x = (getRange x) : (findNames mod xs name)
 |otherwise = findNames mod xs name


isNotTypSig :: ModuleDom -> QualifiedName -> Bool
isNotTypSig mod x
 |typeSignatures == [] && decl == [] = True
 |otherwise = False
  where
    typeSignatures :: [TypeSignature]
    typeSignatures = (snd mod) ^? nodesContaining (getRealSp (getRange x))
    decl :: [FixitySignature]
    decl = (snd mod) ^? nodesContaining (getRealSp (getRange x))

isProperName ::  GHC.Name -> QualifiedName -> Bool
isProperName ghcName qualif
 |Just name <- semanticsName qualif = name == ghcName
 |otherwise = False
