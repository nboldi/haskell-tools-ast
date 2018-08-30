module Language.Haskell.Tools.Refactor.Builtin.DefinedInThis where
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.FindUsages(getFile,getLineUsage)
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.PrettyPrint
import OccName
import Control.Reference
import Outputable
import Data.List
import Data.Aeson
import FastString
import qualified Name as GHC (Name,nameOccName)
import SrcLoc as GHC

--The result of this is every time when user types DefinedHere command the definedHere function will be invoked
defined :: QueryChoice
defined = LocationQuery "DefinedHere" definedHere

--Returns the result of defMods wrapped to QueryMonad Value
definedHere :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
definedHere _ mod mods
  = return $ toJSON $ defMods (mod : mods)

--Returns list of definitions in the moduls.
--The returned value is [(filename,[(qualifiedName, short name, line of definition)])]
defMods :: [ModuleDom] -> [(String, [(String, String, Int)])]
defMods [] = []
defMods (x:xs) = (getFile (getRange (head qualifiedNames)), uniqueQualifs) : defMods xs
  where
    qualifiedNames :: [QualifiedName]
    qualifiedNames = ((snd x) ^? biplateRef)
    uniqueQualifs = getUnique (defs qualifiedNames) isEqualQualif isLesser 

--Returns the list of definitions to list of QualifiedNames.
--The returned value is [(qualifiedName, short name, line of definition)]
defs :: [QualifiedName] -> [(String, String, Int)]
defs [] = []
defs (x:xs)
 |semanticsDefining x == True = (outPut x) ++ defs xs
 |otherwise = defs xs

--Filters the duplicates by the first parameter. This is needed because in the case of more
--Parameters:
--x:[a]: The list from which we would like to filter the duplicates.
--f: Function which tells when two element with type a is equal
--g: Function which tells that the first parameter is greater, equal, or lesser then the second
getUnique :: [a] -> (a -> a -> Bool) -> (a -> a -> Ordering) -> [a]
getUnique x f g = getHead $ groupBy f $ sortBy g x

--Functions which returns a list from the head of every inside list.
getHead :: [[a]] -> [a]
getHead [] = []
getHead ([]:xs) = getHead xs
getHead (x:xs) = (head) x : (getHead xs)

--Returns an ordering to tuples compared by first element.
isLesser :: (String, String, b) -> (String, String, b) -> Ordering
isLesser (s1, _, _ ) (s3, _, _) = s1 `compare` s3

--Returns true if the first elements from the tuples are equal.
--Otherwise returns false.
isEqualQualif :: (String, String, b) -> (String, String, b) -> Bool
isEqualQualif (s1, _, _) (s3, _, _ ) = s1 == s3

--Returns the short name for the GHC.Name parameter
getOccNameString :: GHC.Name -> String
getOccNameString name = showSDocUnsafe $ pprOccName $ GHC.nameOccName name

--Returns to QualifiedName a qualifiedName in String a short name and the line of the definition.
outPut :: QualifiedName -> [(String, String, Int)]
outPut x
 | name /= [] 
 = [(qualifiedName (head name),getOccNameString (head name), getLineUsage sp)]
 | otherwise = []
 where
  sp = getRange x
  name = getNameToList x

--Returns a GHC.Name to the QualifiedName in list.
getNameToList :: QualifiedName -> [GHC.Name]
getNameToList t
 | Just m <- semanticsName t = [m]
 | Nothing <- semanticsName t = []
