module Language.Haskell.Tools.Refactor.Builtin.DefinedInThis where
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.FindUsages(getFile,getLineUsage)
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.PrettyPrint
import OccName
--import Control.Monad.Writer
import Control.Reference
import Outputable
import Data.List
import Data.Aeson
import FastString
import qualified Name as GHC (Name,nameOccName)
import SrcLoc as GHC


defined :: QueryChoice
defined = LocationQuery "DefinedHere" definedHere

definedHere :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
definedHere _ mod mods
  = return $ toJSON $ defMods (mod : mods)

defMods :: [ModuleDom] -> [(String, [(String, String, Int)])]
defMods [] = []
defMods (x:xs) = (getFile (getRange (head qualifiedNames)), uniqueQualifs) : defMods xs
  where
    qualifiedNames :: [QualifiedName]
    qualifiedNames = ((snd x) ^? biplateRef)
    uniqueQualifs = getUnique (defs qualifiedNames) isEqualQualif isLesser 
 
defs :: [QualifiedName] -> [(String, String, Int)]
defs [] = []
defs (x:xs)
 |semanticsDefining x == True = (outPut x) ++ defs xs
 |otherwise = defs xs

getUnique :: [a] -> (a -> a -> Bool) -> (a -> a -> Ordering) -> [a]
getUnique x f g = getHead $ groupBy f $ sortBy g x

getHead :: [[a]] -> [a]
getHead [] = []
getHead ([]:xs) = getHead xs
getHead (x:xs) = (head) x : (getHead xs)

isLesser :: (String, String, b) -> (String, String, b) -> Ordering
isLesser (s1, _, _ ) (s3, _, _) = s1 `compare` s3

isEqualQualif :: (String, String, b) -> (String, String, b) -> Bool
isEqualQualif (s1, _, _) (s3, _, _ ) = s1 == s3

getOccNameString :: GHC.Name -> String
getOccNameString name = showSDocUnsafe $ pprOccName $ GHC.nameOccName name

outPut :: QualifiedName -> [(String, String, Int)]
outPut x
 |name /= [] = [(qualifiedName (head name),getOccNameString (head name), getLineUsage sp)]
 |otherwise = []
 where
  sp = getRange x
  name = getNameToList x

getNameToList :: QualifiedName -> [GHC.Name]
getNameToList t
 |Just m <- semanticsName t = [m]
 |Nothing <- semanticsName t = []
