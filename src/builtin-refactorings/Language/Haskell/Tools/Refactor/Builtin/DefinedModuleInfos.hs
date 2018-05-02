module Language.Haskell.Tools.Refactor.Builtin.DefinedModuleInfos where
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.DefinedInThis(getNameToList,getOccNameString,isLesser,isEqualQualif,getUnique)
import Language.Haskell.Tools.Refactor.Builtin.FindUsages
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.PrettyPrint

--import Language.Haskell.Tools.Refactor.Builtin.DefinedInThis

import Control.Monad.Writer
import Control.Reference
import Data.Aeson
import FastString
import qualified Name as GHC (Name)
import Control.Monad.State
import SrcLoc as GHC
import Debug.Trace (trace)


definedModule :: QueryChoice
definedModule = LocationQuery "DefinedInfo" definedInfo

definedInfo :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
definedInfo _ mod _
  = return $ toJSON $ defMods mod

defMods :: ModuleDom -> [(String, String, [Int])]
defMods x = getUnique (nameAndPlace qualifiedNames) isEqualQualif isLesser
  where
    qualifiedNames :: [QualifiedName]
    qualifiedNames = ((snd x) ^? biplateRef)


nameAndPlace :: [QualifiedName] -> [(String, String, [Int])]
nameAndPlace [] = []
nameAndPlace (x:xs) = (outPutInfo x) ++ nameAndPlace xs

{-
getUnique :: [(String, String, [Int])] -> [(String, String, [Int])]
getUnique x = getHead $ groupBy isEqualQualif $ sortBy isLesser x
-}
getPlaceInCode :: SrcSpan -> [Int]
getPlaceInCode (RealSrcSpan sp) = [srcSpanStartLine sp, srcSpanStartCol sp, srcSpanEndLine sp, srcSpanEndCol sp]

outPutInfo :: QualifiedName -> [(String, String, [Int])]
outPutInfo x
 |name /= [] = [(qualifiedName (head name), getOccNameString (head name), getPlaceInCode sp)]
 |otherwise = []
 where
  sp = getRange x
  name = getNameToList x

