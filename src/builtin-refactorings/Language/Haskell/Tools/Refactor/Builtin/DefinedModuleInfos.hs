module Language.Haskell.Tools.Refactor.Builtin.DefinedModuleInfos where
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.DefinedInThis(getNameToList,getOccNameString,isLesser,isEqualQualif,getUnique)

import Control.Reference
import Data.Aeson
import SrcLoc as GHC

--Result of this function is that every time when user runs DefinedInfo command
--the definedInfo function will be invoked.

definedModule :: QueryChoice
definedModule = LocationQuery "DefinedInfo" definedInfo

--Returns the result of defMods wrapped by JSON and QueryMonad.

definedInfo :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad QueryValue
definedInfo _ mod _
  = return $ GeneralQuery $ toJSON $ defMods mod

--Returns the appearances of unique GHC.Names
--Paramteres:
--x:ModuleDom: The modul from which we would like to get appearances of Names.
--Result:[(qualifiedName, short name, position)]
--where position [start line,start column, end line, end column]

defMods :: ModuleDom -> [(String, String, [Int])]
defMods x = getUnique (nameAndPlace qualifiedNames) isEqualQualif isLesser
  where
    qualifiedNames :: [QualifiedName]
    qualifiedNames = ((snd x) ^? biplateRef)

--Returns the appearances of GHC.Names
--Paramteres:
--[QualifiedName]: The of QualifiedName from which we get the information
--Result:[(qualifiedName, short name, position)]
--where position [start line,start column, end line, end column]

nameAndPlace :: [QualifiedName] -> [(String, String, [Int])]
nameAndPlace [] = []
nameAndPlace (x:xs) = (outPutInfo x) ++ nameAndPlace xs

--Returns the position to SrcSpan if it is RealSrcSpan
--position: [start line,start column, end line, end column]

getPlaceInCode :: SrcSpan -> [Int]
getPlaceInCode (RealSrcSpan sp) = [srcSpanStartLine sp, srcSpanStartCol sp, srcSpanEndLine sp, srcSpanEndCol sp]

--If GHC.Name belongs to the QualifiedName this function returns its string representation
--Otherwise returns an empty list

outPutInfo :: QualifiedName -> [(String, String, [Int])]
outPutInfo x
 |name /= [] = [(qualifiedName (head name), getOccNameString (head name), getPlaceInCode sp)]
 |otherwise = []
 where
  sp = getRange x
  name = getNameToList x
