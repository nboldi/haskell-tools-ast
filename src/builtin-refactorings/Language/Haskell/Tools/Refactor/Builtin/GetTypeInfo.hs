module Language.Haskell.Tools.Refactor.Builtin.GetTypeInfo where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.JumpToDefinition(getName,isfoundedName)
import Language.Haskell.Tools.Refactor.Builtin.FindUsages(getRealSp,isProperName)
import Language.Haskell.Tools.AST.Ann
import BasicTypes
import Control.Reference
import Data.Aeson
import Outputable as GHC
import SrcLoc as GHC
import Id as GHC
import qualified Name as GHC (Name)

--The result of this is every time when user runs GetType command
--The getType function will be invoked.
getTypeInfo :: QueryChoice
getTypeInfo = LocationQuery "GetType" getType

--Indentifies the QualifiedName in the selected position.
--Then returns the type, the fixity information and the comments from it
--wrapped to QueryMonad.
getType :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad QueryValue
getType sp modul@(_,mod) mods
  = case selectedName of [n] -> do name <- getName n
                                   return $ GeneralQuery $ toJSON $ result n name (modul:mods)
                         []  -> queryError "No name is selected."
                         _   -> queryError "Multiple names are selected."
  where
    selectedName :: [QualifiedName]
    selectedName = mod ^? nodesContaining sp
    
--Returns the sourcerange part for the definition and the fixity informations
--for the GHC.Name parameter.
--Parameters:
--[ModuleDom]: List of moduls in sourcecode.
--GHC.Name: Name from which we would like to get informations
getInfo :: [ModuleDom] -> GHC.Name ->  [[String]]
getInfo [] _ = []
getInfo (x:xs) name
  |result == [] && xs /= [] = getInfo xs name
  |otherwise = result
 where
  result = getFixityAndComments x name

--Returns the sourcerange part for the definition and the fixity informations
--for the GHC.Name parameter if definition is in the actual modul.
--Parameters:
--ModuleDom
--GHC.Name: Name from which we would like to get informations
getFixityAndComments :: ModuleDom -> GHC.Name -> [[String]]
getFixityAndComments mod name
 |result == [] = []
 |otherwise = [result !! 0, list ++ result !! 1]
 where
   qualifiedNames :: [QualifiedName]
   qualifiedNames = (snd mod) ^? biplateRef
   list = getSignature qualifiedNames name mod
   result = findInfoQualifList qualifiedNames name mod

--Returns the sourcerange for the type signature of the choosen name
getSignature :: [QualifiedName] -> GHC.Name -> ModuleDom -> [String]
getSignature [] _ _= []
getSignature (x:xs) name mod
 |isProperName name x == True && typeSignatures /= [] = signatureBindingComm typeSignatures
 |otherwise = getSignature xs name mod
 where
    typeSignatures :: [TypeSignature]
    typeSignatures = (snd mod) ^? nodesContaining (getRealSp (getRange x))

--Returns the type the source code for the definition and the fixity information
result :: QualifiedName -> GHC.Name -> [ModuleDom] -> (String, [[String]])
result n name mods = (typeNameToString n, getInfo mods name)

--Returns the type of the QualifiedName
typeNameToString :: QualifiedName -> String
typeNameToString name = showSDocUnsafe $ ppr $ idType $ semanticsId name

--Returns the fixity information and the source code for definition and type signature
--Parameters:
--[QualifiedName]: One of this could be the definition.
--GHC.Name: which we examine
--ModuleDom: The current modul.
findInfoQualifList :: [QualifiedName] -> GHC.Name -> ModuleDom -> [[String]]
findInfoQualifList [] _ _ = []
findInfoQualifList (x:xs) n mod
 |isfoundedName x n == True = [[getFixityInformation (getFixity x)], (getComm x mod)]
 |otherwise = findInfoQualifList xs n mod


getComm :: QualifiedName -> ModuleDom -> [String]
getComm qualif mod = signatureBindingComm valueBindings
  where
    valueBindings :: [Decl]
    valueBindings = (snd mod) ^? nodesContaining (getRealSp (getRange qualif))

--Returns the list of source codes to the list of AST elements
signatureBindingComm :: [Ann elem dom SrcTemplateStage] -> [String]
signatureBindingComm [] = []
signatureBindingComm [x] = getSrcTemplateInfo (getSourceInfo x)

--Returns the Fixity for QualifiedName
getFixity :: QualifiedName -> [Fixity]
getFixity name
 |Just fix <- semanticsFixity name = [fix]
 |Nothing  <- semanticsFixity name = []

--Returns the Fixity information in String
getFixityInformation :: [Fixity] -> String
getFixityInformation [(Fixity _ precStrongness fixityDirection)] = "precedence: " ++ precedence ++ " " ++ "Direction: " ++ fixDir
  where
    precedence = show precStrongness
    fixDir = getFixityDirection fixityDirection
getFixityInformation [] = ""

--Returns the FixityDirection in String
getFixityDirection :: FixityDirection -> String
getFixityDirection BasicTypes.InfixL = "Left"
getFixityDirection BasicTypes.InfixR = "Right"
getFixityDirection BasicTypes.InfixN = "None"

--Returns the NodeInfo part for an AST element
getNodeInfo :: Ann elem dom SrcTemplateStage 
            -> NodeInfo (SemanticInfo' dom (SemaInfoClassify elem)) (SpanInfo SrcTemplateStage)
getNodeInfo (Ann _annotation _element) = _annotation

--Returns the list of SourceTemplateElem for AST element
--From this the source code for the AST element can be retrieved
getSourceInfo :: Ann elem dom SrcTemplateStage -> [SourceTemplateElem]
getSourceInfo name = ((getNodeInfo name) ^. sourceInfo) ^. sourceTemplateNodeElems

--Returns the list of source code for list of SourceTemplateElem
getSrcTemplateInfo :: [SourceTemplateElem] -> [String]
getSrcTemplateInfo [] = []
getSrcTemplateInfo (x:xs) = (getStringTemplateElem x : (getSrcTemplateInfo xs))

--Returns the list of source code for SourceTemplateElem
getStringTemplateElem :: SourceTemplateElem -> String
getStringTemplateElem (TextElem a _) = srcTextElem a
getStringTemplateElem ChildElem = ""

--Returns the list of source code for list of SourceTemplateTextElem
srcTextElem :: [SourceTemplateTextElem] -> String
srcTextElem [] = ""
srcTextElem (x:xs) = (srcTextToString x) ++ srcTextElem xs

--Returns the list of source code for SourceTemplateTextElem
srcTextToString :: SourceTemplateTextElem -> String
srcTextToString (NormalText _sourceTemplateText) = _sourceTemplateText
srcTextToString (StayingText _sourceTemplateText _lineEndings) = _sourceTemplateText ++ _lineEndings
