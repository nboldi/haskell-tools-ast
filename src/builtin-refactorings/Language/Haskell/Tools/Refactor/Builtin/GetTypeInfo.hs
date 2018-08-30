module Language.Haskell.Tools.Refactor.Builtin.GetTypeInfo where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST.SemaInfoTypes
import Language.Haskell.Tools.Refactor.Builtin.JumpToDefinition(getName,isfoundedName)
import Language.Haskell.Tools.Refactor.Builtin.FindUsages(getRealSp,isProperName)
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Match.Decls
import Language.Haskell.Tools.AST.Ann
import BasicTypes
import Control.Monad.Writer
import Control.Reference
import Data.Aeson
import Debug.Trace (trace)
import Outputable as GHC
import SrcLoc as GHC
import Id as GHC
import Type as GHC
import qualified Name as GHC (Name)

getTypeInfo :: QueryChoice
getTypeInfo = LocationQuery "GetType" getType

getType :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
getType sp modul@(_,mod) mods
  = case selectedName of [n] -> do name <- getName n
                                   return $ toJSON $ result n name (modul:mods)
                         []  -> queryError "No name is selected."
                         _   -> queryError "Multiple names are selected."
  where
    selectedName :: [QualifiedName]
    selectedName = mod ^? nodesContaining sp
    
getInfo :: [ModuleDom] -> GHC.Name ->  [[String]]
getInfo [] _ = []
getInfo (x:xs) name
  |result == [] && xs /= [] = (getInfo xs name)
  |otherwise = result
 where
  result = getFixityAndComments x name

getFixityAndComments :: ModuleDom -> GHC.Name -> [[String]]
getFixityAndComments mod name
 |result == [] = []
 |otherwise = [result !! 0, list ++ result !! 1]
 where
   qualifiedNames :: [QualifiedName]
   qualifiedNames = (snd mod) ^? biplateRef
   list = getSignature qualifiedNames name mod
   result = findInfoQualifList qualifiedNames name mod

getSignature :: [QualifiedName] -> GHC.Name -> ModuleDom -> [String]
getSignature [] _ _= []
getSignature (x:xs) name mod
 |isProperName name x == True && typeSignatures /= [] = signatureBindingComm typeSignatures
 |otherwise = getSignature xs name mod
 where
    typeSignatures :: [TypeSignature]
    typeSignatures = (snd mod) ^? nodesContaining (getRealSp (getRange x))

--result n name qualifiedNames signatures = trace "empty" $ []
result n name mods = (typeNameToString n, getInfo mods name)

typeNameToString :: QualifiedName -> String
typeNameToString name = showSDocUnsafe $ ppr $ idType $ semanticsId name

findInfoQualifList :: [QualifiedName] -> GHC.Name -> ModuleDom -> [[String]]
findInfoQualifList [] _ _ = []
findInfoQualifList (x:xs) n mod
 |isfoundedName x n == True = [[getFixityInformation (getFixity x)], (getComm x mod)]
 |otherwise = findInfoQualifList xs n mod

extendInfo :: [[String]] -> [String] -> [String] -> [[String]]
extendInfo [] d c = [d,c]
extendInfo [[],b] d c =  trace (concat (d ++ c)) $ [d, b ++ c]
extendInfo [a,b] _ c = trace (concat (c)) $ [a, b ++ c]

getComm :: QualifiedName -> ModuleDom -> [String]
getComm qualif mod = signatureBindingComm valueBindings
  where
    
    valueBindings :: [Decl]
    valueBindings = (snd mod) ^? nodesContaining (getRealSp (getRange qualif))


signatureBindingComm [] = []
signatureBindingComm [x] = getSrcTemplateInfo (getSourceInfo x)

getFixity :: QualifiedName -> [Fixity]
getFixity name
 |Just fix <- semanticsFixity name = [fix]
 |Nothing  <- semanticsFixity name = []

getFixityInformation :: [Fixity] -> String
getFixityInformation [(Fixity text precStrongness fixityDirection)] = "precedence: " ++ precedence ++ " " ++ "Direction: " ++ fixDir
  where
    precedence = show precStrongness
    fixDir = getFixityDirection fixityDirection
getFixityInformation [] = ""

getFixityDirection :: FixityDirection -> String
getFixityDirection BasicTypes.InfixL = "Left"
getFixityDirection BasicTypes.InfixR = "Right"
getFixityDirection BasicTypes.InfixN = "None"

getNodeInfo :: Ann elem dom SrcTemplateStage -> NodeInfo (SemanticInfo' dom (SemaInfoClassify elem)) (SpanInfo SrcTemplateStage)
getNodeInfo (Ann _annotation _element) = _annotation

getSourceInfo :: Ann elem dom SrcTemplateStage -> [SourceTemplateElem]
getSourceInfo name = ((getNodeInfo name) ^. sourceInfo) ^. sourceTemplateNodeElems

getSrcTemplateInfo :: [SourceTemplateElem] -> [String]
getSrcTemplateInfo [] = []
getSrcTemplateInfo (x:xs) = (getStringTemplateElem x : (getSrcTemplateInfo xs))

getStringTemplateElem :: SourceTemplateElem -> String
getStringTemplateElem (TextElem a _) = srcTextElem a
getStringTemplateElem ChildElem = ""


srcTextElem :: [SourceTemplateTextElem] -> String
srcTextElem [] = ""
srcTextElem (x:xs) = (srcTextToString x) ++ srcTextElem xs

srcTextToString :: SourceTemplateTextElem -> String
srcTextToString (NormalText _sourceTemplateText) = _sourceTemplateText
srcTextToString (StayingText _sourceTemplateText _lineEndings) = _sourceTemplateText ++ _lineEndings

