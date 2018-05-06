module Language.Haskell.Tools.Refactor.Builtin.Scope where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint (prettyPrint)

import Control.Monad.Writer
import Control.Reference
import Language.Haskell.Tools.AST
import Data.Aeson
import Language.Haskell.Tools.AST.Ann
import FastString
import qualified Name as GHC (Name,nameOccName)
import Outputable
import Control.Monad.State
import SrcLoc as GHC
import Module as GHC
import Debug.Trace (trace)
import OccName
import Language.Haskell.Tools.AST.SemaInfoTypes
--import Language.Haskell.Tools.Refactor.Builtin.JumpToDefinition

getScope :: QueryChoice
getScope = LocationQuery "GetScope" scope

scope :: RealSrcSpan -> ModuleDom -> [ModuleDom] -> QueryMonad Value
scope sp (_,mod) mods
  = case selectedExpr of (_:_)-> if result == [] then queryError "Empty scope"
                                 else return $ toJSON $ scopeToString $ semanticsScope $ head result
                         _  -> queryError "No name is selected."

  where

    selectedExpr :: [Expr]
    selectedExpr = mod ^? biplateRef
    result = getMin [] (getPos sp) selectedExpr

getMin :: [Expr] -> [Int] -> [Expr] -> [Expr]
getMin [] _ [] = []
getMin [] sp (x:xs) = getMin [x] sp xs
getMin [minExp] _ [] = [minExp]
getMin [minExp] sp (x:xs)
 |getDistance minExp sp > getDistance x sp = getMin [x] sp xs
 |otherwise = getMin [minExp] sp xs

getDistance :: Expr -> [Int] -> Int
getDistance expr list = (exprList !! 0 - list !! 0) ^ 2 + (exprList !! 1- list !! 1) ^ 2
 where
   exprList = getPosition $ getRange expr


getPosition :: SrcSpan -> [Int]
getPosition (RealSrcSpan sp) = [srcSpanStartLine sp, srcSpanEndLine sp]

getPos :: RealSrcSpan -> [Int]
getPos sp = [srcSpanStartLine sp, srcSpanEndLine sp]

--getNodeInfo :: Ann elem dom stage -> NodeInfo (SemanticInfo' dom (SemaInfoClassify elem)) stage
--getNodeInfo (Ann _annotation _element) = _element
{-
getUmodule :: UnnamedModule -> String
getUmodule (Ann umodule b) = getList umodule

getList :: UModule dom stage -> String
getList (UModule _ _ imports _) = getValami imports

getValami :: AnnListG UImportDecl dom stage -> String
getValami (AnnListG _ annListElems) = getString annListElems

--fromNodeInfo :: NodeInfo(SemanticInfo dom UModuleName) (SpanInfo stage) -> UModuleName
--fromNodeInfo (NodeInfo(SemanticInfo dom umod) _) = umod

getString :: [Ann UImportDecl dom stage] -> String
getString [] = ""
getString ((Ann a _):xs) = getImport a ++ getString xs

getUmoduleName :: Ann UModuleName dom stage -> UModuleName dom stage
getUmoduleName (Ann umod _) = umod

getImport :: UImportDecl dom stage -> String
getImport (UImportDecl _ _ _ _ importName _ _) = getN (getUmoduleName importName)

getN :: UModuleName dom stage -> String
getN (UModuleName _moduleNameString) = _moduleNameString
-}
--str :: GHC.Name -> String
--str n = qualifiedName n

--importedModules :: ModuleDom -> [GHC.Module]
--importedModules (_,mod) = semanticsTransMods mod


scopeToString :: Scope -> [[String]]
scopeToString [] = []
scopeToString (x:xs) = inner x ++ scopeToString xs

inner :: [(GHC.Name, Maybe [UsageSpec], Maybe GHC.Name)] -> [[String]]
inner [] = []
inner (x:xs) = (getNameString x : (getOccNameString x : (getUsageSpec (spec x)))) : inner xs
 
getOccNameString :: (GHC.Name, Maybe [UsageSpec], Maybe GHC.Name) -> String
getOccNameString (name, _ , _) = showSDocUnsafe $ pprOccName $ GHC.nameOccName name

getNameString :: (GHC.Name, Maybe [UsageSpec], Maybe GHC.Name) -> String
getNameString (name, _ , _) = qualifiedName name

spec :: (GHC.Name, Maybe [UsageSpec], Maybe GHC.Name) -> Maybe [UsageSpec]
spec (_,sp,_) = sp

getUsageSpec :: Maybe [UsageSpec] -> [String]
getUsageSpec a
 |Just b <- a = getSpec b
 |Nothing<-a = []

getSpec :: [UsageSpec] -> [String]
getSpec [] = []
getSpec ((UsageSpec a _ c):xs) = ([show a,c] ++ getSpec xs)

