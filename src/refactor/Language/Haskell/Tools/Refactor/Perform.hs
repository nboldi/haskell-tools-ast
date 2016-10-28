{-# LANGUAGE StandaloneDeriving
           , DeriveGeneric
           , LambdaCase
           , ScopedTypeVariables
           , BangPatterns
           , MultiWayIf
           , FlexibleContexts
           , TypeFamilies
           , TupleSections
           , TemplateHaskell
           , ViewPatterns
           #-}
-- | Defines common utilities for using refactorings. Provides an interface for both demo, command line and integrated tools.
module Language.Haskell.Tools.Refactor.Perform where

import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AnnTrf.RangeToRangeTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplateToSourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.RangeTemplate
import Language.Haskell.Tools.AnnTrf.PlaceComments
import Language.Haskell.Tools.PrettyPrint.RoseTree
import Language.Haskell.Tools.PrettyPrint
 
import Data.List
import Data.List.Split
import GHC.Generics hiding (moduleName)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.IORef
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Reference
import Control.Exception
import System.Directory
import System.IO
import System.FilePath
import Data.Generics.Uniplate.Operations

import Language.Haskell.Tools.Refactor.Predefined.OrganizeImports
import Language.Haskell.Tools.Refactor.Predefined.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.Predefined.GenerateExports
import Language.Haskell.Tools.Refactor.Predefined.RenameDefinition
import Language.Haskell.Tools.Refactor.Predefined.ExtractBinding
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Prepare

import Language.Haskell.TH.LanguageExtensions
import GHC
import SrcLoc
          

import Debug.Trace


-- | Executes a given command on the selected module and given other modules
performCommand :: (HasModuleInfo dom, DomGenerateExports dom, OrganizeImportsDomain dom, DomainRenameDefinition dom, ExtractBindingDomain dom, GenerateSignatureDomain dom) 
               => RefactorCommand -> ModuleDom dom -- ^ The module in which the refactoring is performed
                                  -> [ModuleDom dom] -- ^ Other modules
                                  -> Ghc (Either String [RefactorChange dom])
performCommand rf mod mods = runRefactor mod mods $ selectCommand rf
  where selectCommand NoRefactor = localRefactoring return
        selectCommand OrganizeImports = localRefactoring organizeImports
        selectCommand GenerateExports = localRefactoring generateExports 
        selectCommand (GenerateSignature sp) = localRefactoring $ generateTypeSignature' (correctSp mod sp)
        selectCommand (RenameDefinition sp str) = renameDefinition' (correctSp mod sp) str
        selectCommand (ExtractBinding sp str) = localRefactoring $ extractBinding' (correctSp mod sp) str

        correctSp mod sp = mkRealSrcSpan (updateSrcFile fileName $ realSrcSpanStart sp) 
                                         (updateSrcFile fileName $ realSrcSpanEnd sp)
        fileName = case srcSpanStart $ getRange (snd mod) of RealSrcLoc loc -> srcLocFile loc 
        updateSrcFile fn loc = mkRealSrcLoc fn (srcLocLine loc) (srcLocCol loc) 

-- | A refactoring command
data RefactorCommand = NoRefactor 
                     | OrganizeImports
                     | GenerateExports
                     | GenerateSignature RealSrcSpan
                     | RenameDefinition RealSrcSpan String
                     | ExtractBinding RealSrcSpan String
    deriving Show

readCommand :: String -> RefactorCommand
readCommand (splitOn " " -> refact:args) = analyzeCommand refact args

analyzeCommand :: String -> [String] -> RefactorCommand
analyzeCommand "" _ = NoRefactor
analyzeCommand "CheckSource" _ = NoRefactor
analyzeCommand "OrganizeImports" _ = OrganizeImports
analyzeCommand "GenerateExports" _ = GenerateExports
analyzeCommand "GenerateSignature" [sp] = GenerateSignature (readSrcSpan sp)
analyzeCommand "RenameDefinition" [sp, newName] = RenameDefinition (readSrcSpan sp) newName
analyzeCommand "ExtractBinding" [sp, newName] = ExtractBinding (readSrcSpan sp) newName
analyzeCommand ref _ = error $ "Unknown command: " ++ ref

