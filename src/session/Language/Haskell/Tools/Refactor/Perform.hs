{-# LANGUAGE FlexibleContexts #-}
-- | Defines common utilities for using refactorings. Provides an interface for both demo, command line and integrated tools.
module Language.Haskell.Tools.Refactor.Perform where

import Data.List.Split
import Data.List
import Control.Monad.Trans.Except

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.Refactoring
import Language.Haskell.Tools.Refactor.RefactorBase

import Language.Haskell.Tools.Refactor.Predefined.ExtractBinding
import Language.Haskell.Tools.Refactor.Predefined.FloatOut
import Language.Haskell.Tools.Refactor.Predefined.GenerateExports
import Language.Haskell.Tools.Refactor.Predefined.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.Predefined.InlineBinding
import Language.Haskell.Tools.Refactor.Predefined.OrganizeImports
import Language.Haskell.Tools.Refactor.Predefined.RenameDefinition

import GHC

-- | Executes a given command on the selected module and given other modules
performCommand :: (HasModuleInfo dom, DomGenerateExports dom, OrganizeImportsDomain dom, DomainRenameDefinition dom, ExtractBindingDomain dom, GenerateSignatureDomain dom)
               => [RefactoringChoice dom] -- ^ The set of available refactorings
                    -> [String] -- ^ The refactoring command
                    -> Either FilePath (ModuleDom dom) -- ^ The module in which the refactoring is performed
                    -> [ModuleDom dom] -- ^ Other modules
                    -> Ghc (Either String [RefactorChange dom])
performCommand refactorings (name:args) mod mods =
    case (refactoring, mod, args) of
      (Just (NamingRefactoring _ trf), Right mod, (sp:newName:_))
        -> runExceptT $ trf (correctRefactorSpan (snd mod) $ readSrcSpan sp) newName mod mods
      (Just (NamingRefactoring _ trf), Right _, _) -> return $ Left $ "The refactoring '" ++ name ++ "' needs two argument: a source range and a name"
      (Just (SelectionRefactoring _ trf), Right mod, (sp:_)) -> runExceptT $ trf (correctRefactorSpan (snd mod) $ readSrcSpan sp) mod mods
      (Just (SelectionRefactoring _ trf), Right _, _) -> return $ Left $ "The refactoring '" ++ name ++ "' needs one argument: a source range"
      (Just (ModuleRefactoring _ trf), Right mod, _) -> runExceptT $ trf mod mods
      (Just (ProjectRefactoring _ trf), _, _) -> runExceptT $ trf mods
      (Just _, Left modPath, _) -> return $ Left $ "The following file is not loaded to Haskell-tools: "
                                                      ++ modPath ++ ". Please add the containing package."
      (Nothing, _, _) -> return $ Left $ "Unknown command: " ++ name
  where refactoring = find ((== name) . refactoringName) refactorings

refactorCommands :: [RefactoringChoice dom] -> [String]
refactorCommands = map refactoringName
