module Language.Haskell.Tools.Refactor.Refactoring where

import Control.Monad.Trans.Except
import Data.List

import SrcLoc
import GHC

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.RefactorBase

data RefactoringChoice dom
  = NamingRefactoring { refactoringName :: String
                      , namingRefactoring :: RealSrcSpan -> String -> Refactoring dom
                      }
  | SelectionRefactoring { refactoringName :: String
                         , selectionRefactoring :: RealSrcSpan -> Refactoring dom
                         }
  | ModuleRefactoring { refactoringName :: String
                      , moduleRefactoring :: Refactoring dom
                      }
  | ProjectRefactoring { refactoringName :: String
                       , projectRefactoring :: ProjectRefactoring dom
                       }

-- | Executes a given command on the selected module and given other modules
performCommand :: [RefactoringChoice dom] -- ^ The set of available refactorings
                    -> [String] -- ^ The refactoring command
                    -> Either FilePath (ModuleDom dom) -- ^ The module in which the refactoring is performed
                    -> [ModuleDom dom] -- ^ Other modules
                    -> Ghc (Either String [RefactorChange dom])
performCommand refactorings (name:args) mod mods =
    case (refactoring, mod, args) of
      (Just (NamingRefactoring _ trf), Right mod, (sp:newName:_))
        -> runExceptT $ trf (correctRefactorSpan (snd mod) $ readSrcSpan sp) newName mod mods
      (Just (NamingRefactoring _ trf), Right _, _)
        -> return $ Left $ "The refactoring '" ++ name
                             ++ "' needs two argument: a source range and a name"
      (Just (SelectionRefactoring _ trf), Right mod, (sp:_))
        -> runExceptT $ trf (correctRefactorSpan (snd mod) $ readSrcSpan sp) mod mods
      (Just (SelectionRefactoring _ trf), Right _, _)
        -> return $ Left $ "The refactoring '" ++ name ++ "' needs one argument: a source range"
      (Just (ModuleRefactoring _ trf), Right mod, _) -> runExceptT $ trf mod mods
      (Just (ProjectRefactoring _ trf), _, _) -> runExceptT $ trf mods
      (Just _, Left modPath, _)
        -> return $ Left $ "The following file is not loaded to Haskell-tools: " ++ modPath
                             ++ ". Please add the containing package."
      (Nothing, _, _) -> return $ Left $ "Unknown command: " ++ name
  where refactoring = find ((== name) . refactoringName) refactorings

refactorCommands :: [RefactoringChoice dom] -> [String]
refactorCommands = map refactoringName
