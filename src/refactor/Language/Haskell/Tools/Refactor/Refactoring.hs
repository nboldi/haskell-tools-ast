module Language.Haskell.Tools.Refactor.Refactoring where

import SrcLoc

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
