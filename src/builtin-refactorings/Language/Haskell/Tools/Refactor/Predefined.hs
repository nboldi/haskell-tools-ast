{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Tools.Refactor.Predefined ( builtinRefactorings ) where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Predefined.RenameDefinition
import Language.Haskell.Tools.Refactor.Predefined.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.Predefined.OrganizeImports
import Language.Haskell.Tools.Refactor.Predefined.GenerateExports
import Language.Haskell.Tools.Refactor.Predefined.ExtractBinding
import Language.Haskell.Tools.Refactor.Predefined.IfToGuards
import Language.Haskell.Tools.Refactor.Predefined.InlineBinding
import Language.Haskell.Tools.Refactor.Predefined.FloatOut

builtinRefactorings :: ( DomGenerateExports dom, OrganizeImportsDomain dom
                       , DomainRenameDefinition dom, ExtractBindingDomain dom
                       , GenerateSignatureDomain dom
                       ) => [RefactoringChoice dom]
builtinRefactorings
  = [ organizeImportsRefactoring
    , projectOrganizeImportsRefactoring
    , inlineBindingRefactoring
    , generateTypeSignatureRefactoring
    , renameDefinitionRefactoring
    , generateExportsRefactoring
    , floatOutRefactoring
    , extractBindingRefactoring
    ]
