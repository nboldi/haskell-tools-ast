{-# LANGUAGE FlexibleContexts #-}
module Language.Haskell.Tools.Refactor.Builtin ( builtinRefactorings ) where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.RenameDefinition
import Language.Haskell.Tools.Refactor.Builtin.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.Builtin.OrganizeImports
import Language.Haskell.Tools.Refactor.Builtin.GenerateExports
import Language.Haskell.Tools.Refactor.Builtin.ExtractBinding
import Language.Haskell.Tools.Refactor.Builtin.IfToGuards
import Language.Haskell.Tools.Refactor.Builtin.InlineBinding
import Language.Haskell.Tools.Refactor.Builtin.FloatOut

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
