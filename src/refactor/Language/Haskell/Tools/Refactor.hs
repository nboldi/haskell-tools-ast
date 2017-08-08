-- | Defines the API for refactorings
module Language.Haskell.Tools.Refactor
    ( module Language.Haskell.Tools.AST.SemaInfoClasses
    , module Language.Haskell.Tools.Rewrite
    , module Language.Haskell.Tools.AST.References
    , module Language.Haskell.Tools.AST.Helpers
    , module Language.Haskell.Tools.Refactor.MonadicOperations
  , module Language.Haskell.Tools.Refactor.Helpers
    , module Language.Haskell.Tools.Rewrite.ElementTypes
    , module Language.Haskell.Tools.Refactor.Prepare
    , module Language.Haskell.Tools.Refactor.ListOperations
    , module Language.Haskell.Tools.Refactor.BindingElem
    , module Language.Haskell.Tools.Refactor.IndentationUtils
    , module Language.Haskell.Tools.Refactor.Refactoring
    , module Language.Haskell.Tools.Refactor.NameUtils
    , module Language.Haskell.Tools.Refactor.Representation
    , module Language.Haskell.Tools.Refactor.Monad
  , Ann, HasSourceInfo(..), HasRange(..), annListElems, annListAnnot, annList, annJust, annMaybe, isAnnNothing, Domain, Dom, IdDom
    , shortShowSpan, SrcTemplateStage, SourceInfoTraversal(..)
    -- elements of source templates
    , sourceTemplateNodeRange, sourceTemplateNodeElems
    , sourceTemplateListRange, srcTmpListBefore, srcTmpListAfter, srcTmpDefaultSeparator, srcTmpIndented, srcTmpSeparators
    , sourceTemplateOptRange, srcTmpOptBefore, srcTmpOptAfter
    , SourceTemplateTextElem(..), sourceTemplateText
    ) where

-- Important: Haddock doesn't support the rename all exported modules and export them at once hack

import Language.Haskell.Tools.Rewrite.ElementTypes
import Language.Haskell.Tools.AST.Helpers
import Language.Haskell.Tools.AST.References
import Language.Haskell.Tools.Rewrite
import Language.Haskell.Tools.AST.SemaInfoClasses
import Language.Haskell.Tools.Refactor.IndentationUtils
import Language.Haskell.Tools.Refactor.NameUtils
import Language.Haskell.Tools.Refactor.BindingElem
import Language.Haskell.Tools.Refactor.Helpers
import Language.Haskell.Tools.Refactor.ListOperations
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.MonadicOperations
import Language.Haskell.Tools.Refactor.Refactoring
import Language.Haskell.Tools.Refactor.Representation
import Language.Haskell.Tools.Refactor.Monad
import Language.Haskell.Tools.PrettyPrint.Prepare

import Language.Haskell.Tools.AST.Ann
