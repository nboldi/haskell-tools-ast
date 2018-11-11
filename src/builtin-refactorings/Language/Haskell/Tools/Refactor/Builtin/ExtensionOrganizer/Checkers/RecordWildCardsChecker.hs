module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.RecordWildCardsChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any contraint.

chkRecordWildCardsPatField :: CheckNode PatternField
chkRecordWildCardsPatField p@(FieldWildcardPattern {}) = addEvidence RecordWildCards p
chkRecordWildCardsPatField p = return p

chkRecordWildCardsFieldUpdate :: CheckNode FieldUpdate
chkRecordWildCardsFieldUpdate e@(FieldWildcard {}) = addEvidence RecordWildCards e
chkRecordWildCardsFieldUpdate e = return e
