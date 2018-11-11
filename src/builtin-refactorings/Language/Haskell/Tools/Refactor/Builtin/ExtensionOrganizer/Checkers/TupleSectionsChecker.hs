module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TupleSectionsChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

chkTupleSections :: CheckNode Expr
chkTupleSections = conditional chkTupleSections' TupleSections

chkTupleSections' :: CheckNode Expr
chkTupleSections' e@(TupleSection        _) = addEvidence TupleSections e
chkTupleSections' e@(UnboxedTupleSection _) = addEvidence TupleSections e
                                           >> addEvidence UnboxedTuples e
chkTupleSections' e = return e
