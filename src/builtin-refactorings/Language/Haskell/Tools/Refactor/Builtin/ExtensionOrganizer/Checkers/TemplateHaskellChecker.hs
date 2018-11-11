module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Checkers.TemplateHaskellChecker where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad

-- NOTE: Here we implicitly constrained the type with ExtDomain.
--       but we don't really need any.

-- can be reached from: Decl, Type, Expr, Pattern
chkTemplateHaskellSplice :: CheckNode Splice
chkTemplateHaskellSplice = addEvidence TemplateHaskell

-- can be reached from: Type, Expr, Pattern
chkTemplateHaskellQuasiQuote :: CheckNode QuasiQuote
chkTemplateHaskellQuasiQuote = addEvidence QuasiQuotes

-- can be reached from: Expr
chkTemplateHaskellBracket :: CheckNode Bracket
chkTemplateHaskellBracket = addEvidence TemplateHaskellQuotes

chkTemplateHaskellhNamePart :: CheckNode NamePart
chkTemplateHaskellhNamePart = conditional chkTemplateHaskellNamePart' TemplateHaskellQuotes


-- should be THQuotes OR DataKinds
chkTemplateHaskellNamePart' :: CheckNode NamePart
chkTemplateHaskellNamePart' n@(NamePart name) =
  if (head name == '\'') then addEvidence TemplateHaskellQuotes n
                         else return n
