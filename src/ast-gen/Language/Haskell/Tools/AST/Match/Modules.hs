-- | UPattern matching on UModule-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Modules where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

pattern Module :: FilePragmaList dom -> MaybeModuleHead dom
              -> ImportDeclList dom -> DeclList dom -> Module dom
pattern Module filePrags head imps decls  <- Ann _ (UModule filePrags head imps decls )

pattern ModuleHead :: ModuleName dom -> MaybeExportSpecs dom 
                  -> MaybeModulePragma dom -> ModuleHead dom
pattern ModuleHead n es pr <- Ann _ (UModuleHead n es pr)

pattern ExportSpecs :: ExportSpecList dom -> ExportSpecs dom
pattern ExportSpecs specs <- Ann _ (UExportSpecs specs)

pattern ModuleExport :: ModuleName dom -> ExportSpec dom
pattern ModuleExport name <- Ann _ (UModuleExport name)

pattern ExportSpec :: IESpec dom -> ExportSpec dom
pattern ExportSpec ieSpec <- Ann _ (UDeclExport ieSpec)

pattern IESpec :: Name dom -> MaybeSubSpec dom -> IESpec dom
pattern IESpec name ss <- Ann _ (UIESpec name ss)

pattern SubList :: NameList dom -> SubSpec dom
pattern SubList names <- Ann _ (USubSpecList names)

pattern SubAll :: SubSpec dom
pattern SubAll <- Ann _ USubSpecAll

pattern ImportDecl :: MaybeImportSource dom -> MaybeImportQualified dom 
                        -> MaybeImportSafe dom -> MaybeStringNode dom
                        -> ModuleName dom -> MaybeImportRenaming dom
                        -> MaybeImportSpec dom -> ImportDecl dom       
pattern ImportDecl source qualified safe pkg name rename spec <- Ann _ (UImportDecl source qualified safe pkg name rename spec)

pattern ImportRenaming :: ModuleName dom -> ImportRenaming dom
pattern ImportRenaming name <- Ann _ (UImportRenaming name)

pattern ImportSpecList :: IESpecList dom -> ImportSpec dom
pattern ImportSpecList ieSpecs <- Ann _ (UImportSpecList ieSpecs)

pattern ImportHidingList :: IESpecList dom -> ImportSpec dom
pattern ImportHidingList hidings <- Ann _ (UImportSpecHiding hidings)

pattern ModuleName :: String -> ModuleName dom
pattern ModuleName s <- Ann _ (UModuleName s)