-- | UPattern matching on UModule-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Modules where

import Language.Haskell.Tools.AST

pattern Module :: AnnListG UFilePragma dom stage -> AnnMaybeG UModuleHead dom stage
              -> AnnListG UImportDecl dom stage -> AnnListG UDecl dom stage -> Ann UModule dom stage
pattern Module filePrags head imps decls  <- Ann _ (UModule filePrags head imps decls )

pattern ModuleHead :: Ann UModuleName dom stage -> AnnMaybeG UExportSpecList dom stage 
                  -> AnnMaybeG UModulePragma dom stage -> Ann UModuleHead dom stage
pattern ModuleHead n es pr <- Ann _ (UModuleHead n es pr)

pattern ExportSpecList :: AnnListG UExportSpec dom stage -> Ann UExportSpecList dom stage
pattern ExportSpecList specs <- Ann _ (UExportSpecList specs)

pattern ModuleExport :: Ann UModuleName dom stage -> Ann UExportSpec dom stage
pattern ModuleExport name <- Ann _ (UModuleExport name)

pattern ExportSpec :: Ann UIESpec dom stage -> Ann UExportSpec dom stage
pattern ExportSpec ieSpec <- Ann _ (UDeclExport ieSpec)

pattern IESpec :: Ann UName dom stage -> AnnMaybeG USubSpec dom stage -> Ann UIESpec dom stage
pattern IESpec name ss <- Ann _ (UIESpec name ss)

pattern SubList :: AnnListG UName dom stage -> Ann USubSpec dom stage
pattern SubList names <- Ann _ (USubSpecList names)

pattern SubAll :: Ann USubSpec dom stage
pattern SubAll <- Ann _ USubSpecAll

pattern ImportDecl :: AnnMaybeG UImportSource dom stage -> AnnMaybeG UImportQualified dom stage 
                        -> AnnMaybeG UImportSafe dom stage -> AnnMaybeG UStringNode dom stage
                        -> Ann UModuleName dom stage -> AnnMaybeG UImportRenaming dom stage
                        -> AnnMaybeG UImportSpec dom stage -> Ann UImportDecl dom stage       
pattern ImportDecl source qualified safe pkg name rename spec <- Ann _ (UImportDecl source qualified safe pkg name rename spec)

pattern ImportRenaming :: Ann UModuleName dom stage -> Ann UImportRenaming dom stage
pattern ImportRenaming name <- Ann _ (UImportRenaming name)

pattern ImportSpecList :: AnnListG UIESpec dom stage -> Ann UImportSpec dom stage
pattern ImportSpecList ieSpecs <- Ann _ (UImportSpecList ieSpecs)

pattern ImportHidingList :: AnnListG UIESpec dom stage -> Ann UImportSpec dom stage
pattern ImportHidingList hidings <- Ann _ (UImportSpecHiding hidings)

pattern ModuleName :: String -> Ann UModuleName dom stage
pattern ModuleName s <- Ann _ (UModuleName s)