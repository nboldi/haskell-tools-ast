-- | UPattern matching on UModule-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Modules where

import Language.Haskell.Tools.AST

pattern Module :: AnnList UFilePragma dom stage -> AnnMaybe UModuleHead dom stage
              -> AnnList UImportDecl dom stage -> AnnList UDecl dom stage -> Ann UModule dom stage
pattern Module filePrags head imps decls  <- Ann _ (UModule filePrags head imps decls )

pattern ModuleHead :: Ann UModuleName dom stage -> AnnMaybe UExportSpecList dom stage 
                  -> AnnMaybe UModulePragma dom stage -> Ann UModuleHead dom stage
pattern ModuleHead n es pr <- Ann _ (UModuleHead n es pr)

pattern ExportSpecList :: AnnList UExportSpec dom stage -> Ann UExportSpecList dom stage
pattern ExportSpecList specs <- Ann _ (UExportSpecList specs)

pattern ModuleExport :: Ann UModuleName dom stage -> Ann UExportSpec dom stage
pattern ModuleExport name <- Ann _ (UModuleExport name)

pattern ExportSpec :: Ann UIESpec dom stage -> Ann UExportSpec dom stage
pattern ExportSpec ieSpec <- Ann _ (UDeclExport ieSpec)

pattern IESpec :: Ann UName dom stage -> AnnMaybe USubSpec dom stage -> Ann UIESpec dom stage
pattern IESpec name ss <- Ann _ (UIESpec name ss)

pattern SubList :: AnnList UName dom stage -> Ann USubSpec dom stage
pattern SubList names <- Ann _ (USubSpecList names)

pattern SubAll :: Ann USubSpec dom stage
pattern SubAll <- Ann _ USubSpecAll

pattern ImportDecl :: AnnMaybe UImportSource dom stage -> AnnMaybe UImportQualified dom stage 
                        -> AnnMaybe UImportSafe dom stage -> AnnMaybe UStringNode dom stage
                        -> Ann UModuleName dom stage -> AnnMaybe UImportRenaming dom stage
                        -> AnnMaybe UImportSpec dom stage -> Ann UImportDecl dom stage       
pattern ImportDecl source qualified safe pkg name rename spec <- Ann _ (UImportDecl source qualified safe pkg name rename spec)

pattern ImportRenaming :: Ann UModuleName dom stage -> Ann UImportRenaming dom stage
pattern ImportRenaming name <- Ann _ (UImportRenaming name)

pattern ImportSpecList :: AnnList UIESpec dom stage -> Ann UImportSpec dom stage
pattern ImportSpecList ieSpecs <- Ann _ (UImportSpecList ieSpecs)

pattern ImportHidingList :: AnnList UIESpec dom stage -> Ann UImportSpec dom stage
pattern ImportHidingList hidings <- Ann _ (UImportSpecHiding hidings)
