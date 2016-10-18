-- | Pattern matching on Module-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Modules where

import Language.Haskell.Tools.AST

pattern Module :: AnnList FilePragma dom stage -> AnnMaybe ModuleHead dom stage
              -> AnnList ImportDecl dom stage -> AnnList UDecl dom stage -> Ann Module dom stage
pattern Module filePrags head imps decls  <- Ann _ (UModule filePrags head imps decls )

pattern ModuleHead :: Ann UModuleName dom stage -> AnnMaybe ExportSpecList dom stage 
                  -> AnnMaybe ModulePragma dom stage -> Ann ModuleHead dom stage
pattern ModuleHead n es pr <- Ann _ (UModuleHead n es pr)

pattern ExportSpecList :: AnnList ExportSpec dom stage -> Ann ExportSpecList dom stage
pattern ExportSpecList specs <- Ann _ (UExportSpecList specs)

pattern ModuleExport :: Ann UModuleName dom stage -> Ann ExportSpec dom stage
pattern ModuleExport name <- Ann _ (UModuleExport name)

pattern ExportSpec :: Ann IESpec dom stage -> Ann ExportSpec dom stage
pattern ExportSpec ieSpec <- Ann _ (UDeclExport ieSpec)

pattern IESpec :: Ann UName dom stage -> AnnMaybe SubSpec dom stage -> Ann IESpec dom stage
pattern IESpec name ss <- Ann _ (UIESpec name ss)

pattern SubList :: AnnList UName dom stage -> Ann SubSpec dom stage
pattern SubList names <- Ann _ (USubSpecList names)

pattern SubAll :: Ann SubSpec dom stage
pattern SubAll <- Ann _ USubSpecAll

pattern ImportDecl :: AnnMaybe ImportSource dom stage -> AnnMaybe ImportQualified dom stage 
                        -> AnnMaybe ImportSafe dom stage -> AnnMaybe UStringNode dom stage
                        -> Ann UModuleName dom stage -> AnnMaybe ImportRenaming dom stage
                        -> AnnMaybe ImportSpec dom stage -> Ann ImportDecl dom stage       
pattern ImportDecl source qualified safe pkg name rename spec <- Ann _ (UImportDecl source qualified safe pkg name rename spec)

pattern ImportRenaming :: Ann UModuleName dom stage -> Ann ImportRenaming dom stage
pattern ImportRenaming name <- Ann _ (UImportRenaming name)

pattern ImportSpecList :: AnnList IESpec dom stage -> Ann ImportSpec dom stage
pattern ImportSpecList ieSpecs <- Ann _ (UImportSpecList ieSpecs)

pattern ImportHidingList :: AnnList IESpec dom stage -> Ann ImportSpec dom stage
pattern ImportHidingList hidings <- Ann _ (UImportSpecHiding hidings)
