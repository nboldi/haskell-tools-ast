-- | Representation of Haskell modules, imports and exports. Also contains file-level pragmas.
module Language.Haskell.Tools.AST.Modules where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Decls

-- | The representation of a haskell module, that is a separate compilation unit.
-- It may or may not have a header.
data UModule dom stage
  = UModule { _filePragmas :: AnnList UFilePragma dom stage
            , _modHead :: AnnMaybe UModuleHead dom stage
            , _modImports :: AnnList UImportDecl dom stage
            , _modDecl :: AnnList UDecl dom stage
            }

-- | UModule declaration with name and (optional) exports
data UModuleHead dom stage
  = UModuleHead { _mhName :: Ann UModuleName dom stage
                , _mhExports :: AnnMaybe UExportSpecList dom stage
                , _mhPragma :: AnnMaybe UModulePragma dom stage
                }

-- | A list of export specifications surrounded by parentheses
data UExportSpecList dom stage
  = UExportSpecList { _espExports :: AnnList UExportSpec dom stage }
  
-- | Export specifier
data UExportSpec dom stage
  = UDeclExport { _exportDecl :: Ann UIESpec dom stage
                } -- ^ Export a name and related names
  | UModuleExport { _exportModuleName :: Ann UModuleName dom stage
                  } -- ^ The export of an imported module (@ module A @)
  
-- | Marks a name to be imported or exported with related names (subspecifier)
data UIESpec dom stage
  = UIESpec { _ieName :: Ann UName dom stage
            , _ieSubspec :: AnnMaybe USubSpec dom stage
            }
  
-- | Marks how related names will be imported or exported with a given name
data USubSpec dom stage
  = USubSpecAll -- @(..)@: a class exported with all of its methods, or a datatype exported with all of its constructors.
  | USubSpecList { _essList :: AnnList UName dom stage } -- @(a,b,c)@: a class exported with some of its methods, or a datatype exported with some of its constructors.
           
-- | Pragmas that must be used before defining the module         
data UFilePragma dom stage
  = ULanguagePragma { _lpPragmas :: AnnList ULanguageExtension dom stage
                    }  -- ^ LANGUAGE pragmdom stage
  | UOptionsPragma {  _opStr :: Ann UStringNode dom stage
                   } -- ^ OPTIONS pragma, possibly qualified with a tool, e.g. OPTIONS_GHC
                        
-- | Pragmas that must be used after the module head  
data UModulePragma dom stage
  = UModuleWarningPragma { _modWarningStr :: AnnList UStringNode dom stage
                         }  -- ^ a warning pragma attached to the module
  | UModuleDeprecatedPragma {  _modDeprecatedPragma :: AnnList UStringNode dom stage
                            } -- ^ a deprecated pragma attached to the module

-- | The name of the enabled language extension, for example (@ LambdaCase @)
data ULanguageExtension dom stage = ULanguageExtension { _langExt :: String }

-- | An import declaration: @import Module.UName@         
data UImportDecl dom stage
  = UImportDecl { _importSource :: AnnMaybe UImportSource dom stage
                , _importQualified :: AnnMaybe UImportQualified dom stage
                , _importSafe :: AnnMaybe UImportSafe dom stage
                , _importPkg :: AnnMaybe UStringNode dom stage
                , _importModule :: Ann UModuleName dom stage
                , _importAs :: AnnMaybe UImportRenaming dom stage
                , _importSpec :: AnnMaybe UImportSpec dom stage
                }

-- | Restriction on the imported names
data UImportSpec dom stage
  = UImportSpecList { _importSpecList :: AnnList UIESpec dom stage
                    } -- ^ Restrict the import definition to ONLY import the listed names
  | UImportSpecHiding { _importSpecHiding :: AnnList UIESpec dom stage
                      } -- ^ Restrict the import definition to DONT import the listed names
               
-- | Marks the import as qualified: @qualified@
data UImportQualified dom stage = UImportQualified

-- | Marks the import as source: @{-# SOURCE #-}@
data UImportSource dom stage = UImportSource

-- | Marks the import as safe: @safe@
data UImportSafe dom stage = UImportSafe

-- | Marks an imported name to belong to the type namespace: @type@
data UTypeNamespace dom stage = UTypeNamespace

-- | Renaming imports (@ as A @)
data UImportRenaming dom stage = UImportRenaming { _importRename :: Ann UModuleName dom stage }