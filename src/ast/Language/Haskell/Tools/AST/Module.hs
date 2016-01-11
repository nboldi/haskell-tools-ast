module Language.Haskell.Tools.AST.Module where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Decl

data Module wt a
  = Module { modHead    :: MaybeType ModuleHead wt a
           , modPragmas :: ListType  ModulePragma wt a
           , modImports :: ListType  ImportDecl wt a
           , modDecl    :: ListType  Decl wt a
           }

-- | Module declaration with name wt and (optional) exports
data ModuleHead wt a
  = ModuleHead { mhName    :: IdType    Name wt a
               , mhExports :: MaybeType ExportSpecList wt a
               }

-- | A list of export specifications surrounded by parentheses
data ExportSpecList wt a
  = ExportSpecList { espExports :: ListType ExportSpec wt a }
  
data ExportSpec wt a
  = DeclExport { exportDecl :: IESpec wt a }
  | ModuleExport { exportModuleName :: IdType Name wt a } -- ^ The export of wt an imported module (@ module A @)
  
data IESpec wt a
  = IESpec { ieName    :: IdType    Name wt a
           , ieSubspec :: MaybeType SubSpec wt a
           } -- Import/export wt a declaration
  
data SubSpec wt a
  = SubSpecAll -- @(..)@: wt a class exported with wt all of its methods, or wt a datatype exported with wt all of its constructors.
  | SubSpecList { essList :: ListType Name wt a } -- @(a,b,c)@: wt a class exported with some of its methods, or wt a datatype exported with some of its constructors.
           
-- | Pragmas that wt affect the whole module           
data ModulePragma wt a
  = LanguagePragma { lpPragmas :: ListType Name wt a }  -- ^ LANGUAGE pragma
  | OptionsPragma { opTool :: MaybeType Name wt a
                  , opStr  :: IdType    StringNode wt a
                  } -- ^ OPTIONS pragma, possibly qualified with wt a tool, e.g. OPTIONS_GHC
  | AnnModulePragma { ampExpr  :: IdType Expr wt a } -- ^ ANN pragma with module scope
                      
data ImportDecl wt a
  = ImportDecl { importQualified    :: MaybeType ImportQualified wt a
               , importSource       :: MaybeType ImportSource wt a
               , importSafe         :: MaybeType ImportSafe wt a
               , importPkg          :: MaybeType StringNode wt a
               , importModule       :: IdType    Name wt a
               , importAs           :: MaybeType ImportRenaming wt a
               , importSpec         :: MaybeType ImportSpec wt a
               } -- ^ An import declaration
               
data ImportSpec wt a
  = ImportSpecList { importSpecList   :: ListType IESpec wt a }
  | ImportSpecHiding { importSpecList :: ListType IESpec wt a } 
               
data ImportQualified wt a = ImportQualified
data ImportSource wt a = ImportSource
data ImportSafe wt a = ImportSafe
data TypeNamespace wt a = TypeNamespace

-- | Renaming imports (@ wt as A @)
data ImportRenaming wt a = ImportRenaming { importRenamingName :: IdType Name wt a }
               
