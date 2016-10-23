-- | Generation of UModule-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkModule@ creates the annotated version of the @UModule@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Modules where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkModule :: [FilePragma dom] -> Maybe (ModuleHead dom) -> [ImportDecl dom] -> [Decl dom] -> Module dom
mkModule filePrags head imps decls 
  = mkAnn (child <> child <> child <> child) 
      $ UModule (mkAnnList (listSepAfter "\n" "\n") filePrags) (mkAnnMaybe opt head)
                (mkAnnList (indentedListBefore "\n") imps) (mkAnnList (indentedListBefore "\n") decls)
               
mkModuleHead :: ModuleName dom -> Maybe (ExportSpecs dom) -> Maybe (ModulePragma dom) -> ModuleHead dom
mkModuleHead n es pr = mkAnn ("module " <> child <> child <> child <> " where") $ UModuleHead n (mkAnnMaybe opt es) (mkAnnMaybe (optBefore "\n") pr)

mkExportSpecs :: [ExportSpec dom] -> ExportSpecs dom
mkExportSpecs = mkAnn ("(" <> child <> ")") . UExportSpecs . mkAnnList (listSep ", ")

mkModuleExport :: ModuleName dom -> ExportSpec dom
mkModuleExport = mkAnn ("module " <> child) . UModuleExport

mkExportSpec :: IESpec dom -> ExportSpec dom
mkExportSpec = mkAnn child . UDeclExport

mkIeSpec :: Name dom -> Maybe (SubSpec dom) -> IESpec dom
mkIeSpec name ss = mkAnn (child <> child) (UIESpec name (mkAnnMaybe opt ss))
        
mkSubList :: [Name dom] -> SubSpec dom
mkSubList = mkAnn ("(" <> child <> ")") . USubSpecList . mkAnnList (listSep ", ")

mkSubAll :: SubSpec dom
mkSubAll = mkAnn "(..)" USubSpecAll

mkImportDecl :: Bool -> Bool -> Bool -> Maybe String -> ModuleName dom -> Maybe String -> Maybe (ImportSpec dom) 
                  -> ImportDecl dom       
mkImportDecl source qualified safe pkg name rename spec
  = mkAnn ("import " <> child <> child <> child <> child <> child <> child <> child) $
      UImportDecl (if source then justVal (mkAnn "{-# SOURCE #-} " UImportSource) else noth)
                 (if qualified then justVal (mkAnn "qualified " UImportQualified) else noth)
                 (if safe then justVal (mkAnn "safe " UImportSafe) else noth)
                 (case pkg of Just str -> justVal (mkStringNode str); _ -> noth)
                 name (mkAnnMaybe opt (fmap (mkAnn (" as " <> child) . UImportRenaming . mkModuleName) rename)) (mkAnnMaybe opt spec)

mkImportSpecList :: [IESpec dom] -> ImportSpec dom
mkImportSpecList = mkAnn ("(" <> child <> ")") . UImportSpecList . mkAnnList (listSep ", ")

mkImportHidingList :: [IESpec dom] -> ImportSpec dom
mkImportHidingList = mkAnn (" hiding (" <> child <> ")") . UImportSpecHiding . mkAnnList (listSep ", ")

mkModuleName :: String -> ModuleName dom
mkModuleName s = mkAnn (fromString s) (UModuleName s)