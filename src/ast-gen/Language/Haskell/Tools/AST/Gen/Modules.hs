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
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkModule :: [Ann UFilePragma dom SrcTemplateStage] -> Maybe (Ann UModuleHead dom SrcTemplateStage)
              -> [Ann UImportDecl dom SrcTemplateStage] -> [Ann UDecl dom SrcTemplateStage] -> Ann UModule dom SrcTemplateStage
mkModule filePrags head imps decls 
  = mkAnn (child <> child <> child <> child) 
      $ UModule (mkAnnList (listSepAfter "\n" "\n") filePrags) (mkAnnMaybe opt head)
                (mkAnnList (indentedListBefore "\n") imps) (mkAnnList (indentedListBefore "\n") decls)
               
mkModuleHead :: Ann UModuleName dom SrcTemplateStage -> Maybe (Ann UExportSpecList dom SrcTemplateStage) 
                  -> Maybe (Ann UModulePragma dom SrcTemplateStage) -> Ann UModuleHead dom SrcTemplateStage
mkModuleHead n es pr = mkAnn ("module " <> child <> child <> child <> " where") $ UModuleHead n (mkAnnMaybe opt es) (mkAnnMaybe (optBefore "\n") pr)

mkExportSpecList :: [Ann UExportSpec dom SrcTemplateStage] -> Ann UExportSpecList dom SrcTemplateStage
mkExportSpecList = mkAnn ("(" <> child <> ")") . UExportSpecList . mkAnnList (listSep ", ")

mkModuleExport :: Ann UModuleName dom SrcTemplateStage -> Ann UExportSpec dom SrcTemplateStage
mkModuleExport = mkAnn ("module " <> child) . UModuleExport

mkExportSpec :: Ann UIESpec dom SrcTemplateStage -> Ann UExportSpec dom SrcTemplateStage
mkExportSpec = mkAnn child . UDeclExport

mkIeSpec :: Ann UName dom SrcTemplateStage -> Maybe (Ann USubSpec dom SrcTemplateStage) -> Ann UIESpec dom SrcTemplateStage
mkIeSpec name ss = mkAnn (child <> child) (UIESpec name (mkAnnMaybe opt ss))
        
mkSubList :: [Ann UName dom SrcTemplateStage] -> Ann USubSpec dom SrcTemplateStage
mkSubList = mkAnn ("(" <> child <> ")") . USubSpecList . mkAnnList (listSep ", ")

mkSubAll :: Ann USubSpec dom SrcTemplateStage
mkSubAll = mkAnn "(..)" USubSpecAll

mkImportDecl :: Bool -> Bool -> Bool -> Maybe String -> Ann UModuleName dom SrcTemplateStage 
                     -> Maybe String -> Maybe (Ann UImportSpec dom SrcTemplateStage) 
                     -> Ann UImportDecl dom SrcTemplateStage       
mkImportDecl source qualified safe pkg name rename spec
  = mkAnn ("import " <> child <> child <> child <> child <> child <> child <> child) $
      UImportDecl (if source then justVal (mkAnn "{-# SOURCE #-} " UImportSource) else noth)
                 (if qualified then justVal (mkAnn "qualified " UImportQualified) else noth)
                 (if safe then justVal (mkAnn "safe " UImportSafe) else noth)
                 (case pkg of Just str -> justVal (mkStringNode str); _ -> noth)
                 name (mkAnnMaybe opt (fmap (mkAnn (" as " <> child) . UImportRenaming . mkModuleName) rename)) (mkAnnMaybe opt spec)

mkImportSpecList :: [Ann UIESpec dom SrcTemplateStage] -> Ann UImportSpec dom SrcTemplateStage
mkImportSpecList = mkAnn ("(" <> child <> ")") . UImportSpecList . mkAnnList (listSep ", ")

mkImportHidingList :: [Ann UIESpec dom SrcTemplateStage] -> Ann UImportSpec dom SrcTemplateStage
mkImportHidingList = mkAnn (" hiding (" <> child <> ")") . UImportSpecHiding . mkAnnList (listSep ", ")

mkModuleName :: String -> Ann UModuleName dom SrcTemplateStage
mkModuleName s = mkAnn (fromString s) (UModuleName s)