{-# LANGUAGE RecordWildCards
           , TypeSynonymInstances
           , FlexibleInstances
           , TemplateHaskell
           #-}
-- | Representation of modules, their collections, refactoring changes and exceptions.
module Language.Haskell.Tools.Refactor.Representation where

import Control.Exception
import Control.Reference
import Data.Typeable
import qualified Data.Map as Map
import qualified Name as GHC
import Data.List
import Data.List.Split
import System.FilePath
import ErrUtils as GHC

import qualified Module as GHC
import DynFlags (HasDynFlags(..))
import GHC hiding (mkModuleName, moduleNameString)
import Outputable
import Bag (bagToList)

import Language.Haskell.Tools.AST as AST

-- | A type for the input and result of refactoring a module
type UnnamedModule dom = Ann AST.UModule dom SrcTemplateStage

-- | The name of the module and the AST
type ModuleDom dom = (SourceFileKey, UnnamedModule dom)

-- | Module name and marker to separate .hs-boot module definitions. Specifies a source file in a working directory.
data SourceFileKey = SourceFileKey { _sfkFileName :: FilePath
                                   , _sfkModuleName :: String
                                   }
  deriving (Eq, Ord, Show)

-- | Change in the project, modification or removal of a module.
data RefactorChange dom = ContentChanged { fromContentChanged :: ModuleDom dom }
                        | ModuleRemoved { removedModuleName :: String }
                        | ModuleCreated { createdModuleName :: String
                                        , createdModuleContent :: UnnamedModule dom
                                        , sameLocation :: SourceFileKey
                                        }
instance Show (RefactorChange dom) where
  show (ContentChanged (n, _)) = "ContentChanged (" ++ show n  ++ ")"
  show (ModuleRemoved n) = "ModuleRemoved " ++ n
  show (ModuleCreated n _ other) = "ModuleCreated " ++ n ++ " (" ++ show other ++ ")"

-- | Exceptions that can occur while loading modules or during internal operations (not during performing the refactor).
data RefactorException = IllegalExtensions [String]
                       | SourceCodeProblem ErrorMessages
                       | ModuleNotInPackage String
                       | UnknownException String
  deriving (Show, Typeable)

instance Show ErrorMessages where
  show = show . bagToList

instance Exception RefactorException where
  displayException (SourceCodeProblem prob)
    = "Source code problem: " ++ showSDocUnsafe (vcat (pprErrMsgBagWithLoc prob))
  displayException (IllegalExtensions exts)
    = "The following extensions are not allowed: " ++ (concat $ intersperse ", " exts) ++ "."
  displayException (ModuleNotInPackage modName) = "The module is not in the package: " ++ modName
  displayException (UnknownException ex) = "An unexpected problem appeared: " ++ ex ++ "."

-- | Transforms module name to a .hs file name relative to the source root directory.
moduleSourceFile :: String -> FilePath
moduleSourceFile m = (foldl1 (</>) (splitOn "." m)) <.> "hs"

-- | Transforms a source root relative file name into module name.
sourceFileModule :: FilePath -> String
sourceFileModule fp = intercalate "." $ splitDirectories $ dropExtension fp

makeReferences ''SourceFileKey
