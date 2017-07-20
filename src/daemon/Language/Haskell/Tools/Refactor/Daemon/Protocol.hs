{-# LANGUAGE DeriveGeneric
           , OverloadedStrings
           #-}
module Language.Haskell.Tools.Refactor.Daemon.Protocol where

import qualified Data.Aeson as A ((.=))
import Data.Aeson hiding ((.=))
import GHC.Generics

import FastString
import SrcLoc

import Language.Haskell.Tools.Refactor.Daemon.PackageDB


data ClientMessage
  = KeepAlive
  | Handshake { clientVersion :: [Int] }
  | SetPackageDB { pkgDB :: PackageDB }
  | AddPackages { addedPathes :: [FilePath] }
  | RemovePackages { removedPathes :: [FilePath] }
  | SetWorkingDir { newWorkingDir :: FilePath }
  | SetGHCFlags { ghcFlags :: [String] }
  | PerformRefactoring { refactoring :: String
                       , modulePath :: FilePath
                       , editorSelection :: String
                       , details :: [String]
                       }
  | Stop
  | Disconnect
  | ReLoad { addedModules :: [FilePath]
           , changedModules :: [FilePath]
           , removedModules :: [FilePath]
           }
  deriving (Show, Generic)

instance FromJSON ClientMessage

data ResponseMsg
  = KeepAliveResponse
  | HandshakeResponse { serverVersion :: [Int] }
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMarkers :: [(SrcSpan, String)] }
  | ModulesChanged { undoChanges :: [UndoRefactor] }
  | LoadedModules { loadedModules :: [(FilePath, String)] }
  | LoadingModules { modulesToLoad :: [FilePath] }
  | UnusedFlags { unusedFlags :: [String] }
  | Disconnected
  deriving (Show, Generic)

instance ToJSON ResponseMsg

instance ToJSON SrcSpan where
  toJSON (RealSrcSpan sp) = object [ "file" A..= unpackFS (srcSpanFile sp)
                                   , "startRow" A..= srcLocLine (realSrcSpanStart sp)
                                   , "startCol" A..= srcLocCol (realSrcSpanStart sp)
                                   , "endRow" A..= srcLocLine (realSrcSpanEnd sp)
                                   , "endCol" A..= srcLocCol (realSrcSpanEnd sp)
                                   ]
  toJSON _ = Null

data UndoRefactor = RemoveAdded { undoRemovePath :: FilePath }
                  | RestoreRemoved { undoRestorePath :: FilePath
                                   , undoRestoreContents :: String
                                   }
                  | UndoChanges { undoChangedPath :: FilePath
                                , undoDiff :: FileDiff
                                }
  deriving (Show, Generic)

instance ToJSON UndoRefactor

type FileDiff = [(Int, Int, String)]
