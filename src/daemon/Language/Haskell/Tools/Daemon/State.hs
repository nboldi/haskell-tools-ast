{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Daemon.State where

import Control.Reference

import Language.Haskell.Tools.Daemon.PackageDB
import Language.Haskell.Tools.Daemon.Representation
import Language.Haskell.Tools.Daemon.Session
import Language.Haskell.Tools.Refactor

data DaemonSessionState
  = DaemonSessionState { _refactorSession :: RefactorSessionState
                       , _packageDB :: PackageDB
                       , _packageDBSet :: Bool
                       , _packageDBLocs :: [FilePath]
                       , _exiting :: Bool
                       }

makeReferences ''DaemonSessionState

instance IsRefactSessionState DaemonSessionState where
  refSessMCs = refactorSession & refSessMCs
  initSession = DaemonSessionState initSession AutoDB False [] False
