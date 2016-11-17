{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Refactor.Session where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Reference

import Language.Haskell.Tools.AST (IdDom)
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.RefactorBase

type RefactorSession = StateT RefactorSessionState

data RefactorSessionState
  = RefactorSessionState { _refSessMCs :: [ModuleCollection (UnnamedModule IdDom)]
                         , _actualMod :: Maybe SourceFileKey
                         , _exiting :: Bool
                         , _dryMode :: Bool
                         }

initSession :: RefactorSessionState
initSession = RefactorSessionState [] Nothing False False

makeReferences ''RefactorSessionState

getMods :: Monad m => RefactorSession m (Maybe (SourceFileKey, UnnamedModule IdDom), [(SourceFileKey, UnnamedModule IdDom)])
getMods = do mcs <- gets (^. refSessMCs)
             actMod <- gets (^. actualMod)
             return $ ( flip lookupModInSCs mcs =<< actMod
                      , filter ((actMod /=) . Just . fst) $ concatMap (Map.assocs . (^. mcModules)) mcs )

assocToNamedMod :: (SourceFileKey, UnnamedModule dom) -> ModuleDom dom
assocToNamedMod (SourceFileKey _ n, mod) = (n, mod)
