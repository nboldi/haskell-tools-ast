{-# LANGUAGE TemplateHaskell #-}
module Language.Haskell.Tools.Refactor.Session where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Reference
import System.IO

import GHC
import GhcMonad as GHC
import HscTypes as GHC
import Digraph as GHC

import Language.Haskell.Tools.AST (IdDom)
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.RefactorBase

data RefactorSessionState
  = RefactorSessionState { __refSessMCs :: [ModuleCollection (UnnamedModule IdDom)]
                         }

makeReferences ''RefactorSessionState

class IsRefactSessionState st where
  refSessMCs :: Simple Lens st [ModuleCollection (UnnamedModule IdDom)]
  initSession :: st

instance IsRefactSessionState RefactorSessionState where
  refSessMCs = _refSessMCs
  initSession = RefactorSessionState []


loadPackagesFrom :: IsRefactSessionState st => (String -> IO a) -> [FilePath] -> StateT st Ghc [a]
loadPackagesFrom report packages = 
  do modColls <- liftIO $ getAllModules packages
     res <- lift $ flip evalStateT [] $ forM modColls $ \mc -> do
       lift $ useDirs (mc ^. mcSourceDirs)
       lift $ setTargets $ map (\mod -> (Target (TargetModule (GHC.mkModuleName mod)) True Nothing)) 
                                        (map (^. sfkModuleName) $ Map.keys $ mc ^. mcModules)
       -- lift $ load LoadAllTargets
       alreadyLoaded <- get
       -- depanal sets the dynamic flags for modules, so they need to be set before calling it
       withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup)) $
         do modsForMC <- lift $ depanal alreadyLoaded True
            let modsToParse = flattenSCCs $ topSortModuleGraph False modsForMC Nothing
            mods <- lift $ mapM (loadModule report) modsToParse
            modify $ (++ map ms_mod_name modsForMC)
            return $ (map fst mods, (mcModules .= Map.fromList (map snd mods)) mc)
     modify $ refSessMCs .= map snd res
     return (concatMap fst res)

  where loadModule :: (String -> IO a) -> ModSummary -> Ghc (a, (SourceFileKey, UnnamedModule IdDom))
        loadModule report ms = 
          do let modName = GHC.moduleNameString $ moduleName $ ms_mod ms
             mm <- parseTyped ms
             rep <- liftIO $ report modName
             res <- return (rep, (SourceFileKey (case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot) modName, mm))
             return res

getMods :: (Monad m, IsRefactSessionState st) => Maybe SourceFileKey -> StateT st m (Maybe (SourceFileKey, UnnamedModule IdDom), [(SourceFileKey, UnnamedModule IdDom)])
getMods actMod 
  = do mcs <- gets (^. refSessMCs)
       return $ ( flip lookupModInSCs mcs =<< actMod
                , filter ((actMod /=) . Just . fst) $ concatMap (Map.assocs . (^. mcModules)) mcs )

assocToNamedMod :: (SourceFileKey, UnnamedModule dom) -> ModuleDom dom
assocToNamedMod (SourceFileKey _ n, mod) = (n, mod)


withAlteredDynFlags :: GhcMonad m => (DynFlags -> m DynFlags) -> m a -> m a
withAlteredDynFlags modDFs action = do
  dfs <- getSessionDynFlags
  setSessionDynFlags =<< modDFs dfs
  res <- action
  setSessionDynFlags dfs
  return res

-- reloadChangedModules :: IsRefactSessionState st => [String] -> StateT st Ghc ()
-- reloadChangedModules changedMods = do
--   allMods <- lift $ depanal alreadyLoaded True
--   let allModsGraph = topSortModuleGraph False allMods Nothing

  

reloadModule :: IsRefactSessionState st => String -> ModSummary -> StateT st Ghc ()
reloadModule modName ms = do 
  Just mc <- gets (lookupModuleColl modName . (^. refSessMCs))
  newm <- lift $ withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup)) $
    parseTyped ms
  modify $ refSessMCs .- updateModule modName NormalHs newm