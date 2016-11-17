{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           #-}
module Language.Haskell.Tools.Refactor.CLI (refactorSession) where

import System.Directory
import System.IO
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.List.Split
import Control.Monad.State
import Control.Reference

import GHC
import Digraph
import HscTypes as GHC
import Module as GHC
import GHC.Paths ( libdir )

import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Session

import Debug.Trace

tryOut = refactorSession [ "-dry-run", "-one-shot", "-module-name=Language.Haskell.Tools.AST", "-refactoring=OrganizeImports"
                         , "src/ast", "src/backend-ghc", "src/prettyprint", "src/rewrite", "src/refactor"]

refactorSession :: [String] -> IO String
refactorSession args = runGhc (Just libdir) $ flip evalStateT initSession $
  do lift $ initGhcFlags
     workingDirsAndHtFlags <- lift $ useFlags args
     let (htFlags, workingDirs) = partition (\f -> head f == '-') workingDirsAndHtFlags
     if null workingDirs then return usageMessage
                         else do initializeSession workingDirs htFlags
                                 runSession htFlags
     
  where initializeSession :: [FilePath] -> [String] -> RefactorSession Ghc ()
        initializeSession workingDirs flags = do
          modColls <- liftIO $ getAllModules workingDirs

          liftIO $ putStrLn "Compiling modules. This may take some time. Please wait."
          modColls' <- lift $ flip evalStateT [] $ forM modColls $ \mc -> do
            lift $ useDirs (mc ^. mcSourceDirs)
            lift $ setTargets $ map (\mod -> (Target (TargetModule (GHC.mkModuleName mod)) True Nothing)) 
                                             (map (^. sfkModuleName) $ Map.keys $ mc ^. mcModules)
            alreadyLoaded <- get
            -- depanal sets the dynamic flags for modules, so they need to be set before calling it
            withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup)) $
              do modsForMC <- lift $ depanal alreadyLoaded True
                 mods <- lift $ forM (flattenSCCs $ topSortModuleGraph False modsForMC Nothing) loadModule
                 modify $ (++ map ms_mod_name modsForMC)
                 return $ (mcModules .= Map.fromList mods) mc

          liftIO $ putStrLn "All modules loaded. Use 'SelectModule module-name' to select a module"
          modify $ refSessMCs .= modColls'
          liftIO $ hSetBuffering stdout NoBuffering
          when ("-dry-run" `elem` flags) $ modify (dryMode .= True)

        loadModule :: ModSummary -> Ghc (SourceFileKey, TypedModule)
        loadModule ms = 
          do let modName = GHC.moduleNameString $ moduleName $ ms_mod ms
             mm <- parseTyped ms
             liftIO $ putStrLn ("Loaded module: " ++ modName)
             return (SourceFileKey (case ms_hsc_src ms of HsSrcFile -> NormalHs; _ -> IsHsBoot) modName, mm)

        runSession :: [String] -> RefactorSession Ghc String
        runSession flags | "-one-shot" `elem` flags
          = let modName = catMaybes $ map (\f -> case splitOn "=" f of ["-module-name", mod] -> Just mod; _ -> Nothing) flags
                refactoring = catMaybes $ map (\f -> case splitOn "=" f of ["-refactoring", ref] -> Just ref; _ -> Nothing) flags
             in case (modName, refactoring) of 
                  ([modName],[refactoring]) ->
                    do performSessionCommand (LoadModule modName)
                       command <- readSessionCommand (takeWhile (/='"') $ dropWhile (=='"') $ refactoring)
                       performSessionCommand command
                  _ -> return usageMessage
        runSession _ = runSessionLoop

        runSessionLoop :: RefactorSession Ghc String
        runSessionLoop = do 
          actualMod <- gets (^. actualMod)
          liftIO $ putStr (maybe "no-module-selected" (\sfk -> (sfk ^. sfkModuleName) ++ ">") actualMod)
          cmd <- liftIO $ getLine 
          sessionComm <- readSessionCommand cmd
          liftIO . putStrLn =<< performSessionCommand sessionComm
          doExit <- gets (^. exiting)
          when (not doExit) (void runSessionLoop)
          return ""

        usageMessage = "Usage: ht-refact [ht-flags, ghc-flags] package-pathes\n"
                         ++ "ht-flags: -dry-run -one-shot -module-name=modulename -refactoring=\"refactoring\""

withAlteredDynFlags :: GhcMonad m => (DynFlags -> m DynFlags) -> m a -> m a
withAlteredDynFlags modDFs action = do
  dfs <- getSessionDynFlags
  setSessionDynFlags =<< modDFs dfs
  res <- action
  setSessionDynFlags dfs
  return res

data RefactorSessionCommand 
  = LoadModule String
  | Exit
  | RefactorCommand RefactorCommand
  deriving Show

readSessionCommand :: Monad m => String -> RefactorSession m RefactorSessionCommand
readSessionCommand cmd = case splitOn " " cmd of 
    ["SelectModule", mod] -> return $ LoadModule mod
    ["Exit"] -> return Exit
    _ -> do actualMod <- gets (^. actualMod)
            case actualMod of Just sf -> return $ RefactorCommand $ readCommand "" cmd
                              Nothing -> error "Set the actual module first"

performSessionCommand :: RefactorSessionCommand -> RefactorSession Ghc String
performSessionCommand (LoadModule modName) = do 
  mod <- gets (lookupModInSCs (SourceFileKey NormalHs modName) . (^. refSessMCs))
  if isJust mod then modify $ actualMod .= fmap fst mod
                else liftIO $ putStrLn ("Cannot find module: " ++ modName)
  return ""
performSessionCommand Exit = do modify $ exiting .= True
                                return ""
performSessionCommand (RefactorCommand cmd) 
  = do (Just actualMod, otherMods) <- getMods
       res <- lift $ performCommand cmd (assocToNamedMod actualMod) (map assocToNamedMod otherMods)
       inDryMode <- gets (^. dryMode)
       case res of Left err -> return err
                   Right resMods -> performChanges inDryMode resMods
                     
  where performChanges False resMods = do 
          mss <- forM resMods $ \case 
            ContentChanged (n,m) -> do
              let modName = semanticsModule $ m ^. semantics
              ms <- getModSummary modName (isBootModule $ m ^. semantics)
              let file = fromJust $ ml_hs_file $ ms_location ms
              liftIO $ withBinaryFile file WriteMode (`hPutStr` prettyPrint m)
              return $ Just (GHC.moduleNameString $ moduleName modName, ms)
            ModuleRemoved mod -> do
              modify $ refSessMCs .- removeModule mod
              return Nothing
          forM_ (catMaybes mss) $ \(modName, ms) -> do
              -- TODO: add target if module is added as a change
              Just mc <- gets (lookupModuleColl modName . (^. refSessMCs))
              newm <- lift $ withAlteredDynFlags (liftIO . (mc ^. mcFlagSetup)) $
                parseTyped ms
              modify $ refSessMCs .- updateModule modName NormalHs newm
              liftIO $ putStrLn ("Re-loaded module: " ++ modName)
          return ""
        performChanges True resMods = concat <$> forM resMods (liftIO . \case 
          ContentChanged (n,m) -> do
            return $ "### Module changed: " ++ n ++ "\n### new content:\n" ++ prettyPrint m
          ModuleRemoved mod ->
            return $ "### Module removed: " ++ mod)

        getModSummary name boot
          = do allMods <- lift getModuleGraph
               return $ fromJust $ find (\ms -> ms_mod ms == name && (ms_hsc_src ms == HsSrcFile) /= boot) allMods 
