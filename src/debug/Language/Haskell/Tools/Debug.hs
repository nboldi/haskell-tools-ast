 {-# LANGUAGE DeriveGeneric, LambdaCase, StandaloneDeriving #-}
module Language.Haskell.Tools.Debug where

import Control.Monad (Monad(..), (=<<), forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Reference ((^.))
import Data.List.Split (splitOn)
import Data.Maybe (Maybe(..), fromJust)
import GHC.Generics (Generic(..))
import System.FilePath (pathSeparator, (</>), (<.>))

import DynFlags (xopt)
import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import Language.Haskell.TH.LanguageExtensions (Extension(..))
import StringBuffer (hGetStringBuffer)

import Language.Haskell.Tools.AST (NodeInfo(..))
import Language.Haskell.Tools.BackendGHC
import Language.Haskell.Tools.Debug.DebugGhcAST ()
import Language.Haskell.Tools.Debug.RangeDebug (srcInfoDebug)
import Language.Haskell.Tools.Debug.RangeDebugInstances ()
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)

-- | Should be only used for testing
demoRefactor :: String -> String -> [String] -> String -> IO ()
demoRefactor command workingDir args moduleName =
  runGhc (Just libdir) $ do
    initGhcFlags
    _ <- useFlags args
    useDirs [workingDir]
    ms <- loadModule workingDir moduleName
    p <- parseModule ms
    t <- typecheckModule p

    let annots = pm_annotations $ tm_parsed_module t
        hasCPP = Cpp `xopt` ms_hspp_opts ms

    liftIO $ putStrLn "=========== tokens:"
    liftIO $ putStrLn $ show (fst annots)
    liftIO $ putStrLn "=========== comments:"
    liftIO $ putStrLn $ show (snd annots)
    liftIO $ putStrLn "=========== parsed source:"
    liftIO $ putStrLn $ show (pm_parsed_source p)
    liftIO $ putStrLn "=========== renamed source:"
    liftIO $ putStrLn $ show (fromJust $ tm_renamed_source t)
    liftIO $ putStrLn "=========== typechecked source:"
    liftIO $ putStrLn $ show (typecheckedSource t)
    liftIO $ putStrLn "=========== parsed:"
    --transformed <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule (pm_parsed_source p)
    parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p)
    liftIO $ putStrLn $ srcInfoDebug parseTrf
    liftIO $ putStrLn "=========== typed:"
    transformed <- addTypeInfos (typecheckedSource t) =<< (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModuleRename ms parseTrf (fromJust $ tm_renamed_source t) (pm_parsed_source p))
    liftIO $ putStrLn $ srcInfoDebug transformed
    liftIO $ putStrLn "=========== ranges fixed:"
    sourceOrigin <- if hasCPP then liftIO $ hGetStringBuffer (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName <.> "hs")
                              else return (fromJust $ ms_hspp_buf $ pm_mod_summary p)
    let commented = fixRanges $ placeComments (fst annots) (getNormalComments $ snd annots) $ fixMainRange sourceOrigin transformed
    liftIO $ putStrLn $ srcInfoDebug commented
    liftIO $ putStrLn "=========== cut up:"
    let cutUp = cutUpRanges commented
    liftIO $ putStrLn $ srcInfoDebug cutUp
    liftIO $ putStrLn $ show $ getLocIndices cutUp

    liftIO $ putStrLn $ show $ mapLocIndices sourceOrigin (getLocIndices cutUp)
    liftIO $ putStrLn "=========== sourced:"
    let sourced = (if hasCPP then extractStayingElems else id) $ rangeToSource sourceOrigin cutUp
    liftIO $ putStrLn $ srcInfoDebug sourced
    liftIO $ putStrLn "=========== pretty printed:"
    let prettyPrinted = prettyPrint sourced
    liftIO $ putStrLn prettyPrinted
    transformed <- performCommand builtinRefactorings (splitOn " " command)
                                  (Right ((SourceFileKey (moduleSourceFile moduleName) moduleName), sourced))
                                  []
    case transformed of
      Right changes -> do
        forM_ changes $ \case
          ContentChanged (mod, correctlyTransformed) -> do
            liftIO $ putStrLn $ "=========== transformed AST (" ++ (mod ^. sfkModuleName) ++ "):"
            liftIO $ putStrLn $ srcInfoDebug correctlyTransformed
            liftIO $ putStrLn $ "=========== transformed & prettyprinted (" ++ (mod ^. sfkModuleName) ++ "):"
            let prettyPrinted = prettyPrint correctlyTransformed
            liftIO $ putStrLn prettyPrinted
            liftIO $ putStrLn "==========="
          ModuleRemoved mod -> do
            liftIO $ putStrLn $ "=========== module removed: " ++ mod
          ModuleCreated mod cont _ -> do
            liftIO $ putStrLn $ "=========== created AST (" ++ mod ++ "):"
            liftIO $ putStrLn $ srcInfoDebug cont
            liftIO $ putStrLn $ "=========== created & prettyprinted (" ++ mod ++ "):"
            let prettyPrinted = prettyPrint cont
            liftIO $ putStrLn prettyPrinted
      Left transformProblem -> do
        liftIO $ putStrLn "==========="
        liftIO $ putStrLn transformProblem
        liftIO $ putStrLn "==========="

deriving instance Generic SrcSpan
deriving instance Generic (NodeInfo sema src)
