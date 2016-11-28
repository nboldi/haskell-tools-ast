{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings 
           , DeriveGeneric
           , LambdaCase
           , TemplateHaskell
           #-}
module Language.Haskell.Tools.Refactor.Daemon where

import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import GHC.Generics

import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy
import Control.Exception
import Data.Aeson hiding ((.=))
import Data.Map (Map, (!), member, insert)
import qualified Data.Map as Map
import GHC.Generics

import System.IO
import System.IO.Error
import System.FilePath
import System.Directory
import Data.IORef
import Data.List hiding (insert)
import Data.Tuple
import Data.Maybe
import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import System.Environment
import Debug.Trace

import GHC hiding (loadModule)
import Bag (bagToList)
import SrcLoc (realSrcSpanStart)
import ErrUtils (errMsgSpan)
import DynFlags (gopt_set)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), reflectGhc)
import HscTypes (SourceError, srcErrorMessages)
import FastString (unpackFS)

import Control.Reference

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.Daemon.State

-- TODO: find out which modules have changed
-- TODO: handle boot files


runDaemonCLI :: IO ()
runDaemonCLI = getArgs >>= runDaemon

runDaemon :: [String] -> IO ()
runDaemon _ = withSocketsDo $
    do addrinfos <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just "4123")
       let serveraddr = head addrinfos
       sock <- socket (addrFamily serveraddr) Stream defaultProtocol
       setSocketOption sock ReuseAddr 1
       bind sock (addrAddress serveraddr)
       listen sock 1
       clientLoop sock

clientLoop :: Socket -> IO ()
clientLoop sock
  = do (conn,_) <- accept sock
       ghcSess <- initGhcSession
       state <- newMVar initSession
       serverLoop ghcSess state conn
       sessionData <- readMVar state
       when (not (sessionData ^. exiting))
         $ clientLoop sock

serverLoop :: Session -> MVar DaemonSessionState -> Socket -> IO ()
serverLoop ghcSess state sock =
    do msg <- recv sock 1024
       putStrLn $ "message received: " ++ unpack msg
       respondTo ghcSess state (sendAll sock) msg
       sessionData <- readMVar state
       when (not (sessionData ^. exiting))
         $ serverLoop ghcSess state sock
  `catch` interrupted sock
  where interrupted = \s ex -> do
                        let err = show (ex :: IOException)
                        putStrLn "Closing down socket"
                        hPutStrLn stderr $ "Some exception caught: " ++ err

respondTo :: Session -> MVar DaemonSessionState -> (ByteString -> IO ()) -> ByteString -> IO ()
respondTo ghcSess state next mess = case decode mess of
  Nothing -> next $ encode $ ErrorMessage "WRONG MESSAGE FORMAT"
  Just req -> modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient (next . encode) req) st) ghcSess)

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: (ResponseMsg -> IO ()) -> ClientMessage -> StateT DaemonSessionState Ghc ()
updateClient resp KeepAlive = liftIO $ resp KeepAliveResponse
updateClient resp (AddPackages packagePathes) = do 
    (modules, ignoredMods) <- loadPackagesFrom (return . getModSumOrig) packagePathes
    liftIO $ resp 
      $ if not (null ignoredMods) 
          then ErrorMessage 
                 $ "The following modules are ignored: " 
                     ++ concat (intersperse ", " $ map (\(id,mod) -> mod ++ " (from " ++ moduleCollectionIdString id ++ ")") ignoredMods)
                     ++ ". Multiple modules with the same qualified name are not supported."
          else LoadedModules modules

updateClient resp (ReLoad changed) =
  void $ reloadChangedModules (\ms -> resp $ LoadedModules [getModSumOrig ms]) (\ms -> getModSumOrig ms `elem` changed)
updateClient _ Stop = modify (exiting .= True)

updateClient resp (PerformRefactoring refact modPath selection args) = do
    (Just actualMod, otherMods) <- getFileMods modPath
    let cmd = analyzeCommand refact (selection:args)
    res <- lift $ performCommand cmd (assocToNamedMod actualMod) (map assocToNamedMod otherMods)
    case res of
      Left err -> liftIO $ resp $ ErrorMessage err
      Right diff -> do changedMods <- applyChanges diff
                       liftIO $ resp $ ModulesChanged (map snd changedMods)
                       void $ reloadChanges (map fst changedMods)
  where applyChanges changes = do 
          forM changes $ \case 
            ContentChanged (n,m) -> do
              ms <- lift $ getModSummary (mkModuleName n)
              liftIO $ withBinaryFile (getModSumOrig ms) WriteMode (`hPutStr` prettyPrint m)
              return (n, getModSumOrig ms)
            ModuleRemoved mod -> do
              Just (_,m) <- gets (lookupModInSCs (SourceFileKey NormalHs mod) . (^. refSessMCs))
              let modName = GHC.moduleName $ fromJust $ fmap semanticsModule (m ^? typedRecModule) <|> fmap semanticsModule (m ^? renamedRecModule)
              ms <- getModSummary modName
              modify $ (refSessMCs .- removeModule mod)
              liftIO $ removeFile (getModSumOrig ms)
              return (mod, getModSumOrig ms)
          
        reloadChanges changedMods 
          = reloadChangedModules (\ms -> resp $ LoadedModules [getModSumOrig ms]) (\ms -> modSumName ms `elem` changedMods)

getModuleFilePath :: UnnamedModule IdDom -> Ghc FilePath
getModuleFilePath mod = do
  let modName = GHC.moduleName $ semanticsModule mod 
  ms <- getModSummary modName
  return $ getModSumOrig ms

createFileForModule :: FilePath -> String -> String -> IO ()
createFileForModule dir name newContent = do
  let fname = toFileName dir name
  createDirectoryIfMissing True (takeDirectory fname)
  withBinaryFile fname WriteMode (`hPutStr` newContent) 

removeDirectoryIfPresent :: FilePath -> IO ()
removeDirectoryIfPresent dir = removeDirectoryRecursive dir `catch` \e -> if isDoesNotExistError e then return () else throwIO e

moduleNameAndContent :: ((String,IsBoot), mod) -> (String, mod)
moduleNameAndContent ((name,_), mod) = (name, mod)

initGhcSession :: IO Session
initGhcSession = Session <$> (newIORef =<< runGhc (Just libdir) (initGhcFlags >> getSession))

getModSumOrig :: ModSummary -> FilePath
getModSumOrig = normalise . fromMaybe (error "getModSumOrig: The given module doesn't have haskell source file.") . ml_hs_file . ms_location

data ClientMessage
  = KeepAlive
  | AddPackages { addedPathes :: [FilePath] }
  -- | RemovePackage { removedPathes :: [FilePath] }
  | PerformRefactoring { refactoring :: String
                       , modulePath :: FilePath
                       , editorSelection :: String
                       , details :: [String]
                       }
  | Stop
  | ReLoad { changedModules :: [FilePath] }
  deriving (Eq, Show, Generic)

instance ToJSON ClientMessage
instance FromJSON ClientMessage 

data ResponseMsg
  = KeepAliveResponse
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMsg :: String }
  | ModulesChanged { moduleChanges :: [FilePath] }
  | LoadedModules { loadedModules :: [FilePath] }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseMsg
instance FromJSON ResponseMsg 
