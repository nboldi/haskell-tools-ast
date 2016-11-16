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
import Control.Monad
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Monad.IO.Class
import System.Environment

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
import Language.Haskell.Tools.PrettyPrint

-- TODO: find out which modules have changed
-- TODO: exit
-- TODO: handle boot files
-- TODO: handle multiple modules in different packages with the same module name



data RefactorSessionState
  = RefactorSessionState { _refSessMods :: Map.Map (String, IsBoot) (UnnamedModule IdDom)
                         }

makeReferences ''RefactorSessionState

initSession :: RefactorSessionState
initSession = RefactorSessionState Map.empty

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
       clientLoop sock

serverLoop :: Session -> MVar RefactorSessionState -> Socket -> IO ()
serverLoop ghcSess state sock =
    do msg <- recv sock 1024
       putStrLn $ "message received: " ++ unpack msg
       respondTo ghcSess state (sendAll sock) msg
       serverLoop ghcSess state sock
  `catch` interrupted sock
  where interrupted = \s ex -> do
                        let err = show (ex :: IOException)
                        putStrLn "Closing down socket"
                        hPutStrLn stderr $ "Some exception caught: " ++ err

respondTo :: Session -> MVar RefactorSessionState -> (ByteString -> IO ()) -> ByteString -> IO ()
respondTo ghcSess state next mess = case decode mess of
  Nothing -> next $ encode $ ErrorMessage "WRONG MESSAGE FORMAT"
  Just req -> do resp <- modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient req) st) ghcSess)
                 case resp of Just respMsg -> next $ encode respMsg
                              Nothing -> return ()

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: ClientMessage -> StateT RefactorSessionState Ghc (Maybe ResponseMsg)
updateClient KeepAlive = return $ Just KeepAliveResponse
updateClient (AddPackage packagePathes) = do 
    modules <- liftIO $ (concat . map snd . concat) <$> mapM getModules packagePathes
    liftIO $ putStrLn (show modules)
    lift $ useDirs packagePathes
    lift $ mapM_ addTarget (map ((\modName -> Target (TargetModule (GHC.mkModuleName modName)) True Nothing)) modules)
    lift $ load LoadAllTargets
    forM modules $ \modName -> do
      mod <- lift $ getModSummary (GHC.mkModuleName modName) >>= parseTyped
      modify $ refSessMods .- Map.insert (modName, NormalHs) mod
    return (Just $ LoadedModules modules)
updateClient ReLoad = do
    lift $ load LoadAllTargets
    -- mod <- lift $ getModSummary (GHC.mkModuleName name) >>= parseTyped
    -- modify $ refSessMods .- Map.insert (name, NormalHs) mod
    return Nothing
-- updateClient _ Stop = return () 

updateClient (PerformRefactoring refact modName selection args) = do
    mod <- gets ((Map.lookup (modName, NormalHs)) . (^. refSessMods))
    allModules <- gets (map moduleNameAndContent . Map.assocs . (^. refSessMods))
    let command = analyzeCommand refact (selection:args)
    case mod of Just m -> do res <- lift $ performCommand command (modName,m) allModules 
                             case res of
                               Left err -> return $ Just $ ErrorMessage err
                               Right diff -> do applyChanges diff
                                                return $ Just $ RefactorChanges (map trfDiff diff)
                Nothing -> return $ Just $ ErrorMessage "The module is not found"
  where trfDiff (ContentChanged (name,_)) = name
        trfDiff (ModuleRemoved name) = name

        applyChanges 
          = mapM_ $ \case 
              ContentChanged (n,m) -> do
                filePath <- getFilePath n
                liftIO $ withBinaryFile filePath WriteMode (`hPutStr` prettyPrint m)
                newm <- lift $ (parseTyped =<< getModSummary (mkModuleName n))
                modify $ refSessMods .- Map.insert (n, NormalHs) newm
              ModuleRemoved mod -> do
                modify $ refSessMods .- Map.delete (mod, IsHsBoot) . Map.delete (mod, NormalHs)
                liftIO . removeFile =<< getFilePath mod

getFilePath :: String -> StateT RefactorSessionState Ghc FilePath
getFilePath modName = 
  lift . getModuleFilePath =<< gets ((!(modName, NormalHs)) . (^. refSessMods))

getModuleFilePath :: UnnamedModule IdDom -> Ghc FilePath
getModuleFilePath mod = do
  let modName = GHC.moduleName $ semanticsModule mod 
  ms <- getModSummary modName
  return $ fromMaybe (error "Module location not found: " ++ GHC.moduleNameString modName) $ ml_hs_file $ ms_location ms

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
initGhcSession = Session <$> (newIORef =<< runGhc (Just libdir) (do 
    dflags <- getSessionDynFlags
    -- don't generate any code
    setSessionDynFlags 
      $ flip gopt_set Opt_KeepRawTokenStream
      $ flip gopt_set Opt_NoHsMain
      $ dflags { importPaths = []
               , hscTarget = HscAsm -- needed for static pointers
               , ghcLink = LinkInMemory
               , ghcMode = CompManager 
               }
    getSession))

data ClientMessage
  = KeepAlive
  | AddPackage { addedPathes :: [FilePath] }
  -- | RemovePackage { removedPathes :: [FilePath] }
  | PerformRefactoring { refactoring :: String
                       , moduleName :: String
                       , editorSelection :: String
                       , details :: [String]
                       }
  | ReLoad
  -- | Stop
  deriving (Eq, Show, Generic)

instance ToJSON ClientMessage
instance FromJSON ClientMessage 

data ResponseMsg
  = KeepAliveResponse
  | RefactorChanges { moduleChanges :: [String] }
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMsg :: String }
  | LoadedModules { loadedModules :: [String] }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseMsg
instance FromJSON ResponseMsg 
