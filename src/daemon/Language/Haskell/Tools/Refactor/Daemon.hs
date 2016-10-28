{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings 
           , DeriveGeneric
           , LambdaCase
           , TemplateHaskell
           #-}
module Language.Haskell.Tools.Refactor.Daemon where

import Network.WebSockets (ServerApp, Connection, DataMessage(..), ConnectionException, defaultConnectionOptions, acceptRequest, receiveDataMessage, sendTextData)
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types(status400)
import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy (ByteString)
import GHC.Generics

import Network.WebSockets
import Network.Wai.Handler.WebSockets
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Control.Exception
import Data.Aeson hiding ((.=))
import Data.Map (Map, (!), member, insert)
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
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
import Language.Haskell.Tools.ASTDebug
import Language.Haskell.Tools.ASTDebug.Instances
import Language.Haskell.Tools.PrettyPrint

type ClientId = Int

data RefactorSessionState
  = RefactorSessionState { _refSessMods :: Map.Map (String, String, IsBoot) (UnnamedModule IdDom)
                         }

makeReferences ''RefactorSessionState

initSession :: RefactorSessionState
initSession = RefactorSessionState Map.empty

runFromCLI :: IO ()
runFromCLI = getArgs >>= runDemo

runDemo :: [String] -> IO ()
runDemo args = do
  let settings = setPort 8206 $ setTimeout 20 $ defaultSettings 
  runSettings settings app

-- | The application that is evoked for each incoming request
app :: Application
app = websocketsOr defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp conn = do
        conn <- acceptRequest conn
        ghcSess <- initGhcSession
        state <- newMVar initSession
        serverLoop ghcSess state conn

    serverLoop :: Session -> MVar RefactorSessionState -> Connection -> IO ()
    serverLoop ghcSess state conn =
        do Text msg <- receiveDataMessage conn
           respondTo ghcSess state (sendTextData conn) msg
           serverLoop ghcSess state conn

    backupApp :: Application
    backupApp req respond = respond $ responseLBS status400 [] "Not a WebSocket request"

respondTo :: Session -> MVar RefactorSessionState -> (ByteString -> IO ()) -> ByteString -> IO ()
respondTo ghcSess state next mess = case decode mess of
  Nothing -> next $ encode $ ErrorMessage "WRONG MESSAGE FORMAT"
  Just req -> do resp <- modifyMVar state (\st -> swap <$> reflectGhc (runStateT (updateClient req) st) ghcSess)
                 case resp of Just respMsg -> next $ encode respMsg
                              Nothing -> return ()

-- | This function does the real job of acting upon client messages in a stateful environment of a client
updateClient :: ClientMessage -> StateT RefactorSessionState Ghc (Maybe ResponseMsg)
updateClient KeepAlive = return Nothing
updateClient (AddPackage packagePathes) = do 
    modules <- liftIO $ (concat . map snd . concat) <$> mapM getModules packagePathes
    lift $ useDirs packagePathes
    lift $ mapM_ addTarget (map ((\modName -> Target (TargetModule (GHC.mkModuleName modName)) True Nothing)) modules)
    lift $ load LoadAllTargets
    forM modules $ \modName -> do
      mod <- lift $ getModSummary (GHC.mkModuleName modName) >>= parseTyped
      modify $ refSessMods .- Map.insert (dir, modName, NormalHs) mod
    return Nothing
updateClient ReLoad = do
    -- TODO: find out which modules have changed
    lift $ load LoadAllTargets
    -- mod <- lift $ getModSummary (GHC.mkModuleName name) >>= parseTyped
    -- modify $ refSessMods .- Map.insert (dir, name, NormalHs) mod
    return Nothing
-- updateClient _ Stop = return () -- TODO: exit

-- TODO: handle boot files

updateClient (PerformRefactoring refact modName selection args) = do
    mod <- gets (find ((modName ==) . (\(_,m,_) -> m) . fst) . Map.assocs . (^. refSessMods))
    allModules <- gets (map moduleNameAndContent . Map.assocs . (^. refSessMods))
    let command = analyzeCommand (toFileName dir modName) refact (selection:args)
    case mod of Just m -> do res <- lift $ performCommand command (moduleNameAndContent m) allModules 
                             case res of
                               Left err -> return $ Just $ ErrorMessage err
                               Right diff -> do applyChanges diff
                                                return $ Just $ RefactorChanges (map trfDiff diff)
                Nothing -> return $ Just $ ErrorMessage "The module is not found"
  where trfDiff (ContentChanged (name,cont)) = (name, Just (prettyPrint cont))
        trfDiff (ModuleRemoved name) = (name, Nothing)

        applyChanges 
          = mapM_ $ \case 
              ContentChanged (n,m) -> do
                liftIO $ withBinaryFile (toFileName dir n)
                                        WriteMode (`hPutStr` prettyPrint m)
                w <- gets (find ((n ==) . (\(_,m,_) -> m)) . Map.keys . (^. refSessMods))
                newm <- lift $ (parseTyped =<< loadModule dir n)
                modify $ refSessMods .- Map.insert (dir, n, NormalHs) newm
              ModuleRemoved mod -> do
                modify $ refSessMods .- Map.delete (dir, mod, NormalHs)
                liftIO $ removeFile (toFileName dir mod)

-- TODO: remove
dir = "unknown-dir"

createFileForModule :: FilePath -> String -> String -> IO ()
createFileForModule dir name newContent = do
  let fname = toFileName dir name
  createDirectoryIfMissing True (takeDirectory fname)
  withBinaryFile fname WriteMode (`hPutStr` newContent) 

removeDirectoryIfPresent :: FilePath -> IO ()
removeDirectoryIfPresent dir = removeDirectoryRecursive dir `catch` \e -> if isDoesNotExistError e then return () else throwIO e

moduleNameAndContent :: ((String,String,IsBoot), mod) -> (String, mod)
moduleNameAndContent ((_,name,_), mod) = (name, mod)

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
  = RefactorChanges { moduleChanges :: [(String, Maybe String)] }
  | ASTViewContent { astContent :: String }
  | ErrorMessage { errorMsg :: String }
  | CompilationProblem { errorMsg :: String }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseMsg
instance FromJSON ResponseMsg 
