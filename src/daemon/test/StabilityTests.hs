{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception (SomeException(..), catch)
import Data.Aeson
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Socket hiding (KeepAlive, send, recv)
import Network.Socket.ByteString.Lazy as Sock (sendAll, recv)
import System.Directory
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import SrcLoc
import FastString

import Language.Haskell.Tools.Daemon
import Language.Haskell.Tools.Daemon.PackageDB (PackageDB(..))
import Language.Haskell.Tools.Daemon.Protocol (ClientMessage(..), ResponseMsg(..))
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)

main :: IO ()
main = defaultMain (testGroup "stability-tests" $ map makeStabilityTest stabilityTests)

stabilityTests :: [(String, IO [ClientMessage])]
stabilityTests =
  [ ( "simple-refactoring" -- make a total of 100 refactorings
    , do r <- canonicalizePath root
         return $ [SetPackageDB DefaultDB, AddPackages [r </> "CppHs"]]
                    ++ concat (replicate 50 [ mkRename r "SymbolTable", mkRename r "SymTab" ])
    )
  ]
  where root = ".." </> "cli" </> "examples"
        mkRename r name = PerformRefactoring "RenameDefinition" (r </> path)
                            "28:6" [name] False False
        path = "CppHs" </> "Language" </> "Preprocessor" </> "Cpphs" </> "SymTab.hs"

makeStabilityTest :: (String, IO [ClientMessage]) -> TestTree
makeStabilityTest (label, input) = testCase label $ runTest input

runTest :: IO [ClientMessage] -> IO ()
runTest calcMessages = withSocketsDo $ do
    forkIO $ runDaemon' builtinRefactorings daemonOpts
    addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just (show portNum))
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    waitToConnect sock (addrAddress serverAddr)
    msgs <- calcMessages
    mapM_ (\m -> sendAll sock (encode m `BS.snoc` '\n')) msgs
    sendAll sock (encode Disconnect)
    resps <- readSockResponsesUntil sock Disconnected BS.empty
    let correct = all correctResponse resps
    assertBool ("There was an erronous response in the results: " ++ show resps) correct
    close sock
  where waitToConnect sock addr
          = connect sock addr `catch` \(_ :: SomeException) -> threadDelay 10000 >> waitToConnect sock addr
        daemonOpts = DaemonOptions { daemonVersion = False, portNumber = portNum, silentMode = True
                                   , noWatch = True, watchExe = Nothing }
        portNum = 4123
        correctResponse ErrorMessage{} = False
        correctResponse CompilationProblem{} = False
        correctResponse _ = True

readSockResponsesUntil :: Socket -> ResponseMsg -> BS.ByteString -> IO [ResponseMsg]
readSockResponsesUntil sock rsp bs
  = do resp <- recv sock 2048
       let fullBS = bs `BS.append` resp
       if BS.null resp
         then return []
         else
           let splitted = BS.split '\n' fullBS
               recognized = catMaybes $ map decode splitted
            in if rsp `elem` recognized
                 then return $ takeWhile (/= rsp) recognized
                 else readSockResponsesUntil sock rsp fullBS

instance ToJSON ClientMessage
instance ToJSON PackageDB
instance FromJSON ResponseMsg
deriving instance Eq ResponseMsg

instance FromJSON SrcSpan where
    parseJSON (Object v) = mkSrcSpanReal <$> v .: "file"
                                         <*> v .: "startRow"
                                         <*> v .: "startCol"
                                         <*> v .: "endRow"
                                         <*> v .: "endCol"
    parseJSON _          = fail "not an object"

mkSrcSpanReal :: String -> Int -> Int -> Int -> Int -> SrcSpan
mkSrcSpanReal file startRow startCol endRow endCol
  = mkSrcSpan (mkSrcLoc (mkFastString file) startRow startCol)
              (mkSrcLoc (mkFastString file) endRow endCol)
