module Main where

import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Network.Socket hiding (KeepAlive, send, recv)
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as List
import Data.Aeson (encode, decode)
import Data.Maybe
import System.IO
import System.Directory

import Language.Haskell.Tools.Refactor.Daemon

main :: IO ()
main = do -- create one daemon process for the whole testing session
          -- with separate processes it is not a problem
          forkIO $ runDaemon ["4123", "True"]
          defaultMain allTests
          stopDaemon

allTests :: TestTree
allTests 
  = localOption (mkTimeout ({- 5s -} 1000 * 1000 * 5)) 
      $ testGroup "daemon-tests" 
          [ testGroup "simple-tests" 
              $ map (makeDaemonTest . (\(label, input, output) -> (Nothing, label, input, output))) simpleTests
          , testGroup "loading-tests" 
              $ map (makeDaemonTest . (\(label, input, output) -> (Nothing, label, input, output))) loadingTests
          , testGroup "refactor-tests" 
              $ map (makeDaemonTest . (\(label, input, output) -> (Just (testRoot </> label), label, input, output))) refactorTests
          ]

simpleTests :: [(String, [ClientMessage], [ResponseMsg])]
simpleTests = 
  [ ( "empty-test", [], [] )
  , ( "keep-alive", [KeepAlive], [KeepAliveResponse] )
  ]

loadingTests :: [(String, [ClientMessage], [ResponseMsg])]
loadingTests =
  [ ( "load-package"
    , [AddPackages [testRoot </> "has-cabal"]]
    , [LoadedModules [testRoot </> "has-cabal" </> "A.hs"]] )
  , ( "no-cabal"
    , [AddPackages [testRoot </> "no-cabal"]]
    , [LoadedModules [testRoot </> "no-cabal" </> "A.hs"]] )
  , ( "source-dir"
    , [AddPackages [testRoot </> "source-dir"]]
    , [LoadedModules [testRoot </> "source-dir" </> "src" </> "A.hs"]] )
  , ( "source-dir-outside"
    , [AddPackages [testRoot </> "source-dir-outside"]]
    , [LoadedModules [testRoot </> "source-dir-outside" </> ".." </> "src" </> "A.hs"]] )
  , ( "multi-packages"
    , [ AddPackages [ testRoot </> "multi-packages" </> "package1"
                    , testRoot </> "multi-packages" </> "package2" ]]
    , [ LoadedModules [ testRoot </> "multi-packages" </> "package1" </> "A.hs"
                      , testRoot </> "multi-packages" </> "package2" </> "B.hs"]] )
  , ( "multi-packages-flags"
    , [ AddPackages [ testRoot </> "multi-packages-flags" </> "package1"
                    , testRoot </> "multi-packages-flags" </> "package2" ]]
    , [ LoadedModules [ testRoot </> "multi-packages-flags" </> "package1" </> "A.hs"
                      , testRoot </> "multi-packages-flags" </> "package2" </> "B.hs"]] )
  , ( "multi-packages-dependent"
    , [ AddPackages [ testRoot </> "multi-packages-dependent" </> "package1"
                    , testRoot </> "multi-packages-dependent" </> "package2" ]]
    , [ LoadedModules [ testRoot </> "multi-packages-dependent" </> "package1" </> "A.hs"
                      , testRoot </> "multi-packages-dependent" </> "package2" </> "B.hs"]] )
  ]

refactorTests :: [(String, [ClientMessage], [ResponseMsg])]
refactorTests =
  [ ( "simple-refactor"
    , [ AddPackages [ testRoot </> "simple-refactor" ]
      , PerformRefactoring "RenameDefinition" (testRoot </> "simple-refactor" </> "A.hs") "3:1-3:2" ["y"]
      ]
    , [ LoadedModules [ testRoot </> "simple-refactor" </> "A.hs" ]
      , ModulesChanged [ testRoot </> "simple-refactor" </> "A.hs" ] 
      , LoadedModules [ testRoot </> "simple-refactor" </> "A.hs" ]
      ] )
  ]

makeDaemonTest :: (Maybe FilePath, String, [ClientMessage], [ResponseMsg]) -> TestTree
makeDaemonTest (Nothing, label, input, expected) = testCase label $ do  
    actual <- communicateWithDaemon input (length expected)
    assertEqual "" expected actual
makeDaemonTest (Just dir, label, input, expected) = testCase label $ do   
    copyDir dir (dir ++ "_orig")
    actual <- communicateWithDaemon input (length expected)
    assertEqual "" expected actual
  `finally` do removeDirectoryRecursive dir
               renameDirectory (dir ++ "_orig") dir

communicateWithDaemon :: [ClientMessage] -> Int -> IO [ResponseMsg]
communicateWithDaemon msgs numResps = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4123")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  forM msgs (sendAll sock . (`BS.snoc` '\n') . encode)
  resps <- replicateM numResps (recv sock 2048)
  close sock
  return $ map (\r -> fromMaybe (error $ "Response cannot be decoded: " ++ show (BS.unpack r)) $ decode r) resps

stopDaemon :: IO ()
stopDaemon = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4123")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)

  sendAll sock $ encode Stop
  close sock

testRoot = ".." </> ".." </> "examples" </> "Project"

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

longestCommonPrefix :: (Eq a) => [[a]] -> [a]
longestCommonPrefix = foldl1 commonPrefix