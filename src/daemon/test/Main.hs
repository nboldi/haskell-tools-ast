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
import qualified Data.List as List
import Data.Aeson (encode, decode)
import Data.Maybe
import System.IO
import System.Directory

import Language.Haskell.Tools.Refactor.Daemon

main :: IO ()
main = do forkIO $ runDaemon ["4123", "True"]
          defaultMain allTests
          stopDaemon

allTests :: TestTree
allTests = localOption (mkTimeout ({- 1s -} 1000 * 1000)) 
             $ testGroup "daemon-tests" $ map makeDaemonTest daemonTests

daemonTests :: [([FilePath], [ClientMessage], [ResponseMsg])]
daemonTests 
  = [ ( [testRoot </> "has-cabal"], [], [] )
    , ( [testRoot </> "has-cabal"], [KeepAlive], [KeepAliveResponse] )
    ]

makeDaemonTest :: ([FilePath], [ClientMessage], [ResponseMsg]) -> TestTree
makeDaemonTest (dirs, input, expected) = let dir = joinPath $ longestCommonPrefix $ map splitDirectories dirs
  in testCase dir $ do   
    copyDir dir (dir ++ "_orig")
    terminated <- newEmptyMVar 
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

  forM msgs $ send sock . encode
  resps <- replicateM numResps (recv sock 2048)
  close sock
  return $ map (fromMaybe (error "Response cannot be decoded") . decode) resps

stopDaemon :: IO ()
stopDaemon = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4123")
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)

  send sock $ encode Stop
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