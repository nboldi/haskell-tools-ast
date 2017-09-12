{-# LANGUAGE TupleSections #-}
-- | Defines different working modes for the daemon. It can work by using a socket connection
-- or channels to communicate with the client. When the daemon is used by CLI, it uses channel if
-- it communicates with an editor plugin it uses the socket connection.
module Language.Haskell.Tools.Daemon.Mode where

import Control.Concurrent.Chan
import qualified Data.Aeson as A ()
import Data.Aeson hiding ((.=))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (Maybe(..), catMaybes)
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy (sendAll, recv)

import Language.Haskell.Tools.Daemon.Protocol (ResponseMsg, ClientMessage)

-- | An abstraction over the connection to the client.
data WorkingMode a = WorkingMode { daemonConnect :: Int -> IO a -- TODO: could we generalize this Int parameter nicely?
                                 , daemonDisconnect :: a -> IO ()
                                 , daemonSend :: a -> ResponseMsg -> IO ()
                                 , daemonReceive :: ByteString -> a -> IO ([Either String ClientMessage], ByteString)
                                 }

-- | Connect to the client running in a separate process using socket connection
socketMode :: WorkingMode (Socket,Socket)
socketMode = WorkingMode sockConn sockDisconnect sockSend sockReceive
  where
    sockConn portNumber = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      bind sock (SockAddrInet (read $ show portNumber) iNADDR_ANY)
      listen sock 1
      (conn, _) <- accept sock
      return (sock,conn)
    sockDisconnect (sock,conn) = close conn >> close sock
    sockSend (_,conn) = sendAll conn . (`BS.snoc` '\n') . encode
    sockReceive rem (_,conn) = do
      msg <- recv conn 2048
      if not $ BS.null msg -- null on TCP means closed connection
        then do -- when (not isSilent) $ putStrLn $ "message received: " ++ show (unpack msg)
          let msgs = BS.split '\n' (rem `BS.append` msg)
              decoded = catMaybes $ map decodeMsg msgs
           in return $ case reverse decoded of
                         Left msg : _ -> (init decoded, last msgs)
                         _ -> (decoded, BS.empty)
        else return ([], BS.empty)
      where decodeMsg :: ByteString -> Maybe (Either String ClientMessage)
            decodeMsg mess
              | BS.null mess = Nothing
              | otherwise = case decode mess of
                Nothing -> Just $ Left $ unpack mess
                Just req -> Just $ Right req

-- | Connect to the client running in the same process using a channel
channelMode :: WorkingMode (Chan ResponseMsg, Chan ClientMessage)
channelMode = WorkingMode chanConn chanDisconnect chanSend chanReceive
  where
    chanConn _ = (,) <$> newChan <*> newChan
    chanDisconnect _ = return ()
    chanSend (send,_) resp = writeChan send resp
    chanReceive _ (_,recv) = (,BS.empty) . (:[]) . Right <$> readChan recv
