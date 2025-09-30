module Host (main) where

import Control.Exception (bracket, catch, throwIO)
import Data.ByteString.Lazy (hPut)
import Lib (createUnixSocket, getSocketPath)
import Network.Socket (SockAddr (SockAddrUnix), Socket, accept, close, listen)
import Network.Socket.Address (bind)
import Network.Socket.ByteString.Lazy (getContents)
import Relude
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

-- https://stackoverflow.com/a/8502391
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = pure ()
      | otherwise = throwIO e

serveClient :: Socket -> IO ()
serveClient socket = do
  contents <- getContents socket
  hPut stdout contents

main :: IO ()
main = do
  unixSocket <- createUnixSocket
  socketPath <- getSocketPath
  removeIfExists socketPath
  bind unixSocket $ SockAddrUnix socketPath
  listen unixSocket 1
  bracket (fst <$> accept unixSocket) close serveClient
