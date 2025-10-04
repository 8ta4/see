module Host (main) where

import Control.Exception (bracket, catch, throwIO)
import Data.Binary.Get (getWord32le, runGet)
import Data.ByteString.Lazy (hGet, hPut)
import Lib (createUnixSocket, getSocketPath)
import Network.Socket (SockAddr (SockAddrUnix), Socket, accept, close, listen)
import Network.Socket.Address (bind)
import Network.Socket.ByteString.Lazy (getContents, sendAll)
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
  hFlush stdout
  prefix <- hGet stdin 4
  message <- hGet stdin $ fromIntegral $ runGet getWord32le prefix
  sendAll socket message

main :: IO ()
main = do
  unixSocket <- createUnixSocket
  socketPath <- getSocketPath
  removeIfExists socketPath
  bind unixSocket $ SockAddrUnix socketPath
  listen unixSocket 1
  forever $ bracket (fst <$> accept unixSocket) close serveClient
