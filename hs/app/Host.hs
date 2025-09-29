module Host (main) where

import Control.Exception (catch, throwIO)
import Lib (createUnixSocket, getSocketPath)
import Network.Socket (SockAddr (SockAddrUnix), listen)
import Network.Socket.Address (bind)
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

main :: IO ()
main = do
  unixSocket <- createUnixSocket
  socketPath <- getSocketPath
  removeIfExists socketPath
  bind unixSocket $ SockAddrUnix socketPath
  listen unixSocket 1
