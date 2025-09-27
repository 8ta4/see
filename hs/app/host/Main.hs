module Main (main) where

import Control.Exception (catch, throwIO)
import Lib (createUnixSocket)
import Network.Socket (SockAddr (SockAddrUnix), listen)
import Network.Socket.Address (bind)
import Relude
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
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
  temporaryDirectory <- getTemporaryDirectory
  let socketPath = temporaryDirectory </> "see.sock"
  removeIfExists socketPath
  bind unixSocket $ SockAddrUnix socketPath
  listen unixSocket 1
