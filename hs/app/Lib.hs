module Lib (createUnixSocket, getSocketPath) where

import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), defaultProtocol, socket)
import Relude
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

createUnixSocket :: IO Socket
createUnixSocket = socket AF_UNIX Stream defaultProtocol

getSocketPath :: IO FilePath
getSocketPath = do
  temporaryDirectory <- getTemporaryDirectory
  pure $ temporaryDirectory </> "see.sock"
