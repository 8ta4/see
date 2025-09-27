module Main (main) where

import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), defaultProtocol, socket)
import Network.Socket.Address (bind)
import Relude
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

main :: IO ()
main = do
  temporaryDirectory <- getTemporaryDirectory
  unixSocket <- socket AF_UNIX Stream defaultProtocol
  bind unixSocket $ SockAddrUnix $ temporaryDirectory </> "see.sock"
