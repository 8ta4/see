module Lib (createUnixSocket) where

import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), defaultProtocol, socket)
import Relude

createUnixSocket :: IO Socket
createUnixSocket = socket AF_UNIX Stream defaultProtocol
