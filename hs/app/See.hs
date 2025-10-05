module See (main) where

import Data.Aeson (KeyValue ((.=)), decode, encode, object)
import Data.Binary.Put (putWord32le, runPut)
import Data.ByteString.Lazy (LazyByteString)
import Data.MonoTraversable.Unprefixed
import Lib (createUnixSocket, getSocketPath)
import Network.Socket (ShutdownCmd (ShutdownSend), SockAddr (SockAddrUnix), connect, shutdown)
import Network.Socket.ByteString.Lazy (getContents, sendAll)
import Options.Applicative (execParser, helper, strArgument)
import Options.Applicative.Builder (info)
import Relude hiding (length)
import Relude.Unsafe (fromJust)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, takeFileName, (</>))

name :: FilePath
name = "host"

registerHost :: IO ()
registerHost = do
  maybeDevenvRoot <- lookupEnv "DEVENV_ROOT"
  seePath <- getExecutablePath
  let hostPath = maybe (takeDirectory seePath) (</> "hs/bin") (guarded ((== "see") <$> takeFileName) =<< maybeDevenvRoot) </> "host"
  homeDirectory <- getHomeDirectory
  let nativeMessagingHostsPath = homeDirectory </> "Library/Application Support/Mozilla/NativeMessagingHosts"
  createDirectoryIfMissing True nativeMessagingHostsPath
  writeFileLBS (nativeMessagingHostsPath </> (name <> ".json"))
    $ encode
    $ object
      [ "allowed_extensions" .= ["@see" :: Text],
        "description" .= ("" :: Text),
        "name" .= name,
        "path" .= hostPath,
        "type" .= ("stdio" :: Text)
      ]

-- https://github.com/mdn/content/blob/4173f52767fe81e1dfe7ae373936a56b5abb50ea/files/en-us/mozilla/add-ons/webextensions/native_messaging/index.md?plain=1#L235
encodeNativeMessage :: Text -> LazyByteString
encodeNativeMessage = uncurry (<>) <$> (runPut <$> putWord32le <$> fromIntegral <$> length &&& id) <$> encode

main :: IO ()
main = do
  url <- execParser $ info (strArgument mempty <**> helper) mempty
  registerHost
  unixSocket <- createUnixSocket
  socketPath <- getSocketPath
  connect unixSocket $ SockAddrUnix socketPath
  sendAll unixSocket $ encodeNativeMessage url
  shutdown unixSocket ShutdownSend
  contents <- getContents unixSocket
  putTextLn $ fromJust $ decode contents
