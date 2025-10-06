module See (main) where

import Data.Aeson (KeyValue ((.=)), Object, Value (Object), decode, encode)
import Data.Aeson.KeyMap qualified as KeyMap
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
import System.FilePath (takeDirectory, takeFileName, (<.>), (</>))

main :: IO ()
main = do
  url <- execParser $ info (strArgument mempty <**> helper) mempty
  registerHost "Library/Application Support/Google/Chrome/NativeMessagingHosts" $ KeyMap.fromList ["allowed_origins" .= ["chrome-extension://aobaoadfgfpeggekafmdlmgdondfnpdo/" :: Text]]
  registerHost "Library/Application Support/Mozilla/NativeMessagingHosts" $ KeyMap.fromList ["allowed_extensions" .= ["@see" :: Text]]
  unixSocket <- createUnixSocket
  socketPath <- getSocketPath
  connect unixSocket $ SockAddrUnix socketPath
  sendAll unixSocket $ encodeNativeMessage url
  shutdown unixSocket ShutdownSend
  contents <- getContents unixSocket
  putTextLn $ fromJust $ decode contents

registerHost :: FilePath -> Object -> IO ()
registerHost hostsPathSegment config = do
  maybeDevenvRoot <- lookupEnv "DEVENV_ROOT"
  seePath <- getExecutablePath
  let hostPath = maybe (takeDirectory seePath) (</> "hs/bin") (guarded ((== "see") <$> takeFileName) =<< maybeDevenvRoot) </> name
  homeDirectory <- getHomeDirectory
  let nativeMessagingHostsPath = homeDirectory </> hostsPathSegment
  createDirectoryIfMissing True nativeMessagingHostsPath
  writeFileLBS (nativeMessagingHostsPath </> (name <.> "json"))
    $ encode
    $ Object
    $ config
    <> KeyMap.fromList
      [ "description" .= ("Enables communication between the browser extension and the CLI." :: Text),
        "name" .= name,
        "path" .= hostPath,
        "type" .= ("stdio" :: Text)
      ]

name :: FilePath
name = "host"

-- https://github.com/mdn/content/blob/4173f52767fe81e1dfe7ae373936a56b5abb50ea/files/en-us/mozilla/add-ons/webextensions/native_messaging/index.md?plain=1#L235
encodeNativeMessage :: Text -> LazyByteString
encodeNativeMessage = uncurry (<>) <$> (runPut <$> putWord32le <$> fromIntegral <$> length &&& id) <$> encode
