module See (main) where

import Data.Aeson (KeyValue ((.=)), encode, object)
import Lib (createUnixSocket, getSocketPath)
import Network.Socket (SockAddr (SockAddrUnix), connect)
import Options.Applicative (execParser, helper, strArgument)
import Options.Applicative.Builder (info)
import Relude
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (takeFileName, (</>))

registerHost :: IO ()
registerHost = do
  maybeDevenvRoot <- lookupEnv "DEVENV_ROOT"
  seePath <- getExecutablePath
  let hostPath = maybe seePath (</> "hs/bin") (guarded ((== "see") <$> takeFileName) =<< maybeDevenvRoot) </> "host"
  homeDirectory <- getHomeDirectory
  let nativeMessagingHostsPath = homeDirectory </> "Library/Application Support/Mozilla/NativeMessagingHosts"
  createDirectoryIfMissing True nativeMessagingHostsPath
  writeFileLBS (nativeMessagingHostsPath </> "host.json")
    $ encode
    $ object
      [ "allowed_extensions" .= ["@see" :: Text],
        "description" .= ("" :: Text),
        "name" .= ("host" :: Text),
        "path" .= hostPath,
        "type" .= ("stdio" :: Text)
      ]

main :: IO ()
main = do
  url <- execParser $ info (strArgument mempty <**> helper) mempty
  putTextLn "Processing URL:"
  putTextLn url
  registerHost
  unixSocket <- createUnixSocket
  socketPath <- getSocketPath
  connect unixSocket $ SockAddrUnix socketPath
