module Main (main) where

import Data.Aeson (KeyValue ((.=)), encode, object)
import Data.Text qualified as T
import Lib (createUnixSocket)
import Options.Applicative (execParser, strArgument)
import Options.Applicative.Builder (info)
import Relude
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (takeFileName, (</>))

registerHost :: IO ()
registerHost = do
  maybeDevenvRoot <- lookupEnv "DEVENV_ROOT"
  seePath <- getExecutablePath
  let hostPath = maybe seePath (</> "hs/bin") (guarded ((== "see") . takeFileName) =<< maybeDevenvRoot) </> "host"
  homeDirectory <- getHomeDirectory
  let nativeMessagingHostsPath = homeDirectory </> "Library/Application Support/Mozilla/NativeMessagingHosts"
  createDirectoryIfMissing True nativeMessagingHostsPath
  writeFileLBS (nativeMessagingHostsPath </> "host.json")
    $ encode
    $ object
      [ "allowed_extensions" .= ["@see" :: T.Text],
        "description" .= ("" :: T.Text),
        "name" .= ("host" :: T.Text),
        "path" .= hostPath,
        "type" .= ("stdio" :: T.Text)
      ]

main :: IO ()
main = do
  url <- execParser $ info (strArgument mempty) mempty
  putTextLn "Processing URL:"
  putTextLn url
  registerHost
  unixSocket <- createUnixSocket
  pure ()
