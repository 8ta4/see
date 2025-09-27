module Main (main) where

import Data.Aeson (KeyValue ((.=)), encode, object)
import Options.Applicative (execParser, strArgument)
import Options.Applicative.Builder (info)
import Relude
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getExecutablePath)
import System.FilePath (takeFileName, (</>))

main :: IO ()
main = do
  maybeDevenvRoot <- lookupEnv "DEVENV_ROOT"
  seePath <- getExecutablePath
  let hostPath = maybe seePath (</> "hs/bin") (guarded ((== "see") . takeFileName) =<< maybeDevenvRoot) </> "host"
  homeDirectory <- getHomeDirectory
  let nativeMessagingHostsPath = homeDirectory </> "Library/Application Support/Mozilla/NativeMessagingHosts"
  createDirectoryIfMissing True nativeMessagingHostsPath
  writeFileLBS (nativeMessagingHostsPath </> "see.json") $ encode $ object ["path" .= hostPath]
  url <- execParser $ info (strArgument mempty) mempty
  putTextLn "Processing URL:"
  putTextLn url
