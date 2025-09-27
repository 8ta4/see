module Main (main) where

import Options.Applicative (execParser, strArgument)
import Options.Applicative.Builder (info)
import Relude
import System.Environment (getExecutablePath)
import System.FilePath (takeFileName, (</>))

main :: IO ()
main = do
  maybeDevenvRoot <- lookupEnv "DEVENV_ROOT"
  seePath <- getExecutablePath
  let hostPath = maybe seePath (</> "hs/bin") (guarded ((== "see") . takeFileName) =<< maybeDevenvRoot) </> "host"
  url <- execParser $ info (strArgument mempty) mempty
  putTextLn "Processing URL:"
  putTextLn url
