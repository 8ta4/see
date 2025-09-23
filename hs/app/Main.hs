module Main (main) where

import Options.Applicative (execParser, strArgument)
import Options.Applicative.Builder (info)
import Relude

main :: IO ()
main = do
  url <- execParser $ info (strArgument mempty) mempty
  putTextLn url
