module Main (main) where

import Relude
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)

main :: IO ()
main = do
  temporaryDirectory <- getTemporaryDirectory
  createDirectoryIfMissing False $ temporaryDirectory <> "see"
