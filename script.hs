#!/usr/bin/env runhaskell

import System.Process (readProcess, callProcess)
import System.Directory (getModificationTime)
import Data.List (sortOn)
import Control.Monad ( void)
import Data.Foldable (for_, traverse_)
import Data.Traversable (for)

run :: String -> [String] -> IO String
run cmd args = readProcess cmd args ""

run_ :: String -> [String] -> IO ()
run_ cmd args = void $ callProcess cmd args

-- find: produces paths
find :: FilePath -> String -> IO [FilePath]
find dir pattern = do
  out <- run "find" [dir, "-name", pattern]
  pure $ lines out

-- sort by mtime (oldest first)
sortByAge :: [FilePath] -> IO [FilePath]
sortByAge files = do
  withTimes <- for files $ \f -> do
    t <- getModificationTime f
    pure (t, f)
  pure $ map snd $ sortOn fst withTimes

uploadAll :: [FilePath] -> IO ()
uploadAll = traverse_ upload'

upload' :: FilePath -> IO ()
upload' f = upload "user@host" f "/remote/path/"

upload :: String -> FilePath -> FilePath -> IO ()
upload host local remoteDir =
  run_ "scp" [local, host <> ":" <> remoteDir]

main = do
  find "." "*bin.gz"
    >>= sortByAge
    >>= uploadAll
