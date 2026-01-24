#!/usr/bin/env runhaskell

import Backup (genSftpCommands)
import System.Process (readProcess, callProcess, createProcess, StdStream(..), std_in, waitForProcess, proc)
import System.Environment (getArgs)
import System.IO (hPutStr, hPutStrLn, hClose, stderr)
import System.Exit (exitWith, ExitCode(..))
import Data.List (intercalate)
import Control.Monad (void)

-- Execute command and capture output
run :: String -> [String] -> IO String
run command args = readProcess command args ""

-- Find all files (not directories) under a path
findAllFiles :: FilePath -> IO [FilePath]
findAllFiles path = do
  output <- run "find" [path, "-type", "f"]
  pure $ filter (not . null) $ lines output

-- Execute SFTP with batch commands piped to stdin
executeSftp :: String -> [String] -> IO ()
executeSftp host commands = do
  let cmdStr = intercalate "\n" commands ++ "\nquit\n"
  (Just hIn, _, _, process) <- createProcess (proc "sftp" [host])
    { std_in = CreatePipe }
  hPutStr hIn cmdStr
  hClose hIn
  exitCode <- waitForProcess process
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      hPutStrLn stderr $ "SFTP failed with exit code " ++ show code
      exitWith (ExitFailure code)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [sourceDir, remoteHost, remoteDir] -> do
      putStrLn $ "Backing up " ++ sourceDir ++ " -> " ++ remoteHost ++ ":" ++ remoteDir

      files <- findAllFiles sourceDir
      if null files
        then putStrLn "No files found."
        else do
          putStrLn $ "Found " ++ show (length files) ++ " files"

          let commands = genSftpCommands remoteDir sourceDir files
          putStrLn $ "Executing " ++ show (length commands) ++ " SFTP commands..."

          executeSftp remoteHost commands
          putStrLn "Backup complete."
    _ -> putStrLn "Usage: ./script.hs <source-dir> <user@host> <remote-dir>"
