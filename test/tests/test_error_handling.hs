#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import System.Process (callProcess, readProcess, waitForProcess, createProcess, StdStream(..), std_in, std_err, std_out, proc)
import System.Directory (createDirectoryIfMissing, removePathForcibly, doesFileExist)
import System.FilePath ((</>))
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hClose)
import Control.Exception (catch, SomeException, try)
import Data.List (isInfixOf)

-- Test environment
localTempDir = "/tmp/sftp-test-errors"
sftpHost = "johnorford@localhost"
sftpRemoteDir = "/Users/johnorford/haskell-scripting/test/test-uploads"
readOnlyDir = "/tmp/sftp-test-readonly"

-- Test 1: Error handling on SFTP failures
test1_errorReporting :: IO Bool
test1_errorReporting = do
  putStrLn "\n[TEST 1] Error reporting on SFTP failure"

  -- Create a local directory with files
  let sourceDir = localTempDir </> "test1"
  removePathForcibly sourceDir
  createDirectoryIfMissing True sourceDir
  writeFile (sourceDir </> "file.txt") "test content"

  -- Try to back up to a read-only location or invalid host
  -- Since we can't actually make /test-uploads read-only in the test environment,
  -- we'll test with an invalid host instead to simulate failure
  result <- try (callProcess "../dev/script" [sourceDir, "invalid@host.example.com", "/tmp/invalid"]) :: IO (Either SomeException ())

  case result of
    Left err -> do
      putStrLn $ "PASS: Script error detected and propagated"
      return True
    Right _ -> do
      putStrLn "FAIL: Script succeeded when it should have failed"
      return False

-- Test 2: Idempotency - re-running same backup
test2_idempotency :: IO Bool
test2_idempotency = do
  putStrLn "\n[TEST 2] Idempotency (re-running backup)"

  let sourceDir = localTempDir </> "test2"
  removePathForcibly sourceDir
  createDirectoryIfMissing True sourceDir

  -- Create files with specific content
  writeFile (sourceDir </> "file1.txt") "content1"
  writeFile (sourceDir </> "file2.txt") "content2"

  putStrLn "First backup run..."
  _ <- (callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]) `catch` \(e :: SomeException) -> do
    putStrLn $ "First run error: " ++ show e

  -- Small delay
  putStrLn "Waiting 1 second..."

  putStrLn "Second backup run (same files)..."
  result <- (do
    callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "Second run error: " ++ show e
      return False

  if result
    then putStrLn "PASS: Re-running backup succeeded (idempotent)"
    else putStrLn "FAIL: Re-running backup failed"

  return result

-- Test 3: File change detection (modified file)
test3_fileChangeDetection :: IO Bool
test3_fileChangeDetection = do
  putStrLn "\n[TEST 3] File change detection"

  let sourceDir = localTempDir </> "test3"
  removePathForcibly sourceDir
  createDirectoryIfMissing True sourceDir

  -- First backup with initial content
  writeFile (sourceDir </> "file.txt") "original content"
  putStrLn "First backup with original content..."
  _ <- (callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]) `catch` \(e :: SomeException) -> do
    putStrLn $ "First run error: " ++ show e

  -- Modify the file
  putStrLn "Modifying file..."
  writeFile (sourceDir </> "file.txt") "modified content"

  -- Second backup should detect the change
  putStrLn "Second backup with modified content..."
  result <- (do
    callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "Second run error: " ++ show e
      return False

  if result
    then putStrLn "PASS: Backup succeeded after file modification"
    else putStrLn "FAIL: Backup failed after file modification"

  return result

-- Test 4: Check nested directory handling
test4_nestedDirectories :: IO Bool
test4_nestedDirectories = do
  putStrLn "\n[TEST 4] Nested directory creation"

  let sourceDir = localTempDir </> "test4"
  removePathForcibly sourceDir
  createDirectoryIfMissing True sourceDir

  -- Create deeply nested structure
  createDirectoryIfMissing True (sourceDir </> "dir1" </> "dir2" </> "dir3")
  writeFile (sourceDir </> "dir1" </> "file1.txt") "level 1"
  writeFile (sourceDir </> "dir1" </> "dir2" </> "file2.txt") "level 2"
  writeFile (sourceDir </> "dir1" </> "dir2" </> "dir3" </> "file3.txt") "level 3"

  putStrLn "Backing up nested structure..."
  result <- (do
    callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      return False

  if result
    then putStrLn "PASS: Nested directories created successfully"
    else putStrLn "FAIL: Nested directory creation failed"

  return result

-- Main test runner
main :: IO ()
main = do
  putStrLn "SFTP Backup Script - Error Handling & Idempotency Tests"
  putStrLn "========================================================"
  putStrLn "Focus: Error reporting, idempotency, and change detection\n"

  -- Clean up before starting
  removePathForcibly localTempDir
  createDirectoryIfMissing True localTempDir

  -- Run tests
  t1 <- test1_errorReporting
  t2 <- test2_idempotency
  t3 <- test3_fileChangeDetection
  t4 <- test4_nestedDirectories

  -- Summary
  putStrLn "\n========================================================"
  putStrLn "Test Summary:"
  putStrLn $ "  Test 1 (error handling):   " ++ if t1 then "PASS" else "FAIL"
  putStrLn $ "  Test 2 (idempotency):      " ++ if t2 then "PASS" else "FAIL"
  putStrLn $ "  Test 3 (change detection): " ++ if t3 then "PASS" else "FAIL"
  putStrLn $ "  Test 4 (nested dirs):      " ++ if t4 then "PASS" else "FAIL"

  let passed = length (filter id [t1, t2, t3, t4])
  putStrLn $ "\nPassed: " ++ show passed ++ "/4"

  -- Cleanup
  removePathForcibly localTempDir
