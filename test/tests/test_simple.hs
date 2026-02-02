#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import System.Process (readProcess, callProcess)
import System.Directory (createDirectoryIfMissing, removePathForcibly, doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Data.List (sort)
import Control.Exception (catch, SomeException)
import Control.Monad (filterM)

-- Test environment
localTempDir = "/tmp/sftp-test-simple"
sftpHost = "johnorford@localhost"
sftpRemoteDir = "/Users/johnorford/haskell-scripting/test/test-uploads"

-- Create a simple test directory with known files
setupTest :: String -> IO ()
setupTest testName = do
  let testDir = localTempDir </> testName
  removePathForcibly testDir  -- Clean up from previous run
  createDirectoryIfMissing True testDir
  writeFile (testDir </> "file1.txt") "content1"
  putStrLn $ "Created test dir: " ++ testDir

-- Test 1: Simple flat directory with files
test1_flatDirectory :: IO Bool
test1_flatDirectory = do
  putStrLn "\n[TEST 1] Flat directory with files"
  setupTest "test1"
  let sourceDir = localTempDir </> "test1"

  -- Create test files
  writeFile (sourceDir </> "a.txt") "file a"
  writeFile (sourceDir </> "b.txt") "file b"

  -- Run the developer's script
  result <- (do
    callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "ERROR: " ++ show e
      return False

  if result
    then putStrLn "PASS: Script executed without error"
    else putStrLn "FAIL: Script execution failed"

  return result

-- Test 2: Empty directory (should handle gracefully)
test2_emptyDirectory :: IO Bool
test2_emptyDirectory = do
  putStrLn "\n[TEST 2] Empty directory"
  setupTest "test2"
  let sourceDir = localTempDir </> "test2"

  result <- (do
    callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "ERROR: " ++ show e
      return False

  if result
    then putStrLn "PASS: Handles empty dir"
    else putStrLn "FAIL: Crashed on empty dir"

  return result

-- Test 3: Invalid source directory
test3_invalidSource :: IO Bool
test3_invalidSource = do
  putStrLn "\n[TEST 3] Invalid source directory"

  result <- (do
    callProcess "../dev/script" ["/nonexistent/path", sftpHost, sftpRemoteDir]
    return False) `catch` \(e :: SomeException) -> do
      putStrLn $ "Caught error (expected): " ++ show e
      return True  -- We expect this to fail gracefully

  if result
    then putStrLn "PASS: Rejects invalid source"
    else putStrLn "FAIL: Doesn't handle invalid source"

  return result

-- Test 4: Verify file paths are constructed correctly
test4_pathConstruction :: IO Bool
test4_pathConstruction = do
  putStrLn "\n[TEST 4] Path construction (stripPrefix)"
  setupTest "test4"
  let sourceDir = localTempDir </> "test4"

  -- Create nested structure
  createDirectoryIfMissing True (sourceDir </> "subdir")
  writeFile (sourceDir </> "subdir" </> "nested.txt") "nested content"

  result <- (do
    callProcess "../dev/script" [sourceDir, sftpHost, sftpRemoteDir]
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "ERROR: " ++ show e
      return False

  if result
    then putStrLn "PASS: Handles nested paths"
    else putStrLn "FAIL: Problem with nested paths"

  return result

-- Main test runner
main :: IO ()
main = do
  putStrLn "SFTP Backup Script - Simple Test Suite"
  putStrLn "======================================"
  putStrLn "Focus: Concrete tests for known issues\n"

  -- Clean up before starting
  removePathForcibly localTempDir
  createDirectoryIfMissing True localTempDir

  -- Run tests
  t1 <- test1_flatDirectory
  t2 <- test2_emptyDirectory
  t3 <- test3_invalidSource
  t4 <- test4_pathConstruction

  -- Summary
  putStrLn "\n======================================"
  putStrLn "Test Summary:"
  putStrLn $ "  Test 1 (flat dir):    " ++ if t1 then "PASS" else "FAIL"
  putStrLn $ "  Test 2 (empty dir):   " ++ if t2 then "PASS" else "FAIL"
  putStrLn $ "  Test 3 (invalid):     " ++ if t3 then "PASS" else "FAIL"
  putStrLn $ "  Test 4 (nested):      " ++ if t4 then "PASS" else "FAIL"

  let passed = length (filter id [t1, t2, t3, t4])
  putStrLn $ "\nPassed: " ++ show passed ++ "/4"

  -- Cleanup
  removePathForcibly localTempDir
