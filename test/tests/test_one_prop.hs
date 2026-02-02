#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import System.Process (readProcess)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)
import Data.List (intercalate)

-- Test config
localTempDir = "/tmp/prop-test"
sftpHost = "johnorford@localhost"
sftpRemoteDir = "/Users/johnorford/haskell-scripting/test/test-uploads"

-- Simple file generator - just names
newtype TestFile = TestFile String deriving (Show)

instance Arbitrary TestFile where
  arbitrary = TestFile <$> vectorOf 5 (elements ['a'..'z'])
  shrink (TestFile s) | length s <= 1 = []
                       | otherwise = [TestFile (take (length s - 1) s)]

-- Path generator to test edge cases
newtype DestinationPath = DestinationPath String deriving (Show)

instance Arbitrary DestinationPath where
  arbitrary = do
    -- Generate paths with potential edge cases
    pathType <- elements ["simple", "dot", "dotslash", "embedded", "nested", "deep"]

    baseComponents <- listOf1 (vectorOf 3 (elements ['a'..'z']))
    let basePath = intercalate "/" baseComponents

    case pathType of
      "simple"   -> return $ DestinationPath basePath
      "dot"      -> return $ DestinationPath "."
      "dotslash" -> return $ DestinationPath ("./backups/" ++ basePath)
      "embedded" -> return $ DestinationPath (basePath ++ "/./subdir")
      "nested"   -> return $ DestinationPath ("a/b/c")
      "deep"     -> return $ DestinationPath "deep/nested/very/long/path"
      _          -> return $ DestinationPath basePath

  shrink (DestinationPath s) | length s <= 1 = []
                              | otherwise = [DestinationPath (take (length s - 1) s)]

-- Simple test case: create a few files, run script with various destination paths
prop_scriptExecutes :: TestFile -> TestFile -> DestinationPath -> Property
prop_scriptExecutes (TestFile f1) (TestFile f2) (DestinationPath destPath) = monadicIO $ do
  let testDir = localTempDir </> "test"

  -- Setup
  run $ removePathForcibly testDir
  run $ createDirectoryIfMissing True testDir
  run $ writeFile (testDir </> f1) "content1"
  run $ writeFile (testDir </> f2) "content2"

  -- Run script with generated destination path
  success <- run $ (do
    _ <- readProcess "../dev/script" [testDir, sftpHost, destPath] ""
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "ERROR with path '" ++ destPath ++ "': " ++ show e
      return False

  -- Cleanup
  run $ removePathForcibly testDir

  run $ putStrLn $ "  Files: " ++ f1 ++ ", " ++ f2 ++ " | Path: " ++ destPath ++ " -> " ++ if success then "OK" else "FAIL"
  assert success

main :: IO ()
main = do
  putStrLn "QuickCheck - Path Edge Cases"
  putStrLn "===================================="
  putStrLn "Testing script with various destination path formats..."
  putStrLn ""

  quickCheckWith stdArgs { maxSuccess = 10 } prop_scriptExecutes

  putStrLn ""
  putStrLn "Test complete."
