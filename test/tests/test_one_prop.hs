#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import System.Process (readProcess)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath ((</>))
import Control.Exception (catch, SomeException)

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

-- Simple test case: create a few files, run script, check exit code
prop_scriptExecutes :: TestFile -> TestFile -> Property
prop_scriptExecutes (TestFile f1) (TestFile f2) = monadicIO $ do
  let testDir = localTempDir </> "test"

  -- Setup
  run $ removePathForcibly testDir
  run $ createDirectoryIfMissing True testDir
  run $ writeFile (testDir </> f1) "content1"
  run $ writeFile (testDir </> f2) "content2"

  -- Run script
  success <- run $ (do
    _ <- readProcess "./dev/script" [testDir, sftpHost, sftpRemoteDir] ""
    return True) `catch` \(e :: SomeException) -> do
      putStrLn $ "ERROR: " ++ show e
      return False

  -- Cleanup
  run $ removePathForcibly testDir

  run $ putStrLn $ "  Files: " ++ f1 ++ ", " ++ f2 ++ " -> " ++ if success then "OK" else "FAIL"
  assert success

main :: IO ()
main = do
  putStrLn "QuickCheck - Single Simple Property"
  putStrLn "===================================="
  putStrLn ""

  quickCheckWith stdArgs { maxSuccess = 3 } prop_scriptExecutes

  putStrLn ""
  putStrLn "Test complete."
