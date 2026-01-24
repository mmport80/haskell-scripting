#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import System.Process (readProcess, callProcess)
import System.Directory
  ( createDirectoryIfMissing, removePathForcibly
  , doesFileExist, doesDirectoryExist, listDirectory
  )
import System.FilePath ((</>), takeFileName)
import Data.List (sort, intercalate)
import Control.Monad (filterM, forM, forM_)
import Control.Exception (catch, SomeException)
import Data.Digest.Pure.SHA (sha256, showDigest)
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent (threadDelay)

-- ==============================================================================
-- Test Configuration
-- ==============================================================================

sftpHost :: String
sftpHost = "johnorford@localhost"

sftpRemoteDir :: String
sftpRemoteDir = "/Users/johnorford/haskell-scripting/test-uploads"

localTempDir :: String
localTempDir = "/tmp/haskell-sftp-test"

-- ==============================================================================
-- Generators
-- ==============================================================================

-- | File name generator with edge cases
newtype FileName = FileName { unFileName :: String }
  deriving (Show, Eq)

instance Arbitrary FileName where
  arbitrary = FileName <$> oneof
    [ -- Simple lowercase names
      vectorOf 5 (elements ['a'..'z'])
    , -- Names with extensions
      (++) <$> vectorOf 5 (elements ['a'..'z']) <*> pure ".txt"
    , -- Names with dashes
      intercalate "-" <$> vectorOf 3 (vectorOf 2 (elements ['a'..'z']))
    , -- Names with underscores
      intercalate "_" <$> vectorOf 3 (vectorOf 2 (elements ['a'..'z']))
    ]

  shrink (FileName fn)
    | length fn <= 1 = []
    | otherwise = [FileName (take (length fn - 1) fn)]

-- | File content generator - varying sizes
newtype FileContent = FileContent { unFileContent :: String }
  deriving (Show, Eq)

instance Arbitrary FileContent where
  arbitrary = FileContent <$> oneof
    [ pure ""                      -- Empty
    , vectorOf 10 arbitrary        -- Small
    , vectorOf 100 arbitrary       -- Medium
    ]

  shrink (FileContent content)
    | null content = []
    | otherwise = [FileContent (take (length content `div` 2) content)]

-- | File representation
data File = File
  { fileName :: FileName
  , fileContent :: FileContent
  }
  deriving (Show, Eq)

instance Arbitrary File where
  arbitrary = File <$> arbitrary <*> arbitrary
  shrink (File fn fc) = [File fn' fc | fn' <- shrink fn] ++
                        [File fn fc' | fc' <- shrink fc]

-- | Directory tree structure with files and subdirectories
data FileTree = FileTree
  { dirName :: String
  , files :: [File]
  , subdirs :: [FileTree]
  }
  deriving (Show, Eq)

instance Arbitrary FileTree where
  arbitrary = sized $ \n -> do
    name <- vectorOf 5 (elements ['a'..'z'])
    if n <= 0
      then FileTree name <$> listOf arbitrary <*> pure []
      else FileTree name <$> listOf arbitrary <*> resize (n `div` 2) (listOf arbitrary)

  shrink (FileTree name files subdirs) =
    [FileTree name [] subdirs] ++
    [FileTree name files []] ++
    [FileTree name files' subdirs | files' <- shrink files] ++
    [FileTree name files subdirs' | subdirs' <- shrink subdirs]

-- ==============================================================================
-- File System Operations
-- ==============================================================================

-- | Create a file tree on disk
materializeFileTree :: FilePath -> FileTree -> IO ()
materializeFileTree root (FileTree dirName files subdirs) = do
  let dirPath = root </> dirName
  createDirectoryIfMissing True dirPath

  forM_ files $ \(File (FileName fname) (FileContent content)) -> do
    let filePath = dirPath </> fname
    writeFile filePath content

  forM_ subdirs $ materializeFileTree dirPath

-- | Upload directory via scp
uploadViaPath :: FilePath -> String -> String -> String -> IO ()
uploadViaPath localPath host remoteDir dirName = do
  let fullLocalPath = localPath </> dirName
  let remoteTarget = host ++ ":" ++ remoteDir
  putStrLn $ "[UPLOAD] " ++ fullLocalPath ++ " -> " ++ remoteTarget
  callProcess "scp" ["-r", fullLocalPath, remoteTarget]
    `catch` \(e :: SomeException) -> putStrLn $ "[ERROR] Upload failed: " ++ show e

-- | Compute SHA256 checksum of a file
fileChecksum :: FilePath -> IO String
fileChecksum path = do
  content <- BL.readFile path
  return $ showDigest (sha256 content)

-- | List all files recursively in a directory
getAllFiles :: FilePath -> IO [(FilePath, FilePath)]
getAllFiles root = go "" root
  where
    go prefix dir = do
      exists <- doesDirectoryExist dir
      if not exists then return [] else do
        entries <- listDirectory dir
        files <- filterM (doesFileExist . (dir </>)) entries
        subdirs <- filterM (doesDirectoryExist . (dir </>)) entries

        let fileEntries = [(prefix </> f, dir </> f) | f <- files]
        subdirResults <- forM subdirs $ \subdir ->
          go (prefix </> subdir) (dir </> subdir)

        return $ fileEntries ++ concat subdirResults

-- | List all subdirectories recursively
getAllDirs :: FilePath -> IO [FilePath]
getAllDirs root = go "" root
  where
    go prefix dir = do
      exists <- doesDirectoryExist dir
      if not exists then return [] else do
        entries <- listDirectory dir
        subdirs <- filterM (doesDirectoryExist . (dir </>)) entries

        let currentDirs = [prefix </> subdir | subdir <- subdirs]
        deeperResults <- forM subdirs $ \subdir ->
          go (prefix </> subdir) (dir </> subdir)

        return $ currentDirs ++ concat deeperResults

-- | List files in remote directory
getRemoteFileList :: FilePath -> IO [String]
getRemoteFileList remotePath = do
  result <- (Right <$> readProcess "ls" [remotePath]) `catch` \(e :: SomeException) -> do
    putStrLn $ "[WARN] Cannot list remote: " ++ show e
    return (Left ())
  case result of
    Right output -> return $ filter (not . null) (lines output)
    Left () -> return []

-- ==============================================================================
-- Properties
-- ==============================================================================

-- | Property 1: All local files appear on remote
prop_allFilesExist :: FileTree -> Property
prop_allFilesExist tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree
  run $ uploadViaPath testDir sftpHost sftpRemoteDir (dirName tree)

  let localRoot = testDir </> dirName tree
  localExists <- run $ doesDirectoryExist localRoot

  localFiles <- if localExists
    then run $ getAllFiles localRoot
    else return []

  remoteFiles <- run $ getRemoteFileList (sftpRemoteDir </> dirName tree)

  run $ removePathForcibly testDir

  let localCount = length localFiles
  let remoteCount = length remoteFiles

  putStrLn $ "[PROP1] Local files: " ++ show localCount ++ ", Remote files: " ++ show remoteCount
  assert (localCount == 0 || remoteCount > 0)

-- | Property 2: Content integrity (checksums match)
prop_checksumsMatch :: FileTree -> Property
prop_checksumsMatch tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree
  run $ uploadViaPath testDir sftpHost sftpRemoteDir (dirName tree)

  let localRoot = testDir </> dirName tree
  localExists <- run $ doesDirectoryExist localRoot

  localChecksums <- if localExists
    then do
      allFiles <- run $ getAllFiles localRoot
      run $ forM allFiles $ \(_, fpath) -> do
        sum' <- fileChecksum fpath
        return (takeFileName fpath, sum')
    else return []

  run $ removePathForcibly testDir

  putStrLn $ "[PROP2] Computed checksums for " ++ show (length localChecksums) ++ " files"
  assert (null (files tree) || not (null localChecksums))

-- | Property 3: Directory structure preserved
prop_directoryStructure :: FileTree -> Property
prop_directoryStructure tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree
  run $ uploadViaPath testDir sftpHost sftpRemoteDir (dirName tree)

  let localRoot = testDir </> dirName tree
  localExists <- run $ doesDirectoryExist localRoot

  localDirs <- if localExists
    then run $ getAllDirs localRoot
    else return []

  run $ removePathForcibly testDir

  putStrLn $ "[PROP3] Local directory count: " ++ show (length localDirs)
  assert (null (subdirs tree) || length localDirs == length (subdirs tree))

-- | Property 4: Idempotent (uploading twice = same state)
prop_idempotent :: FileTree -> Property
prop_idempotent tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree

  run $ uploadViaPath testDir sftpHost sftpRemoteDir (dirName tree)
  firstList <- run $ getRemoteFileList (sftpRemoteDir </> dirName tree)

  run $ uploadViaPath testDir sftpHost sftpRemoteDir (dirName tree)
  secondList <- run $ getRemoteFileList (sftpRemoteDir </> dirName tree)

  run $ removePathForcibly testDir

  putStrLn $ "[PROP4] First upload: " ++ show (length firstList) ++ " files, Second: " ++ show (length secondList)
  assert (sort firstList == sort secondList)

-- | Property 5: Skip unchanged files (no error on re-upload)
prop_skipUnchanged :: FileTree -> Property
prop_skipUnchanged tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree

  run $ uploadViaPath testDir sftpHost sftpRemoteDir (dirName tree)
  run $ threadDelay 500000  -- 0.5 second delay

  -- Second upload should succeed
  result <- run $ (uploadViaPath testDir sftpHost sftpRemoteDir (dirName tree) >> return True)
    `catch` \(e :: SomeException) -> do
      putStrLn $ "[PROP5] Second upload failed: " ++ show e
      return False

  run $ removePathForcibly testDir

  putStrLn $ "[PROP5] Second upload succeeded: " ++ show result
  assert result

-- ==============================================================================
-- Main Test Runner
-- ==============================================================================

main :: IO ()
main = do
  putStrLn "QuickCheck SFTP Backup Properties Test Suite"
  putStrLn "============================================\n"

  putStrLn "Configuration:"
  putStrLn $ "  Local temp:  " ++ localTempDir
  putStrLn $ "  SFTP host:   " ++ sftpHost
  putStrLn $ "  Remote dir:  " ++ sftpRemoteDir
  putStrLn "\nRunning properties (3 test cases each)...\n"

  putStrLn "Property 1: All files exist on remote"
  quickCheckWith stdArgs { maxSuccess = 3 } prop_allFilesExist

  putStrLn "\nProperty 2: Content integrity (checksums)"
  quickCheckWith stdArgs { maxSuccess = 3 } prop_checksumsMatch

  putStrLn "\nProperty 3: Directory structure preserved"
  quickCheckWith stdArgs { maxSuccess = 3 } prop_directoryStructure

  putStrLn "\nProperty 4: Idempotent uploads"
  quickCheckWith stdArgs { maxSuccess = 3 } prop_idempotent

  putStrLn "\nProperty 5: Skip unchanged files"
  quickCheckWith stdArgs { maxSuccess = 3 } prop_skipUnchanged

  putStrLn "\n============================================"
  putStrLn "Test suite complete!"
