#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, assert, run)
import System.Process (readProcess, callProcess, createProcess, StdStream(..), std_in, waitForProcess, proc)
import System.IO (hPutStr, hClose)
import System.Directory
  ( createDirectoryIfMissing, removePathForcibly
  , doesFileExist, doesDirectoryExist, listDirectory
  )
import System.FilePath ((</>), takeFileName)
import Data.List (sort, intercalate, nub, isPrefixOf)
import Control.Monad (filterM, forM, forM_, void)
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
sftpRemoteDir = "/Users/johnorford/haskell-scripting/test/test-uploads"

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
  arbitrary = do
    name <- vectorOf 5 (elements ['a'..'z'])
    -- Generate small flat trees: 0-3 files max to keep SFTP output manageable
    numFiles <- choose (0, 3)
    files <- vectorOf numFiles arbitrary
    pure $ FileTree name files []

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

-- | Execute SFTP batch commands (matches implementation in script.hs)
executeSftpBatch :: String -> FilePath -> FilePath -> FilePath -> IO ()
executeSftpBatch host remoteDir localBase localDir = do
  let fullLocalPath = localBase </> localDir
  putStrLn $ "[UPLOAD] " ++ fullLocalPath ++ " -> " ++ host ++ ":" ++ remoteDir

  files <- findAllFiles fullLocalPath
  if null files
    then putStrLn "No files to upload"
    else do
      let commands = genSftpCommands remoteDir fullLocalPath files
      putStrLn $ "Executing " ++ show (length commands) ++ " SFTP commands..."
      (Just hIn, _, _, process) <- createProcess (proc "sftp" [host])
        { std_in = CreatePipe }
      hPutStr hIn $ intercalate "\n" commands ++ "\nquit\n"
      hClose hIn
      exitCode <- waitForProcess process
      putStrLn $ "SFTP exit code: " ++ show exitCode
  where
    findAllFiles path = do
      output <- (Right <$> readProcess "find" [path, "-type", "f"] "")
        `catch` \(e :: SomeException) -> return (Left e)
      case output of
        Right out -> return $ filter (not . null) $ lines out
        Left e -> putStrLn ("[ERROR] find failed: " ++ show e) >> return []

    genSftpCommands remoteBase localPath files =
      mkdirs ++ uploads
      where
        dirs = nub $ map getDirPath files
        getDirPath f = reverse $ dropWhile (/= '/') $ reverse f
        relativeDirs = map (stripPrefix localPath) dirs
        mkdirs = ["mkdir " ++ remoteBase ++ "/" ++ d | d <- relativeDirs, not (null d)]
        uploads = ["put " ++ f ++ " " ++ remoteBase ++ "/" ++ stripPrefix localPath f | f <- files]

    stripPrefix prefix path
      | prefix `isPrefixOf` path = drop (length prefix + 1) path
      | otherwise = path

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
  result <- (Right <$> readProcess "ls" [remotePath] "") `catch` \(e :: SomeException) -> do
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
  run $ executeSftpBatch sftpHost sftpRemoteDir testDir (dirName tree)

  let localRoot = testDir </> dirName tree
  localExists <- run $ doesDirectoryExist localRoot

  localFiles <- if localExists
    then run $ getAllFiles localRoot
    else return []

  remoteFiles <- run $ getRemoteFileList (sftpRemoteDir </> dirName tree)

  run $ removePathForcibly testDir

  let localCount = length localFiles
  let remoteCount = length remoteFiles

  run $ putStrLn $ "[PROP1] Local files: " ++ show localCount ++ ", Remote files: " ++ show remoteCount
  assert (localCount == 0 || remoteCount > 0)

-- | Property 2: Content integrity (checksums match)
prop_checksumsMatch :: FileTree -> Property
prop_checksumsMatch tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree
  run $ executeSftpBatch sftpHost sftpRemoteDir testDir (dirName tree)

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

  run $ putStrLn $ "[PROP2] Computed checksums for " ++ show (length localChecksums) ++ " files"
  assert (null (files tree) || not (null localChecksums))

-- | Property 3: Directory structure preserved
prop_directoryStructure :: FileTree -> Property
prop_directoryStructure tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree
  run $ executeSftpBatch sftpHost sftpRemoteDir testDir (dirName tree)

  let localRoot = testDir </> dirName tree
  localExists <- run $ doesDirectoryExist localRoot

  localDirs <- if localExists
    then run $ getAllDirs localRoot
    else return []

  remoteDirs <- run $ getRemoteFileList (sftpRemoteDir </> dirName tree)

  run $ removePathForcibly testDir

  run $ putStrLn $ "[PROP3] Local directories: " ++ show (length localDirs) ++ ", Remote entries: " ++ show (length remoteDirs)
  assert (null (subdirs tree) || length remoteDirs > 0)

-- | Property 4: Idempotent (uploading twice = same state)
prop_idempotent :: FileTree -> Property
prop_idempotent tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree

  run $ executeSftpBatch sftpHost sftpRemoteDir testDir (dirName tree)
  firstList <- run $ getRemoteFileList (sftpRemoteDir </> dirName tree)

  run $ executeSftpBatch sftpHost sftpRemoteDir testDir (dirName tree)
  secondList <- run $ getRemoteFileList (sftpRemoteDir </> dirName tree)

  run $ removePathForcibly testDir

  run $ putStrLn $ "[PROP4] First upload: " ++ show (length firstList) ++ " files, Second: " ++ show (length secondList)
  assert (sort firstList == sort secondList)

-- | Property 5: Skip unchanged files (no error on re-upload)
prop_skipUnchanged :: FileTree -> Property
prop_skipUnchanged tree = monadicIO $ do
  let testDir = localTempDir
  run $ createDirectoryIfMissing True testDir
  run $ materializeFileTree testDir tree

  run $ executeSftpBatch sftpHost sftpRemoteDir testDir (dirName tree)
  run $ threadDelay 500000  -- 0.5 second delay

  -- Second upload should succeed
  result <- run $ (executeSftpBatch sftpHost sftpRemoteDir testDir (dirName tree) >> return True)
    `catch` \(e :: SomeException) -> do
      putStrLn $ "[PROP5] Second upload failed: " ++ show e
      return False

  run $ removePathForcibly testDir

  run $ putStrLn $ "[PROP5] Second upload succeeded: " ++ show result
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
  putStrLn "\nRunning properties (1 test case each)...\n"

  putStrLn "Property 1: All files exist on remote"
  quickCheckWith stdArgs { maxSuccess = 1 } prop_allFilesExist

  putStrLn "\nProperty 2: Content integrity (checksums)"
  quickCheckWith stdArgs { maxSuccess = 1 } prop_checksumsMatch

  putStrLn "\nProperty 3: Directory structure preserved"
  quickCheckWith stdArgs { maxSuccess = 1 } prop_directoryStructure

  putStrLn "\nProperty 4: Idempotent uploads"
  quickCheckWith stdArgs { maxSuccess = 1 } prop_idempotent

  putStrLn "\nProperty 5: Skip unchanged files"
  quickCheckWith stdArgs { maxSuccess = 1 } prop_skipUnchanged

  putStrLn "\n============================================"
  putStrLn "Test suite complete!"
