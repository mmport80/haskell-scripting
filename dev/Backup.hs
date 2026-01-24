-- Backup library - testable core logic
module Backup where

import Data.List (isPrefixOf, nub, sortBy)

-- Extract unique directories from file list
extractDirs :: [FilePath] -> [FilePath]
extractDirs files = nub $ map getDirPath files
  where
    getDirPath f = reverse $ dropWhile (/= '/') $ reverse f

-- Remove prefix from path (make relative)
stripPrefix :: FilePath -> FilePath -> FilePath
stripPrefix prefix path
  | prefix `isPrefixOf` path = drop (length prefix + 1) path
  | otherwise = path

-- Split path on '/' separator
splitPath :: Char -> FilePath -> [FilePath]
splitPath delim path = case break (== delim) path of
  (a, "") -> [a]
  (a, _:rest) -> a : splitPath delim rest

-- Generate all parent directories for a path
-- "a/b/c" -> ["a", "a/b", "a/b/c"]
expandParents :: FilePath -> [FilePath]
expandParents path =
  let parts = filter (not . null) $ splitPath '/' path
      go _ [] = []
      go acc (x:xs) =
        let acc' = if null acc then x else acc ++ "/" ++ x
        in acc' : go acc' xs
  in go "" parts

-- Generate all parent directories from a list, deduplicated and sorted by depth
genParentDirs :: [FilePath] -> [FilePath]
genParentDirs dirs =
  let allDirs = nub $ concatMap expandParents $ filter (not . null) dirs
      byDepth = sortBy (\a b -> compare (length $ filter (=='/') a) (length $ filter (=='/') b))
  in byDepth allDirs

-- Generate SFTP batch commands to upload all files with directory structure
genSftpCommands :: FilePath -> FilePath -> [FilePath] -> [String]
genSftpCommands remoteBase localBase files =
  mkdirs ++ uploads
  where
    dirs = extractDirs files
    relativeDirs = map (stripPrefix localBase) dirs
    -- Generate ALL parent directories, sorted by depth (shallow to deep)
    allParentDirs = genParentDirs relativeDirs
    -- Use -mkdir prefix to make non-fatal (safe for re-runs when dirs already exist)
    mkdirs = map (\d -> "-mkdir " ++ remoteBase ++ "/" ++ d) allParentDirs
    uploads = map (\f -> "put " ++ f ++ " " ++ remoteBase ++ "/" ++ stripPrefix localBase f) files
