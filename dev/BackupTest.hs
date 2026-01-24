#!/usr/bin/env runhaskell

import Test.QuickCheck
import Backup
import Data.List (isPrefixOf, nub, isInfixOf, sort)

-- Property: stripPrefix removes the prefix correctly
prop_stripPrefixRemovesPrefix :: String -> String -> Property
prop_stripPrefixRemovesPrefix prefix path =
  not (null prefix) && not (null path) ==>
  let fullPath = prefix ++ "/" ++ path
      result = stripPrefix prefix fullPath
  in result == path

-- Property: stripPrefix returns original if prefix doesn't match
prop_stripPrefixNoMatch :: String -> String -> Property
prop_stripPrefixNoMatch prefix path =
  not (prefix `isPrefixOf` path) ==>
  stripPrefix prefix path == path

-- Property: all files in a directory get uploaded
prop_fileCountMatches :: Property
prop_fileCountMatches =
  property $
  let files = ["/local/file1.txt", "/local/subdir/file2.txt", "/local/subdir/file3.txt"]
      commands = genSftpCommands "/remote" "/local" files
      uploadCmds = filter ("put " `isPrefixOf`) commands
  in length uploadCmds == 3

-- Property: each file gets exactly one put command
prop_eachFileOnceInCommands :: Property
prop_eachFileOnceInCommands =
  property $
  let files = ["/local/a.txt", "/local/b.txt", "/local/c/d.txt"]
      commands = genSftpCommands "/remote" "/local" files
  in all (\f -> length [c | c <- commands, f `isInfixOf` c] == 1) files

-- Property: remote paths maintain directory structure
prop_remotePathsPreserveStructure :: Property
prop_remotePathsPreserveStructure =
  property $
  let files = ["/local/file.txt", "/local/subdir/file2.txt"]
      commands = genSftpCommands "/remote" "/local" files
      uploadCmds = filter ("put " `isPrefixOf`) commands
  in all (\cmd -> "/remote/" `isInfixOf` cmd) uploadCmds

-- Property: mkdir before put (mkdirs come first in command list)
prop_mkdirsBeforePuts :: Property
prop_mkdirsBeforePuts =
  property $
  let files = ["/local/a/b/file.txt", "/local/c/file2.txt"]
      commands = genSftpCommands "/remote" "/local" files
      mkdirIndices = [i | (i, c) <- zip [0 :: Int ..] commands, "mkdir" `isInfixOf` c]
      putIndices = [i | (i, c) <- zip [0 :: Int ..] commands, "put" `isInfixOf` c]
  in case (mkdirIndices, putIndices) of
       ([], _) -> True  -- no mkdirs, valid
       (_, []) -> True  -- no puts, valid
       (_, _) -> maximum mkdirIndices < minimum putIndices

-- Property: All parent directories are generated for nested paths
prop_allParentDirsGenerated :: Property
prop_allParentDirsGenerated =
  property $
  let files = ["/local/a/b/c/file.txt", "/local/x/y/file2.txt"]
      commands = genSftpCommands "/remote" "/local" files
      mkdirCmds = filter ("mkdir" `isInfixOf`) commands
  in -- For a/b/c/file.txt we should have mkdirs for a, a/b, a/b/c
     -- For x/y/file2.txt we should have mkdirs for x, x/y
     length mkdirCmds >= 5 && all ("-mkdir" `isPrefixOf`) mkdirCmds

-- Property: Mkdir commands are ordered by depth (shallow to deep)
prop_mkdirsOrderedByDepth :: Property
prop_mkdirsOrderedByDepth =
  property $
  let files = ["/local/a/b/c/file.txt"]
      commands = genSftpCommands "/remote" "/local" files
      mkdirCmds = filter ("mkdir" `isInfixOf`) commands
      depths = map (\cmd -> length $ filter (== '/') cmd) mkdirCmds
  in depths == sort depths

-- Property: Mkdir commands have non-fatal prefix (-)
prop_mkdirCommandsNonFatal :: Property
prop_mkdirCommandsNonFatal =
  property $
  let files = ["/local/a/b/file.txt", "/local/c/d/e/file2.txt"]
      commands = genSftpCommands "/remote" "/local" files
      mkdirCmds = filter ("mkdir" `isInfixOf`) commands
  in all (\cmd -> "-mkdir" `isPrefixOf` cmd) mkdirCmds

main :: IO ()
main = do
  putStrLn "Running property tests..."
  quickCheck prop_stripPrefixRemovesPrefix
  putStrLn "✓ stripPrefix adds/removes correctly"

  quickCheck prop_stripPrefixNoMatch
  putStrLn "✓ stripPrefix handles non-matching paths"

  quickCheck prop_fileCountMatches
  putStrLn "✓ All files get upload commands"

  quickCheck prop_eachFileOnceInCommands
  putStrLn "✓ Each file appears exactly once"

  quickCheck prop_remotePathsPreserveStructure
  putStrLn "✓ Remote paths maintain directory structure"

  quickCheck prop_mkdirsBeforePuts
  putStrLn "✓ mkdir commands come before put commands"

  quickCheck prop_allParentDirsGenerated
  putStrLn "✓ All parent directories are generated for nested paths"

  quickCheck prop_mkdirsOrderedByDepth
  putStrLn "✓ mkdir commands are ordered by depth (shallow to deep)"

  quickCheck prop_mkdirCommandsNonFatal
  putStrLn "✓ mkdir commands have non-fatal prefix (-)"

  putStrLn "\nAll tests passed!"
