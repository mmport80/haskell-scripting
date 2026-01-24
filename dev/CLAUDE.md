# CLAUDE.md

## Project
Haskell-as-bash-replacement for scripting tasks. Starting with: backup local files to remote SFTP server.

## Run
Scripts use shebang: `#!/usr/bin/env runhaskell`

Execute directly: `./script.hs`

## Constraints
- SFTP only (narrow scope for now)
- Portable, standard library where possible
- Freely use common bash utils, but be pragmatic and use Haskell libraries when fitting

## Style
KISS

## Testing
- Integration tests primary: run script, verify results
- QuickCheck for system properties (generate file scenarios, verify backup consistency)
- Test scripts: `./test_*.hs`
- Currently, for testing sftp is available here: `sftp johnorford@localhost`. The keys are setup so that you can login etc.
- Upload to this directory in the SFTP folder: `/Users/johnorford/haskell-scripting/test-uploads`

## v0.1 - Basic SFTP Backup

### Properties to verify:

```haskell
-- All local files appear on remote
prop_allFilesExist :: FileTree -> Property

-- Content integrity
prop_checksumsMatch :: FileTree -> Property

-- Structure preserved
prop_directoryStructure :: FileTree -> Property

-- Idempotent (backup twice = same state as once)
prop_idempotent :: FileTree -> Property

-- Unchanged files not re-transferred (optional: check transfer log or mtime)
prop_skipUnchanged :: FileTree -> Property
```

### Generators needed:
- `FileTree` - random directory structures with files
- `FileContent` - random file contents (varying sizes)
- `FileName` - edge cases: spaces, unicode, dots, long names
