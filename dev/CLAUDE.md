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
- **Self-contained scripts**: Keep everything visible within the script. Minimize external dependencies/config files. Configuration should be script args/env vars when possible.

## Style
KISS

## Configuration
- Scripts should accept configurable inputs (source dir, credentials, destination) via command-line args or environment variables
- Allow users to choose what to back up (not hardcoded patterns)

## Testing
- QA feedback in FEEDBACK.md
- QA role: test functionality to spot problems

## Current Goal: v0.1 - Basic SFTP Backup
A script that backs up local files to a remote SFTP server:
- All local files should appear on remote
- Content integrity preserved
- Directory structure preserved  
- Running twice should be safe (idempotent)
- Ideally skip unchanged files
