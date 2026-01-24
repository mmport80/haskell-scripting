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
- **Property-based testing only** - Use QuickCheck or similar for generative testing
- No unit tests - focus on properties that should hold
- QA feedback in FEEDBACK.md focuses on **functionality issues only**, not testing/code details
- QA role: Test functionality to spot problems (test code, don't review it)

## Current Goal: v0.1 - Basic SFTP Backup
A script that backs up local files to a remote SFTP server:
- All local files should appear on remote
- Content integrity preserved
- Directory structure preserved  
- Running twice should be safe (idempotent)
- Ideally skip unchanged files
