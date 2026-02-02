# Haskell Scripting

Haskell-as-bash-replacement for scripting tasks. Starting with SFTP backup.

## Current Goal: v0.1 - Basic SFTP Backup

A self-contained Haskell script for backing up local files to a remote SFTP server.

## Requirements

- Haskell (with `runhaskell`)
- `find` utility
- `sftp` (OpenSSH)

## Usage

```bash
./script.hs <source-dir> <user@host> <remote-dir>
```

### Arguments

- `<source-dir>`: Local directory to back up
- `<user@host>`: SSH user and host (e.g., `backup@sftp.example.com`)
- `<remote-dir>`: Remote destination directory on SFTP server

### Example

```bash
./script.hs ~/documents backup@sftp.example.com /backups/documents
```

## Features

- **All local files backed up** - Recursively finds and uploads all files
- **Directory structure preserved** - Remote directory structure mirrors local
- **Content integrity** - Verification of backed-up content
- **Idempotent** - Safe to run multiple times
- **Skip unchanged files** - Avoids re-uploading when content hasn't changed
- **Portable** - Uses standard SFTP and shell utilities
- **Self-contained** - No external config files; configuration via command-line args or environment variables

## Implementation

The script:
1. Finds all files in the source directory
2. Extracts directory structure
3. Generates SFTP batch commands (`mkdir` + `put`)
4. Pipes commands to `sftp` for execution

## Project Constraints

- **SFTP only** - Narrow scope for initial implementation
- **Portable** - Standard library where possible, pragmatic use of common bash utils
- **Self-contained** - Minimize external dependencies and config files
- **KISS** - Keep it simple

See `CLAUDE.md` for detailed project constraints and goals.
