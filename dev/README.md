# Haskell Scripting - SFTP Backup

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
- **Portable** - Uses standard SFTP and shell utilities
- **Self-contained** - No external config files or dependencies beyond standard tools

## Implementation

The script:
1. Finds all files in the source directory
2. Extracts directory structure
3. Generates SFTP batch commands (`mkdir` + `put`)
4. Pipes commands to `sftp` for execution

See `CLAUDE.md` for project constraints and goals.
