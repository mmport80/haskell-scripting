# QA Feedback - SFTP Backup Script

## [2026-02-03] Functional Issues Found

### Issue 1: Silent Upload Failures with Paths Containing `./`

**Problem:** When the remote destination path contains `./` (e.g., `backups/./2025-02`), the script exits successfully but uploads fail silently.

**What happens:**
- Script: "Backup complete" (exit 0)
- SFTP: "dest open ... No such file or directory"
- Result: No files uploaded, but no error reported

**Example:**
```bash
./script.hs ~/docs user@host backups/./2025-02
# Reports: Backup complete ✓
# Actually: All files failed to upload ✗
```

**Impact:** Backups may appear successful when they actually failed. Data isn't backed up.

---

### Issue 2: Script Doesn't Validate Destination Directory Exists

**Problem:** When the destination path doesn't exist and can't be created, individual file uploads fail, but the script still reports success.

**Behavior:** Each failed `put` shows an error in SFTP output but script exits 0.

**Impact:** Backups may silently fail if remote path is inaccessible or read-only.

---

## What Works Correctly

✓ Basic file backup (flat and nested directories)
✓ Empty directory handling
✓ Invalid source path rejection
✓ Error handling on connection failure
✓ Idempotency (safe to run multiple times)
✓ File change detection (modified files re-upload)
✓ Nested directory structure preservation

---

## [2026-02-02] Test Results - All Tests Passing

### Test Infrastructure Update
Fixed test harness path references to correctly locate the developer's script executable. Tests now run successfully in the test environment.

### Tested Behaviors

**✓ Basic File Backup**
- Script correctly finds and uploads files in flat directories
- Tested: flat directories with multiple files
- Result: All files discovered, uploaded successfully

**✓ Empty Directory Handling**
- Script handles directories with no files gracefully
- Result: Completes without crashing

**✓ Invalid Source Path Rejection**
- Script properly rejects nonexistent source directories
- Tested: /nonexistent/path
- Result: Script exits with error, properly propagated

**✓ Nested Directory Structure**
- Script handles and uploads nested directory structures
- Tested: single-level and multi-level (3+ levels) nesting
- Result: Directory hierarchy preserved correctly on remote

**✓ Error Reporting**
- Script detects SFTP connection failures and reports errors
- Tested: connection to invalid host
- Result: Errors properly detected and exit codes correct

**✓ Idempotency**
- Script can be executed multiple times on same files safely
- Tested: running backup twice on identical file set
- Result: Second run completes successfully

**✓ File Modification Handling**
- Script handles modified files on re-run
- Tested: modify file between runs and re-run backup
- Result: Modified file uploaded successfully

**✓ Property-based Testing**
- Script executes correctly with randomly generated file sets
- Tested: 3 different random file name combinations
- Result: All executions successful

### Test Summary

All test suites passing (4/4 tests in each):
- **test_simple.hs** - Basic unit tests: PASS
- **test_one_prop.hs** - Property-based tests: PASS
- **test_error_handling.hs** - Error & idempotency tests: PASS

---

## [2026-01-24 01:00 UTC] Test Results

### Tested Behaviors

**✓ File Discovery**
- Script correctly finds all files in source directory
- Tested: flat directories, nested directories, mixed structures
- Result: All files discovered and uploaded

**✓ Directory Structure Preservation**
- Script preserves directory structure on remote
- Files in `source/subdir/file.txt` appear as `remote/subdir/file.txt`
- Tested: single level nesting, multi-level nesting (3+ levels deep)
- Result: All paths preserved correctly

**✓ Empty Directory Handling**
- Script handles empty directories without crashing
- Tested: source directory with no files
- Result: Completes successfully with "No files found" message

**✓ Invalid Source Path Rejection**
- Script properly rejects nonexistent source directories
- Tested: /nonexistent/path
- Result: Script fails with appropriate error (exit code 1)

**✓ Error Reporting on SFTP Failure**
- Script detects SFTP failures and exits with non-zero code
- Tested: connection to invalid host
- Result: Script exits with code 255, prints error to stderr

**✓ Safe Re-runs**
- Script can be executed multiple times on same source
- Tested: running backup twice on identical file set
- Result: Second run completes successfully, no crashes

**✓ File Modification Handling**
- Script handles modified files correctly
- Tested: modify file between runs, re-run backup
- Result: Modified file uploaded successfully

---

## Summary

All core backup functionality is working correctly:
- Files are discovered and uploaded
- Directory structures are preserved
- Errors are properly reported
- Re-running backups is safe
