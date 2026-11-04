
## Version 0.2.1

This release fixes a compatibility issue related to determining
the number of available CPU cores.

### Bug fixes

- Replaced `future::availableCores()` with
  `parallelly::availableCores()` to improve compatibility
  with future package updates.
- Removed the redundant numeric check for the number of
  available cores.

## Test environments

- Local Windows 11, R 4.4.1
- GitHub Actions:
  - macOS (devel, oldrel-1)
  - Windows (release, 4.1)
  - Ubuntu (devel, release, oldrel-1, oldrel-2)
- Windows Server 2022, R-devel (2026-11-03, r90835)

## R CMD check results

- Local Windows 11, R 4.4.1: 0 errors, 0 warnings, 1 note
- Windows Server 2022, R-devel: 0 errors, 0 warnings, 0 notes
- GitHub Actions: all 8 jobs passed

The remaining NOTE in the local check is:

- checking for future file timestamps ... NOTE
  - unable to verify current time

## Reverse dependencies

There are currently no reverse dependencies for this package.
