## Version 0.2.2

This release includes an improvement to reduce the execution time of the `adjpin()` documentation example.

### Improvements

* Reduced the number of initial parameter sets used in the `adjpin()` documentation example from 20 to 10 to reduce execution time.
* No changes were made to the underlying estimation algorithm.

## Test environments

* Local Windows 11, R 4.6.2 (2026-10-29 ucrt)
* Win-builder, Windows Server 2022, R-devel (2026-11-04 r90836 ucrt)

## R CMD check results

* Local Windows 11, R 4.6.2: 0 errors, 0 warnings, 2 notes.
* Win-builder, Windows R-devel: 0 errors, 0 warnings, 0 notes.

### Notes

The local Windows 11 check produced two NOTEs:

* The `generatedata_adjpin` example exceeded the 5-second execution-time threshold (6.42 seconds).
* A `lastMiKTeXException` file was detected in the temporary check directory. The PDF and HTML manual checks completed successfully.

The HTML mathematical rendering NOTE from the previous check was resolved after installing the optional `V8` package.

## Reverse dependencies

There are currently no reverse dependencies for this package.
