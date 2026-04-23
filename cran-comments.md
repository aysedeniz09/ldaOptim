## Submission type

This is a new release.

## Test environments

* local macOS (R 4.5.1)
* win-builder: R-devel and R-release (via `devtools::check_win_devel()` and
  `devtools::check_win_release()`)

## R CMD check results

0 errors | 0 warnings | 1 note

* The single note is "unable to verify current time" — a file-timestamp
  check that depends on the build environment's clock and is unrelated to
  the package.

## Downstream dependencies

There are no downstream dependencies (first submission).