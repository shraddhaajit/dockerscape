## Test environments
* local Windows 11 x64 (build 26200), R 4.6.1 (2026-06-24 ucrt)
* win-builder (recommended for maintainer before submission: `devtools::check_win_devel()`)
* R-hub v2 (recommended for maintainer before submission: `rhub::rhub_check()`)

## R CMD check results

Status: 0 ERRORS | 0 WARNINGS | 2 NOTEs

* checking CRAN incoming feasibility ... NOTE
  - Maintainer: 'Shraddha Ajit <shra19076@gmail.com>'
  - New submission
  
  Explanation: This is the first submission of `docketscape` to CRAN. All URLs in DESCRIPTION, documentation, and README resolve with HTTP 200 (verified with `urlchecker::url_check()`).

* checking whether package 'docketscape' can be installed ... NOTE
  - Non-staged installation was used
  
  Explanation: Local environmental artifact resulting from running `R CMD check` with `--no-staged-install` on a Windows OneDrive directory to avoid intermittent filesystem lock delays. Staged installation is standard on CRAN build systems.

## Downstream dependencies
There are currently no reverse dependencies for this new package.
