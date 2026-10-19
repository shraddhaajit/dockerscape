## Test environments
* local Windows 11 x64 (build 26200), R 4.6.1 (2026-06-24 ucrt)
* win-builder (R-devel): `devtools::check_win_devel()`
* R-hub v2: `rhub::rhub_check()`

## R CMD check results

Status: 0 ERRORS | 0 WARNINGS | 1 NOTE

* checking CRAN incoming feasibility ... NOTE
  - Maintainer: 'Shraddha Ajit <shra19076@gmail.com>'
  - New submission

  Explanation: This is the initial submission of `docketscape` to CRAN.

## Additional Compliance Disclosures

* **URL Validation**: All URLs in `DESCRIPTION`, documentation, and `README.md` have been verified with `urlchecker::url_check()` and resolve directly with HTTP 200 without redirects.
* **Third-Party Data & Development Data Lab (DDL)**: `docketscape` contains **zero empirical judicial records**. All package tests, examples, and vignette code run exclusively on small, self-contained synthetic data structures. No external internet access or downloaded datasets are required for testing. Researchers wishing to use empirical DDL data source it independently under DDL terms.
* **Orthography & Language**: The package uses British English conventions (`Language: en-GB`). Spelling has been verified using `spelling::spell_check_package()` with all specialized technical/legal terms recorded in `inst/WORDLIST`.
* **Methodological Scope**: Observational and non-causal framework. The package does not evaluate judicial performance, assess diligence, or rank individual judges.
* **Package Footprint & Performance**: Source package tarball is ~102 KB. The full test suite of 450 unit tests completes in under 7 seconds.

## Downstream dependencies
There are currently no reverse dependencies for this new package.

