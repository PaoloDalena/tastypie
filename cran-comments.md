# Version 0.0.3

In this version I have:

* solved the issue pointed out by the CRAN checks related to the importing of the `magrittr` package that is actually not used
* made the package `tibble`-friendly
* improved something about the documentations of the `pie_bake()` and `pie_bake_pro()` functions.


## Test environments
* local OS X install, R 4.0.3
* win-builder (devel and release)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
0 errors | 0 warnings | 0 note


# Version 0.0.2

## Resubmission #2
This is a resubmission. In this version I have:

* Added `\value` to .Rd files to explain the functions results in the documentations (of `pie_bake()`, `pie_bake_pro()`, `pie_templates()`, `pie_discover()`).
* Solved the `\dontrun{}` issue in the examples of `pie_datacheck()`

## Resubmission #1
This is a resubmission. In this version I have:

* Fixed the lifecycle URL in the README that moved from https://www.tidyverse.org/lifecycle/#maturing to https://lifecycle.r-lib.org/articles/stages.html .

## Test environments
* local OS X install, R 4.0.3
* win-builder (devel and release)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new submission.
