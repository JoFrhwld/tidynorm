## Resubmission

This is a resubmission.
In this version I have:

-   Rewritten the package description.
-   Removed all space+newline sequences.
-   Changed all `T`, `F` to `TRUE`, `FALSE`.
-   Reorganized the documentation of S3 methods.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Description field

-   The initial description sentence has been reworded.
-   All space+newline sequences have been replaced with simple newlines
-   References to methods are identified.
-   The following words are not misspelled:
    -   Fabricius
    -   Gopal
    -   Lobanov
    -   Nearey
    -   Syrdal
    -   formant

## TRUE and FALSE

All `T` values have been rewritten as `TRUE`, and all `F` values have been rewritten as `FALSE`.

## S3 Methods

The following S3 methods are now documented with their respective S3 generic, which have `\value` fields

-   `dct.numeric`
-   `dct.matrix`
-   `idct.numeric`
-   `idct.matrix`
