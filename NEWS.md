# esmpack 0.1-19 (2023-11-29)

- added `calc.fun()` for computing arbitrary summary statistics for each subject

- in `lagvar()`, added `time` argument (that allows for computing the time difference between lagged observations) and made the examples for this function more complete

- in `lagvar()`, moved `id` argument to the second position and made `obs` argument optional

- added `mvvar()` and `rename()` functions

- added `aggreg()` function

- added `nsub()`, `calc.mean()`, and `calc.mcent()` functions

- added `calc.nomiss()` for computing the number of non-missing values for a particular variable

- added `check.nodup()` to check if a variable has no duplicated values for each subject

- added `check.nomiss()` and updated `check.timeinvar()` to provide more output options

- started using Travis CI (https://travis-ci.org/wviechtb/esmpack)

- started on pkgdown website

- added `check.timeinvar()` and `get.timeinvar()` functions

- added `combitems()` and `lagvar()` functions

- initial setup for GitHub
