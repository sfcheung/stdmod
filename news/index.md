# Changelog

## stdmod 0.2.11.1

### Miscellaneous

- Updated vignettes to use precomputed files, instead of using stored
  objects, when bootstrapping is used. This also addressed Issue 127 on
  GitHub. (0.2.11.1)

## stdmod 0.2.11

CRAN release: 2024-09-22

### Miscellaneous

- Updated tests for `testthat` 3rd Edition. (0.2.10.2)

### Bug Fixes

- In
  [`plotmod()`](https://sfcheung.github.io/stdmod/reference/plotmod.md),
  labels regarding SDs will no longer be displayed when `w_values` is
  set. (0.2.10.1)

## stdmod 0.2.10

CRAN release: 2024-02-22

### New Features

- Improved the printout of the
  [`summary()`](https://rdrr.io/r/base/summary.html) of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  and
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  outputs. It now prints the R-squared increase of the highest order
  term, as well as the *F* test for the increase, if the model has one
  and only one highest order term (e.g., an interaction term). (0.2.9.1)

- Added the argument `w_values` to
  [`cond_effect()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
  and `plolmod()`. Users can specify the values of the moderator (`w`)
  to be used to compute the conditional effects. (0.2.9.2)

### Miscellaneous

- Revised
  [`update.std_selected()`](https://sfcheung.github.io/stdmod/reference/update.std_selected.md).
  Though still not recommended, it should now work more reliably if it
  needs to be called. (0.2.9.1)

## stdmod 0.2.9

CRAN release: 2023-09-11

- Fixed the issue with `stdmod-package`. (0.2.8.9001)
- Improved the printout of the
  [`summary()`](https://rdrr.io/r/base/summary.html) of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  and
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  outputs. Small numbers are rounded to prevent the use of scientific
  notation, and small *p*-values can be printed in formats like
  *p*\<.001. Users can also control the number of digits in the
  printout. See the help page of
  [`print.summary.std_selected()`](https://sfcheung.github.io/stdmod/reference/print.summary.std_selected.md)
  to learn more about new arguments (0.2.8.9002).
- Finalized to 0.2.9. (0.2.9)

## stdmod 0.2.8

CRAN release: 2023-06-24

- Added an R CMD check for noSuggests. (0.2.7.1)
- Removed `dplyr` from the tests and Suggests. (0.2.7.2)
- The tests using `visreg` will be skipped if `visreg` is not installed.
  (0.2.7.3)
- Add a note in the printout of a `cond_effect`-class object and the
  summary of a `std_selected`-class object. If one or more variables are
  standardized but bootstrapping is not requested, users will be
  recommended to use
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).
  (0.2.7.4)
- [`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md)
  switched to the bootstrapping algorithm used by
  [`lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html). It also
  updated to allow for partial standardization. To use the older
  algorithm, set `use_old_version()` to `TRUE`. (0.2.7.5)
- Finalized to 0.2.8. (0.2.8)

## stdmod 0.2.7

CRAN release: 2023-03-26

- Updated pkgdown site to bootstrap 5. (0.2.6.1)
- Fixed a missing whitepace in the note of the
  [`print()`](https://rdrr.io/r/base/print.html) method of the
  [`summary()`](https://rdrr.io/r/base/summary.html) output of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).
  (0.2.6.2)
- Added `to_standardize` to
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  and
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).
  (0.2.6.3)
- Fixed a bug with
  [`confint.std_selected()`](https://sfcheung.github.io/stdmod/reference/confint.std_selected.md)
  when `type = "lm"` and bootstrapping is requested. Should not be an
  issue because *t*-based CIs should not be used when bootstrapping is
  requested. This option is just for testing. (0.2.6.4)
- Updated examples to use `to_standardize` or mention it as a shortcut.
  (0.2.6.5)
- Updated vignettes to introduce `to_standardize`. (0.2.6.6)
- Fixed a link in a vignette. (0.2.6.7)
- Finalized to 0.2.7. (0.2.7)

## stdmod 0.2.6

CRAN release: 2023-03-10

- Improved the printout of the
  [`summary()`](https://rdrr.io/r/base/summary.html) of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  and
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  outputs. (0.2.4.9001).
- Fixed deprecated functions and arguments in `ggplot2`. (0.2.4.9002)
- Fixed a problem with wrapping long lines in the printout of the
  [`summary()`](https://rdrr.io/r/base/summary.html) of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).
  (0.2.4.9003)
- Update GitHub actions.
- Fixed a missing link in an Rd file in old macOS machines. (0.2.6)
- Use [`bibentry()`](https://rdrr.io/r/utils/bibentry.html) in CITATION.
  (0.2.6)

## stdmod 0.2.4

- Fixed a bug in
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md):
  It now works correctly when a variable in the data frame is a factor.
  (0.2.0.1)
- Added [`confint()`](https://rdrr.io/r/stats/confint.html) and
  [`coef()`](https://rdrr.io/r/stats/coef.html) methods for
  `cond_effect`-class objects.
  [`confint()`](https://rdrr.io/r/stats/confint.html) can return
  confidence intervals based on *t* statistics, which are appropriate in
  some situations. (0.2.2)
- [`print()`](https://rdrr.io/r/base/print.html) method for
  `cond_effect`-class objects can print confidence intervals based on
  *t* statistics. (0.2.2)
- Added `do_boot` to
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).
  If set to `FALSE`, it will not do bootstrapping. (0.2.3)
- [`cond_effect_boot()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
  will disable bootstrapping in the original call if the output is
  generated by
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
  to avoid redundant bootstrapping inside bootstrapping. (0.2.3)
- Added `do_boot` to
  [`cond_effect_boot()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md).
  If set to `FALSE`, it will not do bootstrapping. (0.2.4)

## stdmod 0.2.0.0

CRAN release: 2022-09-21

- Changed the default of
  [`confint()`](https://rdrr.io/r/stats/confint.html) and
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) for `std_selected`-class
  object. If bootstrap CIs are requested, then bootstrap CIs and VCOV
  based on bootstrapping should be returned. (0.2.0.0)

## stdmod 0.1.7.7

- Updated references. (0.1.7.5)
- Minor changes due to Roxygen updates. (0.1.7.5)
- Added R CMD Check action (0.1.7.6)
- Minor changes due to Roxygen updates. (0.1.7.7)
- Fixed a typo in CITATION

## stdmod 0.1.7.4

CRAN release: 2022-08-07

(All major changes after 0.1.7.1)

- Fixed a bug in
  [`plotmod()`](https://sfcheung.github.io/stdmod/reference/plotmod.md).
  It now correctly handles more than two levels when `w_method` is set
  to`"percentile"`. (0.1.7.2, 0.1.7.3)
- Fixed a typo in DESCRIPTION. (0.1.7.4)

## stdmod 0.1.7.1

CRAN release: 2022-05-11

(All major changes after 0.1.5)

- Added
  [`plotmod()`](https://sfcheung.github.io/stdmod/reference/plotmod.md)
  for plotting moderation effects. This function will check whether a
  variable is standardized. If yes, will note this in the plot.
- [`plotmod()`](https://sfcheung.github.io/stdmod/reference/plotmod.md)
  can also plot a Tumble graph (Bodner, 2016) if `graph_type` is set to
  `"tumble"`.
- Updated vignettes to use
  [`plotmod()`](https://sfcheung.github.io/stdmod/reference/plotmod.md)
  instead of
  [`visreg::visreg()`](https://pbreheny.github.io/visreg/reference/visreg.html).
- Added
  [`cond_effect()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
  for computing conditional effects. This function will check which
  variable(s) is/are standardized. If yes, will note this in the
  printout.
- Added
  [`cond_effect_boot()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md),
  a wrapper of
  [`cond_effect()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
  that can form nonparametric bootstrap confidence intervals for the
  conditional effects, which may be partially or completely
  standardized.
- Updated the print method for the summary of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  and
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).
- Added more vignettes to introduce the new functions.
- [`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md)
  now returns an object of the class `stdmod_lavaan`, with methods
  print, confint, and coef added.

## stdmod 0.1.5

- Changed the column order of the coefficient table in
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  output. Bootstrap confidence intervals are placed next to parameter
  estimates.
- Added [`vcov()`](https://rdrr.io/r/stats/vcov.html) method for
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  output. If bootstrapping is used, it can return the
  variance-covariance matrix of the bootstrap estimates.
- Added [`confint()`](https://rdrr.io/r/stats/confint.html) method for
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  output. If bootstrapping is used, it can return the bootstrap
  percentile confidence intervals if requested.
- Other minor fixes.

## stdmod 0.1.3

- Set up gh-pages and fix dependency in vignette.

## stdmod 0.1.2

- Updated the methods for
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).

## stdmod 0.1.1

- The first version.
