# Sample Dataset: A Path Model With A Moderator

For testing the handling of warnings in
[`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md).
Generated from the following model. `dv` has about 88% missing. A
warning on missing data will be raised in some bootstrap samples.

    mod <-
    "
    med ~ iv + mod + iv:mod + cov1
    dv ~ med + cov2
    "

## Usage

``` r
test_mod3_miss
```

## Format

A data frame with 500 rows and 6 variables:

- dv:

  Dependent variable, continuous

- iv:

  Independent variable, continuous

- med:

  Mediator, continuous

- mod:

  Moderator, continuous

- cov1:

  Covariate, continuous

- cov2:

  Covariate, continuous
