# Sample Dataset: A Path Model With A Moderator

For testing. Generated from the following model.

    mod <-
    "
    med ~ iv + mod + iv:mod + cov1
    dv ~ med + cov2
    "

## Usage

``` r
test_mod1
```

## Format

A data frame with 300 rows and 6 variables:

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
