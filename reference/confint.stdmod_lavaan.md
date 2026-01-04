# Confidence Intervals for a 'stdmod_lavaan' Class Object

Return the confidence interval of the standardized moderation effect in
the output of
[`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md).

## Usage

``` r
# S3 method for class 'stdmod_lavaan'
confint(object, parm, level = 0.95, ...)
```

## Arguments

- object:

  The output of
  [`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md).

- parm:

  Ignored. Always return the bootstrap confidence interval of the
  standardized moderation effect.

- level:

  The level of confidence, default is .95, returning the 95% confidence
  interval.

- ...:

  Additional arguments. Ignored by the function.

## Value

A one-row matrix of the confidence intervals.

## Details

If bootstrapping is used to form the confidence interval by
[`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md),
users can request the percentile confidence interval of using the stored
bootstrap estimate.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Load a test data of 500 cases
dat <- test_mod1
library(lavaan)

mod <-
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
fit <- sem(mod, dat)
coef(fit)
#>     med~iv    med~mod med~iv:mod   med~cov1     dv~med    dv~cov2   med~~med 
#>      0.221      0.104      0.257      0.104      0.246      0.191      0.201 
#>     dv~~dv 
#>      0.169 

# Compute the standardized moderation effect and
# its confidence interval based on nonparametric bootstrapping
# Fit the model with bootstrap confidence intervals
# At least 2000 bootstrap samples should be used
# in real research. 50 is used here only for
# illustration.
fit <- sem(mod, dat, se = "boot", bootstrap = 50,
           iseed = 89574)
out_boot <- stdmod_lavaan(fit = fit,
                          x = "iv",
                          y = "med",
                          w = "mod",
                          x_w = "iv:mod",
                          boot_ci = TRUE)
confint(out_boot)
#>                2.5 %    97.5 %
#> med~iv:mod 0.3157125 0.5199081
```
