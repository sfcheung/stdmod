# Standardized Moderation Effect in a 'stdmod_lavaan' Class Object

Return the estimate of the standardized moderation effect in the output
of
[`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md).

## Usage

``` r
# S3 method for class 'stdmod_lavaan'
coef(object, ...)
```

## Arguments

- object:

  The output of
  [`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md).

- ...:

  Optional arguments. Ignored by the function.

## Value

A scalar: The estimate of the standardized moderation effect.

## Details

It just extracts and returns the element `stdmod`.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Load a test data of 500 cases
dat <- test_mod1
library(lavaan)
#> This is lavaan 0.6-21
#> lavaan is FREE software! Please report any bugs.

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

# Compute the standardized moderation effect
out_noboot <- stdmod_lavaan(fit = fit,
                            x = "iv",
                            y = "med",
                            w = "mod",
                            x_w = "iv:mod")
coef(out_noboot)
#> [1] 0.4397874

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
coef(out_boot)
#> [1] 0.4397874
```
