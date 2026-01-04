# Print a 'stdmod_lavaan' Class Object

Print the output of
[`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md).

## Usage

``` r
# S3 method for class 'stdmod_lavaan'
print(x, conf = 0.95, nd = 3, ...)
```

## Arguments

- x:

  The output of
  [`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md).

- conf:

  If nonparametric bootstrapping has been conducted by
  [`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md),
  this is the level of confidence in proportion (.95 denotes 95%), of
  the confidence interval. Default is .95.

- nd:

  The number of digits to be printed.

- ...:

  Optional arguments. Ignored.

## Value

`x` is returned invisibly.

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

# Compute the standardized moderation effect
out_noboot <- stdmod_lavaan(fit = fit,
                            x = "iv",
                            y = "med",
                            w = "mod",
                            x_w = "iv:mod")
out_noboot
#> 
#> Call:
#> stdmod_lavaan(fit = fit, x = "iv", y = "med", w = "mod", x_w = "iv:mod")
#> 
#>                  Variable
#> Focal Variable         iv
#> Moderator             mod
#> Outcome Variable      med
#> Product Term       iv:mod
#> 
#>              lhs op    rhs   est    se      z pvalue ci.lower ci.upper
#> Original     med  ~ iv:mod 0.257 0.025 10.169      0    0.208    0.307
#> Standardized med  ~ iv:mod 0.440    NA     NA     NA       NA       NA

# Compute the standardized moderation effect and
# its percentile confidence interval based on nonparametric bootstrapping
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

out_boot
#> 
#> Call:
#> stdmod_lavaan(fit = fit, x = "iv", y = "med", w = "mod", x_w = "iv:mod", 
#>     boot_ci = TRUE)
#> 
#>                  Variable
#> Focal Variable         iv
#> Moderator             mod
#> Outcome Variable      med
#> Product Term       iv:mod
#> 
#>              lhs op    rhs   est    se     z pvalue ci.lower ci.upper
#> Original     med  ~ iv:mod 0.257 0.033 7.725      0    0.173    0.297
#> Standardized med  ~ iv:mod 0.440    NA    NA     NA    0.316    0.520
#> 
#> Confidence interval of standardized moderation effect:
#> - Level of confidence: 95%
#> - Bootstrapping Method: Nonparametric
#> - Type: Percentile
#> - Number of bootstrap samples requests: 
#> - Number of bootstrap samples with valid results: 50
#> 
#> NOTE: Bootstrapping conducted by the method in 0.2.7.5 or later. To use
#> the method in the older versions for reproducing previous results, set
#> 'use_old_version' to 'TRUE'.

```
