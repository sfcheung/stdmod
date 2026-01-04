# The 'vcov' Method for a 'std_selected' Class Object

Compute the variance-covariance matrix of estimates in the output of
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
or
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).

## Usage

``` r
# S3 method for class 'std_selected'
vcov(object, type, ...)
```

## Arguments

- object:

  The output of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  or
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).

- type:

  The type of variance-covariance matrix. If set to `"lm"`, returns the
  results of the [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html)
  method for the output of [`lm()`](https://rdrr.io/r/stats/lm.html). If
  set to `"boot"`, the variance-covariance matrix of the bootstrap
  estimates is returned. Default depends on `object`. If bootstrap
  estimates were stored, then the default is `"boot"`. Otherwise, the
  default is `"lm"`.

- ...:

  Arguments to be passed to
  [`stats::vcov()`](https://rdrr.io/r/stats/vcov.html).

## Value

A matrix of the variances and covariances of the parameter estimates.

## Details

If bootstrapping was used to form the confidence intervals, users can
request the variance-covariance matrix of the bootstrap estimates.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Load a sample data set

dat <- test_x_1_w_1_v_1_cat1_n_500
head(dat)
#>         dv       iv       mod        v1 cat1
#> 1 4946.751 12.76737  96.85621 11.756899  gp1
#> 2 6635.081 14.89097 106.25696 11.371237  gp2
#> 3 6060.708 15.24101  97.85852  9.377471  gp2
#> 4 7240.781 16.65782 104.80266 10.508913  gp1
#> 5 5775.759 11.84448  95.85912 15.093480  gp3
#> 6 7725.783 16.31270 100.20561  3.442902  gp2

# Do a moderated regression by lm
lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)

# Standardize all variables except for categorical variables.
# Interaction terms are formed after standardization.
lm_std <- std_selected(lm_raw, to_scale = ~ .,
                               to_center = ~ .)

# VCOV of lm output
vcov(lm_std)
#>               (Intercept)            iv           mod            v1
#> (Intercept)  2.328093e-03 -2.424492e-05 -4.453643e-05  5.904665e-07
#> iv          -2.424492e-05  7.487428e-04 -3.910411e-05 -1.570052e-06
#> mod         -4.453643e-05 -3.910411e-05  7.490437e-04  4.323374e-05
#> v1           5.904665e-07 -1.570052e-06  4.323374e-05  7.466559e-04
#> cat1gp2     -2.327318e-03 -5.104838e-06  9.054077e-05  3.534309e-05
#> cat1gp3     -2.327547e-03  7.491390e-05  2.729951e-05 -5.249772e-05
#> iv:mod      -3.979516e-05  5.489704e-05  4.037895e-05  4.206434e-05
#>                   cat1gp2       cat1gp3        iv:mod
#> (Intercept) -2.327318e-03 -2.327547e-03 -3.979516e-05
#> iv          -5.104838e-06  7.491390e-05  5.489704e-05
#> mod          9.054077e-05  2.729951e-05  4.037895e-05
#> v1           3.534309e-05 -5.249772e-05  4.206434e-05
#> cat1gp2      4.309522e-03  2.323477e-03 -1.319904e-06
#> cat1gp3      2.323477e-03  4.733274e-03  1.279117e-05
#> iv:mod      -1.319904e-06  1.279117e-05  6.477400e-04

# Standardize all variables as in std_selected above, and compute the
# nonparametric bootstrapping percentile confidence intervals.
lm_std_boot <- std_selected_boot(lm_raw,
                                 to_scale = ~ .,
                                 to_center = ~ .,
                                 conf = .95,
                                 nboot = 100)
# In real analysis, nboot should be at least 2000.

# VCOV of bootstrap estimates, default when bootstrap was conducted
vcov(lm_std_boot)
#>               (Intercept)            iv           mod            v1
#> (Intercept)  1.436916e-03 -1.705511e-05  7.363043e-05  2.060856e-04
#> iv          -1.705511e-05  4.390832e-04 -3.211206e-04 -4.446206e-05
#> mod          7.363043e-05 -3.211206e-04  7.906790e-04  7.898140e-05
#> v1           2.060856e-04 -4.446206e-05  7.898140e-05  9.012892e-04
#> cat1gp2     -2.140278e-03 -2.260848e-06  5.620599e-05 -2.770896e-04
#> cat1gp3     -1.927785e-03  7.215458e-05 -2.520011e-04 -2.972410e-04
#> iv:mod      -2.034355e-04  1.313982e-04 -1.398749e-04 -7.203788e-05
#>                   cat1gp2       cat1gp3        iv:mod
#> (Intercept) -2.140278e-03 -1.927785e-03 -2.034355e-04
#> iv          -2.260848e-06  7.215458e-05  1.313982e-04
#> mod          5.620599e-05 -2.520011e-04 -1.398749e-04
#> v1          -2.770896e-04 -2.972410e-04 -7.203788e-05
#> cat1gp2      3.991118e-03  2.030063e-03  2.345365e-04
#> cat1gp3      2.030063e-03  3.642528e-03  2.071899e-04
#> iv:mod       2.345365e-04  2.071899e-04  5.073543e-04

# For OLS VCOV
vcov(lm_std_boot, type = "lm")
#>               (Intercept)            iv           mod            v1
#> (Intercept)  2.328093e-03 -2.424492e-05 -4.453643e-05  5.904665e-07
#> iv          -2.424492e-05  7.487428e-04 -3.910411e-05 -1.570052e-06
#> mod         -4.453643e-05 -3.910411e-05  7.490437e-04  4.323374e-05
#> v1           5.904665e-07 -1.570052e-06  4.323374e-05  7.466559e-04
#> cat1gp2     -2.327318e-03 -5.104838e-06  9.054077e-05  3.534309e-05
#> cat1gp3     -2.327547e-03  7.491390e-05  2.729951e-05 -5.249772e-05
#> iv:mod      -3.979516e-05  5.489704e-05  4.037895e-05  4.206434e-05
#>                   cat1gp2       cat1gp3        iv:mod
#> (Intercept) -2.327318e-03 -2.327547e-03 -3.979516e-05
#> iv          -5.104838e-06  7.491390e-05  5.489704e-05
#> mod          9.054077e-05  2.729951e-05  4.037895e-05
#> v1           3.534309e-05 -5.249772e-05  4.206434e-05
#> cat1gp2      4.309522e-03  2.323477e-03 -1.319904e-06
#> cat1gp3      2.323477e-03  4.733274e-03  1.279117e-05
#> iv:mod      -1.319904e-06  1.279117e-05  6.477400e-04
```
