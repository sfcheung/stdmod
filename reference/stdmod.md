# Standardized Moderation Effect Given an 'lm' Output

Compute the standardized moderation effect in a moderated regression
model.

## Usage

``` r
stdmod(
  lm_out,
  x = NULL,
  w = NULL,
  y = NULL,
  x_rescale = TRUE,
  w_rescale = TRUE,
  y_rescale = TRUE
)

stdmod_boot(
  lm_out,
  ...,
  nboot = 100,
  conf = 0.95,
  boot_args = NULL,
  full_output = FALSE
)
```

## Arguments

- lm_out:

  The output from [`lm()`](https://rdrr.io/r/stats/lm.html).

- x:

  The focal variable, that is, the variable with its effect being
  moderated. If supplied, its standard deviation will be used for
  rescaling. Also called the independent variable in some models.
  Default is `NULL`.

- w:

  The moderator. If supplied, its standard deviation will be used for
  rescaling. Default is `NULL`.

- y:

  The outcome variable (dependent variable) . If supplied, its standard
  deviation will be used for rescaling. Default is NULL.

- x_rescale:

  If `TRUE`, will rescale x by its standard deviation. Default is
  `TRUE`.

- w_rescale:

  If `TRUE`, will rescale w by its standard deviation. Default is
  `TRUE`.

- y_rescale:

  If `TRUE`, will rescale y by its standard deviation. Default is
  `TRUE`.

- ...:

  Parameters to be passed to `stdmod()`.

- nboot:

  The number of bootstrap samples. Default is 100.

- conf:

  The level of confidence for the confidence interval. Default is .95.

- boot_args:

  A named list of arguments to be passed to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). Default is
  `NULL`.

- full_output:

  Whether the full output from
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) is returned.
  Default is `FALSE`.

## Value

`stdmod()` returns a scalar: The standardized moderation effect.

`stdmod_boot()` returns a list with two elements. The element `ci` is a
numeric vector of the bootstrap confidence interval. The element
`boot_out`, if not `NA`, is the output of
[`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html), which is used
to do the bootstrapping.

## Details

Two more general functions,
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
and
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
have been developed and can do what these functions do and more. Users
are recommended to use them instead of `stdmod()` and `stdmod_boot()`.
These two functions will not be updated in the near future.

Nevertheless, if computing the standardized moderation effect and
forming its nonparametric bootstrap interval are all required, then
these functions can still be used.

`stdmod()` computes the standardized moderation effect given an
[`lm()`](https://rdrr.io/r/stats/lm.html) output using the formula from
Cheung, Cheung, Lau, Hui, and Vong (2022). Users specify the moderator,
the focal variable (the variable with its effect on the outcome variable
moderated), the outcome variable (dependent variable) , and the
corresponding standardized moderation effect. Users can also select
which variable(s) will be standardized.

`stdmod_boot()` is a wrapper of `stdmod()`. It computes the
nonparametric bootstrap confidence interval of the standardized
moderation effect, as suggested by Cheung, Cheung, Lau, Hui, and Vong
(2022), given the output of [`lm()`](https://rdrr.io/r/stats/lm.html)

Percentile interval from
[`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html) is
returned by this function. If other types of confidence intervals are
desired, set `full_output = TRUE` and use
[`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html) on the
element `boot_out` in the output of this function.

## Functions

- `stdmod()`: The base function for computing standardized moderation
  effect

- `stdmod_boot()`: A wrapper of `stdmod()` that computes the
  nonparametric bootstrap confidence interval of the standardized
  moderation effect.

## References

Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
(2022) Improving an old way to measure moderation effect in standardized
units. *Health Psychology*, *41*(7), 502-505.
[doi:10.1037/hea0001188](https://doi.org/10.1037/hea0001188)

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Load a test data of 500 cases

dat <- test_x_1_w_1_v_2_n_500

# Do regression as usual:
lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
summary(lm_raw)
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + v2, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1650.87  -513.21     6.61   460.18  2189.74 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept) 2814.565   4684.595   0.601   0.5482  
#> iv          -164.942    304.407  -0.542   0.5882  
#> mod          -17.223     46.420  -0.371   0.7108  
#> v1           -12.157     10.684  -1.138   0.2557  
#> v2            -4.284      6.290  -0.681   0.4962  
#> iv:mod         5.515      3.038   1.815   0.0701 .
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 708.1 on 494 degrees of freedom
#> Multiple R-squared:  0.5862, Adjusted R-squared:  0.582 
#> F-statistic:   140 on 5 and 494 DF,  p-value: < 2.2e-16
#> 

# The standard deviations of iv, dv, and mod:
sds <- apply(dat, 2, sd)
sds
#>          dv          iv         mod          v1          v2 
#> 1095.222413    1.991458    5.004342    2.970378    5.064710 

# Compute the standardized moderation effect:
stdmod_xyw <- stdmod(lm_raw, x = iv, y = dv, w = mod)
stdmod_xyw
#>     iv:mod 
#> 0.05018003 
# By default, all three variables will be standardized.

# Check against self-computed standardized moderation effect:
coef(lm_raw)["iv:mod"] * sds["iv"] * sds["mod"] / sds["dv"]
#>     iv:mod 
#> 0.05018003 

# Standardize only the iv, i.e., do not standardized dv and the moderator:
stdmod_x <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = TRUE,  y_rescale = FALSE, w_rescale = FALSE)
stdmod_x
#>   iv:mod 
#> 10.98212 
# Check against self-computed moderation effect with only iv standardized:
coef(lm_raw)["iv:mod"] * sds["iv"]
#>   iv:mod 
#> 10.98212 


dat <- test_x_1_w_1_v_2_n_500
# Do regression as usual:
lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)

# Compute the standardized moderation effect.
# Form its confidence interval by nonparametric bootstrapping.
set.seed(85740917)
stdmod_xyw_boot <- stdmod_boot(lm_raw, x = iv, w = mod, y = dv, nboot = 100)
# In real analysis, nboot should be at least 2000.

# Print the ci
stdmod_xyw_boot$ci
#> [1] 0.01014322 0.10050906

# Repeat the analysis but keep the results from boot:
set.seed(85740917)
stdmod_xyw_boot <- stdmod_boot(lm_raw, x = iv, w = mod, y = dv,
                                 nboot = 200, full_output = TRUE)
# In real analysis, nboot should be at least 2000.

# Print the 95% percentile confidence interval
stdmod_xyw_boot$ci
#> [1] 0.005379417 0.103307077

```
