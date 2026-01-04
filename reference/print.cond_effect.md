# Print a 'cond_effect' Class Object

Print the output of
[`cond_effect()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
or
[`cond_effect_boot()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md).

## Usage

``` r
# S3 method for class 'cond_effect'
print(
  x,
  nd = 3,
  nd_stat = 3,
  nd_p = 3,
  title = TRUE,
  model = TRUE,
  level_info = TRUE,
  standardized = TRUE,
  boot_info = TRUE,
  table_only = FALSE,
  t_ci = FALSE,
  t_ci_level = 0.95,
  ...
)
```

## Arguments

- x:

  The output of
  [`cond_effect()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
  or
  [`cond_effect_boot()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md).

- nd:

  The number of digits for the variables.

- nd_stat:

  The number of digits for test statistics (e.g., *t*).

- nd_p:

  The number of digits for *p*-values.

- title:

  If `TRUE`, print a title. Default is `TRUE`.

- model:

  If `TRUE`, print the regression model. Default is `TRUE`.

- level_info:

  If `TRUE`, print information for interpreting the levels of the
  moderator, such as the values of the levels and distance from the
  mean. Default is `TRUE`.

- standardized:

  If `TRUE` and one or more variables are standardized, report it.
  Default is `TRUE`.\`

- boot_info:

  If `TRUE` and bootstrap estimates are in `x`, print information about
  the bootstrapping, such as the number of bootstrap samples. Default is
  `TRUE`.

- table_only:

  If `TRUE`, will suppress of other elements except for the table of
  conditional effects. Override arguments such as `title`, `model`, and
  `level_info`.

- t_ci:

  If `TRUE`, will print the confidence intervals based on t statistics.
  These confidence intervals should not be used if some variables are
  standardized.

- t_ci_level:

  The level of confidence of the confidence intervals based on t
  statistics. Default is .95.

- ...:

  Additional arguments. Ignored by this function.

## Value

`x` is returned invisibility.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Load a sample data set

dat <- test_x_1_w_1_v_1_cat1_n_500

# Do a moderated regression by lm
lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)

cond_effect(lm_raw, x = iv, w = mod)
#> The effects of iv on dv, conditional on mod:
#> 
#>   Level     mod iv Effect   S.E.      t     p Sig
#>    High 105.436   412.911 20.827 19.826 0.000 ***
#>  Medium 100.395   395.693 14.684 26.948 0.000 ***
#>     Low  95.354   378.474 19.249 19.662 0.000 ***
#> 
#> 
#> The regression model:
#> 
#>  dv ~ iv * mod + v1 + cat1
#> 
#> Interpreting the levels of mod:
#> 
#>   Level     mod % Below From Mean (in SD)
#>    High 105.436   84.00              1.00
#>  Medium 100.395   47.40              0.00
#>     Low  95.354   17.20             -1.00
#> 
#> - % Below: The percent of cases equal to or less than a level.
#> - From Mean (in SD): Distance of a level from the mean, in standard
#>   deviation (+ve above, -ve below).

lm_std <- std_selected(lm_raw, to_scale = ~ iv + mod,
                               to_center = ~ iv + mod)

cond_effect(lm_std, x = iv, w = mod)
#> The effects of iv on dv, conditional on mod:
#> 
#>   Level    mod iv Effect   S.E.      t     p Sig
#>    High  1.000   841.990 42.468 19.826 0.000 ***
#>  Medium  0.000   806.878 29.942 26.948 0.000 ***
#>     Low -1.000   771.767 39.251 19.662 0.000 ***
#> 
#> 
#> The regression model:
#> 
#>  dv ~ iv * mod + v1 + cat1
#> 
#> Interpreting the levels of mod:
#> 
#>   Level    mod % Below From Mean (in SD)
#>    High  1.000   84.00              1.00
#>  Medium  0.000   47.40              0.00
#>     Low -1.000   17.20             -1.00
#> 
#> - % Below: The percent of cases equal to or less than a level.
#> - From Mean (in SD): Distance of a level from the mean, in standard
#>   deviation (+ve above, -ve below).
#> 
#> Note:
#> 
#> - The variable(s) iv, mod is/are standardized.
#> - One or more variables are scaled by SD or standardized. OLS standard
#>   errors and confidence intervals may be biased for their coefficients.
#>   Please use `cond_effect_boot()`.
```
