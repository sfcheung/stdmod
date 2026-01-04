# Conditional Effects

Compute the conditional effects in a moderated regression model.

## Usage

``` r
cond_effect(
  output,
  x = NULL,
  w = NULL,
  w_method = c("sd", "percentile"),
  w_percentiles = c(0.16, 0.5, 0.84),
  w_sd_to_percentiles = NA,
  w_from_mean_in_sd = 1,
  w_values = NULL
)

cond_effect_boot(
  output,
  x = NULL,
  w = NULL,
  ...,
  conf = 0.95,
  nboot = 100,
  boot_args = NULL,
  save_boot_est = TRUE,
  full_output = FALSE,
  do_boot = TRUE
)
```

## Arguments

- output:

  The output from [`stats::lm()`](https://rdrr.io/r/stats/lm.html). It
  can also accept the output from
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  or
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).

- x:

  The focal variable (independent variable), that is, the variable with
  its effect on the outcome variable (dependent) being moderated. It
  must be a numeric variable.

- w:

  The moderator. Both numeric variables and categorical variables
  (character or factor) are supported.

- w_method:

  How to define "low", "medium", and "high" for the moderator levels.
  Default is in terms of mean and standard deviation (SD) of the
  moderator, `"sd"`: "low", "medium", and "high" are one SD below mean,
  mean, and one SD above mean, respectively. If equal to `"percentile"`,
  then percentiles of the moderator in the dataset are used: "low",
  "medium", and "high" are 16th, 50th (median), and 84th percentiles,
  respectively. Ignored if `w` is categorical.

- w_percentiles:

  If `w_method` is `"percentile"`, then this argument specifies the
  three percentiles to be used, divided by 100. It must be a vector of
  two numbers. The default is `c(.16, .50, .84)`, the 16th, 50th, and
  84th percentiles, which corresponds approximately to one SD below and
  above mean in a normal distribution, respectively. Ignored if `w` is
  categorical.

- w_sd_to_percentiles:

  If `w_method` is `"percentile"` and this argument is set to a number,
  this number will be used to to determine the percentiles to be used.
  The lower percentile is the percentile in a normal distribution that
  is `w_sd_to_percentiles` SD below the mean. The upper percentile is
  the percentile in a normal distribution that is `w_sd_to_percentiles`
  SD above the mean. Therefore, if `w_sd_to_percentiles` is set to 1,
  then the lower and upper percentiles are 16th and 84th, respectively.
  Default is `NA`.

- w_from_mean_in_sd:

  How many SD from mean is used to define "low" and "high" for the
  moderator. Default is 1. Ignored if `w` is categorical.

- w_values:

  The values of `w` to be used. Default is `NULL`. If a numeric vector
  is supplied, these values will be used to compute the conditional
  effects. Other arguments on generating levels are ignored. Note that,
  if `w` has been standardized or centered, these values are for the
  standardized or centered `w`. The values will always be sorted. This
  argument is ignored if `w` is categorical.

- ...:

  Arguments to be passed to `cond_effect()`.

- conf:

  The level of confidence for the confidence interval. Default is .95,
  to get 95% confidence intervals.

- nboot:

  The number of bootstrap samples. Default is 100.

- boot_args:

  A named list of arguments to be passed to
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html). Default is
  `NULL`.

- save_boot_est:

  If `TRUE`, the default, the bootstrap estimates will be saved in the
  element `boot_est` of the output.

- full_output:

  Whether the full output from
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) will be
  returned. Default is `FALSE`. If `TRUE`, the full output from
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) will be saved
  in the element `boot_out` of the output.

- do_boot:

  Whether bootstrapping confidence intervals will be formed. Default is
  `TRUE`. If `FALSE`, all arguments related to bootstrapping will be
  ignored.

## Value

`cond_effect()` returns a data-frame-like object of the conditional
effects. The class is `cond_effect` and the print method will print
additional information of the conditional effects. Additional
information is stored in the following attributes:

- `call`: The original call.

- `output`: The `output` object, such as the output from
  [`lm()`](https://rdrr.io/r/stats/lm.html).

- `x`, `y`, and `w`: The three variables used to compute the conditional
  effects: focal variable (`x`), outcome variable (`y`), and the
  moderator (`w`).

- `w_method`: The method used to determine the values of the moderator
  at the selected levels.

- `w_percentiles` The percentiles to use if `w_method` = `"percentile"`.

- `w_sd_to_percentiles`: If not equal to `NA`, this is a scalar, the
  number of standard deviation from the mean used to determine the
  percentiles for the "low" and "high" levels of the moderator.

- `w_from_mean_in_sd`: The number of SD above or below the mean, for
  determining the "low" and "high" levels of the moderator if `w_method`
  is `"sd"`.

- `w_empirical_percentiles`: The actual percentile levels in the dataset
  for the selected levels of the moderator. A numeric vector.

- `w_empirical_z`: The actual distance from the mean, in SD, of each
  selected level of the moderator. A numeric vector.

- `y_standardized`, `x_standardized`, and `w_standardized`: Each of them
  is a logical scalar, indicating whether the outcome variable, focal
  variable, and moderator are standardized.

`cond_effect_boot()` also returns a data-frame-like object of the
conditional effects of the class `cond_effect`, with additional
information from the bootstrapping stored in these attributes:

- `boot_ci`: A data frame of the bootstrap confidence intervals of the
  conditional effects.

- `nboot`: The number of bootstrap samples requested.

- `conf`: The level of confidence, in proportion.

- `boot_est`: A matrix of the bootstrap estimates of the conditional
  effects. The number of rows equal to `nboot`, and the number of
  columns equal to the number of levels of the moderator.

- `cond_effect_boot_call`: The call to `cond_effect_boot()`.

- `boot_out`: If available, the original output from
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html).

## Details

`cond_effect()` uses the centering approach to find the conditional
effect of the focal variable. For each level of the moderator, the value
for this level is subtracted from the moderator scores, and the model is
fitted to the modified data. The coefficient of the focal variable is
then the conditional effect of the focal variable when the moderator's
score is equal this value.

`cond_effect_boot()` function is a wrapper of `cond_effect()`. It calls
`cond_effect()` once for each bootstrap sample, and then computes the
nonparametric bootstrap percentile confidence intervals (Cheung, Cheung,
Lau, Hui, & Vong, 2022). If the output object is the output of
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
or
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
in which mean-centering and/or standardization have been conducted, they
will be repeated in each bootstrap sample. Therefore, like
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
it can be used for form nonparametric bootstrap confidence intervals for
standardized effects, though `cond_effect_boot()` does this for the
standardized conditional effects.

This function ignores bootstrapping done by
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).
It will do its own bootstrapping.

If `do_boot` is `FALSE`, then the object it returns is identical to that
by `cond_effect()`.

This function intentionally does not have an argument for setting the
seed for random number. Users are recommended to set the seed, e.g.,
using [`set.seed()`](https://rdrr.io/r/base/Random.html) before calling
it, to ensure reproducibility.

## Functions

- `cond_effect_boot()`: A wrapper of `cond_effect()` that forms
  nonparametric bootstrap confidence intervals.

## Author

Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>

## Examples

``` r
# Load a sample data set

dat <- test_x_1_w_1_v_1_cat1_n_500

# Do a moderated regression by lm
lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
summary(lm_raw)
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2146.0  -431.9   -25.0   411.2  2309.3 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)  308.767   4075.066   0.076   0.9396  
#> iv            52.760    271.242   0.195   0.8459  
#> mod            5.127     40.772   0.126   0.9000  
#> v1           -12.760     10.174  -1.254   0.2104  
#> cat1gp2     -158.673     71.834  -2.209   0.0276 *
#> cat1gp3      -43.166     75.283  -0.573   0.5666  
#> iv:mod         3.416      2.709   1.261   0.2080  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 665 on 493 degrees of freedom
#> Multiple R-squared:  0.6352, Adjusted R-squared:  0.6307 
#> F-statistic:   143 on 6 and 493 DF,  p-value: < 2.2e-16
#> 
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

lm_std <- std_selected(lm_raw, to_standardize = ~ iv + mod)
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

# Categorical moderator
lm_cat <- lm(dv ~ iv*cat1 + v1, dat)
summary(lm_cat)
#> 
#> Call:
#> lm(formula = dv ~ iv * cat1 + v1, data = dat)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -2457.67  -506.03     3.46   437.95  2738.18 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   979.20     459.19   2.132   0.0335 *  
#> iv            391.03      29.25  13.370   <2e-16 ***
#> cat1gp2      -845.49     584.85  -1.446   0.1489    
#> cat1gp3       259.55     620.76   0.418   0.6760    
#> v1            -19.36      11.00  -1.759   0.0791 .  
#> iv:cat1gp2     43.28      38.44   1.126   0.2608    
#> iv:cat1gp3    -21.22      41.08  -0.516   0.6058    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 721.3 on 493 degrees of freedom
#> Multiple R-squared:  0.5707, Adjusted R-squared:  0.5655 
#> F-statistic: 109.2 on 6 and 493 DF,  p-value: < 2.2e-16
#> 
cond_effect(lm_cat, x = iv, w = cat1)
#> The effects of iv on dv, conditional on cat1:
#> 
#>  Level cat1 iv Effect   S.E.      t     p Sig
#>    gp1  gp1   391.026 29.246 13.370 0.000 ***
#>    gp2  gp2   434.302 24.937 17.416 0.000 ***
#>    gp3  gp3   369.807 28.858 12.815 0.000 ***
#> 
#> 
#> The regression model:
#> 
#>  dv ~ iv * cat1 + v1


# Load a sample data set

dat <- test_x_1_w_1_v_1_cat1_n_500

# Do a moderated regression by lm
lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
summary(lm_raw)
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2146.0  -431.9   -25.0   411.2  2309.3 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)  
#> (Intercept)  308.767   4075.066   0.076   0.9396  
#> iv            52.760    271.242   0.195   0.8459  
#> mod            5.127     40.772   0.126   0.9000  
#> v1           -12.760     10.174  -1.254   0.2104  
#> cat1gp2     -158.673     71.834  -2.209   0.0276 *
#> cat1gp3      -43.166     75.283  -0.573   0.5666  
#> iv:mod         3.416      2.709   1.261   0.2080  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 665 on 493 degrees of freedom
#> Multiple R-squared:  0.6352, Adjusted R-squared:  0.6307 
#> F-statistic:   143 on 6 and 493 DF,  p-value: < 2.2e-16
#> 

lm_std <- std_selected(lm_raw, to_standardize = ~ iv + mod)
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

# Form nonparametric bootstrap confidence intervals
# Use 2000 or even 5000 for nboot in real research
out <- cond_effect_boot(lm_std, x = iv, w = mod, nboot = 50)
out
#> The effects of iv on dv, conditional on mod:
#> 
#>   Level    mod iv Effect CI Lower CI Upper   S.E.      t     p Sig
#>    High  1.000   841.990  745.363  969.398 42.468 19.826 0.000 ***
#>  Medium  0.000   806.878  731.334  882.046 29.942 26.948 0.000 ***
#>     Low -1.000   771.767  689.364  833.522 39.251 19.662 0.000 ***
#> 
#> [CI Lower, CI Upper] shows the 95% nonparametric bootstrap confidence
#> interval(s) (based on 50 bootstrap samples).
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
```
