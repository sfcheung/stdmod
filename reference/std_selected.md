# Standardize Variables in a Regression Model

Standardize, mean center, or scale by standard deviation selected
variables in a regression model and refit the model

## Usage

``` r
std_selected(lm_out, to_scale = NULL, to_center = NULL, to_standardize = NULL)

std_selected_boot(
  lm_out,
  to_scale = NULL,
  to_center = NULL,
  to_standardize = NULL,
  conf = 0.95,
  nboot = 100,
  boot_args = NULL,
  save_boot_est = TRUE,
  full_output = FALSE,
  do_boot = TRUE
)
```

## Arguments

- lm_out:

  The output from [`lm()`](https://rdrr.io/r/stats/lm.html).

- to_scale:

  The terms to be rescaled by standard deviation, specified by a formula
  as in [`lm()`](https://rdrr.io/r/stats/lm.html). For example, if the
  terms to be scaled are `x1` and `x3`, use `~ x1 + x3`. No need to
  specify the interaction term. To scale the outcome variable, list it
  on the *right hand side* as a predictor. Specify only the original
  variables. If `NULL`, then no terms will be rescaled by their standard
  deviations. Variables that are not numeric will be ignored. Default is
  `NULL`.

- to_center:

  The terms to be mean centered, specified by a formula as in
  [`lm()`](https://rdrr.io/r/stats/lm.html). For example, if the terms
  to be centered is `x1` and `x3`, use `~ x1 + x3`. No need to specify
  the interaction term. To center the outcome variable, list it on the
  *right hand side* as a predictor. Specify only the original variables.
  If `NULL`, then no term will be centered. Default is `NULL`.

- to_standardize:

  The terms to be standardized, specified by a formula as in
  [`lm()`](https://rdrr.io/r/stats/lm.html). For example, if the terms
  to be standardized is `x1` and `x3`, use `~ x1 + x3`. No need to
  specify the interaction term. To standardize the outcome variable,
  list it on the *right hand side* as a predictor. Specify only the
  original variables. This is a shortcut to `to_center` and `to_scale`.
  Listing a variable in `to_standardize` is equivalent to listing this
  variable in both `to_center` and `to_scale`. Default is `NULL`.

- conf:

  The level of confidence for the confidence interval. Default is .95.

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
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) is returned.
  Default is `FALSE`. If `TRUE`, the full output from
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html) will be saved
  in the element `boot_out` of the output.

- do_boot:

  Whether bootstrapping confidence intervals will be formed. Default is
  `TRUE`. If `FALSE`, all arguments related to bootstrapping will be
  ignored.

## Value

The updated [`lm()`](https://rdrr.io/r/stats/lm.html) output, with the
class `std_selected` added. It will be treated as a usual
[`lm()`](https://rdrr.io/r/stats/lm.html) object by most functions.
These are the major additional element in the list:

- `scaled_terms`: If not `NULL`, a character vector of the variables
  scaled.

- `centered_terms`: If not `NULL`, a character vector of the variables
  mean-centered.

- `scaled_by`: A numeric vector of the scaling factors for all the
  variables in the model. The value is 1 for terms not scaled.

- `centered_by`: A numeric vector of the numbers used for centering for
  all the variables in the model. The value is 0 for terms not centered.

- `std_selected_call`: The original call.

- `lm_out_call`: The call in `lm_out`.

Like `std_selected()`, `std_selected_boot()` returns the updated
[`lm()`](https://rdrr.io/r/stats/lm.html) output, with the class
`std_selected` added. The output of `std_selected_boot()` contain these
additional elements in the list:

- `boot_ci`: A data frame of the bootstrap confidence intervals of the
  regression coefficient.

- `nboot`: The number of bootstrap samples requested.

- `conf`: The level of confidence, in proportion.

- `boot_est`: A matrix of the bootstrap estimates of the regression
  coefficients. The number of rows equal to `nboot`, and the number of
  columns equal to the number of terms in the regression model.

- `std_selected_boot_call`: The call to `std_selected_boot()`.

- `boot_out`: If available, the original output from
  [`boot::boot()`](https://rdrr.io/pkg/boot/man/boot.html).

## Details

`std_selected()` was originally developed to compute the standardized
moderation effect and the standardized coefficients for other predictors
given an [`lm()`](https://rdrr.io/r/stats/lm.html) output (Cheung,
Cheung, Lau, Hui, & Vong, 2022). It has been extended such that users
can specify which variables in a regression model are to be
mean-centered and/or rescaled by their standard deviations. If the model
has one or more interaction terms, they will be formed after the
transformation, yielding the correct standardized solution for a
moderated regression model. Moreover, categorical predictors will be
automatically skipped in mean-centering and rescaling.

Standardization is conducted when a variable is mean-centered and then
rescaled by its standard deviation. Therefore, if the goal is to get the
standardized solution of a moderated regression, users just instruct the
function to standardize all non-categorical variables in the regression
model.

`std_selected_boot()` is a wrapper of `std_selected()`. It calls
`std_selected()` once for each bootstrap sample, and then computes the
nonparametric bootstrap percentile confidence intervals (Cheung, Cheung,
Lau, Hui, & Vong, 2022).

If `do_boot` is `FALSE`, then the object it returns is identical to that
by `std_selected()`.

This function intentionally does not have an argument for setting the
seed for random number. Users are recommended to set the seed, e.g.,
using [`set.seed()`](https://rdrr.io/r/base/Random.html) before calling
it, to ensure reproducibility.

## Functions

- `std_selected()`: The base function to center or scale selected
  variables in a regression model

- `std_selected_boot()`: A wrapper of `std_selected()` that forms
  nonparametric bootstrap confidence intervals.

## References

Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
(2022) Improving an old way to measure moderation effect in standardized
units. *Health Psychology*, *41*(7), 502-505.
[doi:10.1037/hea0001188](https://doi.org/10.1037/hea0001188)

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

# Mean center mod only
lm_cw <- std_selected(lm_raw, to_center = ~ mod)
summary(lm_cw)
#> 
#> Call to std_selected():
#> std_selected(lm_out = lm_raw, to_center = ~mod)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: mod
#> 
#>      centered_by scaled_by                Note
#> dv         0.000         1                    
#> iv         0.000         1                    
#> mod      100.395         1 Centered (mean = 0)
#> v1         0.000         1                    
#> cat1          NA        NA Nonnumeric         
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat_mod)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2146.0  -431.9   -25.0   411.2  2309.3 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  823.445    249.736  3.2973 0.001047 ** 
#> iv           395.692     14.684 26.9480  < 0.001 ***
#> mod            5.126     40.772  0.1257 0.899992    
#> v1           -12.760     10.174 -1.2542 0.210365    
#> cat1gp2     -158.673     71.834 -2.2089 0.027642 *  
#> cat1gp3      -43.166     75.283 -0.5734 0.566645    
#> iv:mod         3.416      2.709  1.2608 0.207990    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 665 on 493 degrees of freedom
#> 
#> R-squared                : 0.6352
#> Adjusted R-squared       : 0.6307
#> ANOVA test of R-squared  : F(6, 493) = 143.047, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : iv:mod
#> R-squared increase adding this term: 0.0012
#> F test of R-squared increase       : F(1, 493) = 1.5895, p = 0.208
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - One or more variables are scaled by SD or standardized. OLS standard
#>   errors and confidence intervals may be biased for their coefficients.
#>   Please use `std_selected_boot()`.
#> 

# Mean center mod and iv
lm_cwx <- std_selected(lm_raw, to_center = ~ mod + iv)
summary(lm_cwx)
#> 
#> Call to std_selected():
#> std_selected(lm_out = lm_raw, to_center = ~mod + iv)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: mod iv
#> 
#>      centered_by scaled_by                Note
#> dv       0.00000         1                    
#> iv      15.01576         1 Centered (mean = 0)
#> mod    100.39502         1 Centered (mean = 0)
#> v1       0.00000         1                    
#> cat1          NA        NA Nonnumeric         
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat_mod)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2146.0  -431.9   -25.0   411.2  2309.3 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 6765.069    115.857 58.3916  < 0.001 ***
#> iv           395.692     14.684 26.9480  < 0.001 ***
#> mod           56.418      5.941  9.4962  < 0.001 ***
#> v1           -12.760     10.174 -1.2542  0.21037    
#> cat1gp2     -158.673     71.834 -2.2089  0.02764 *  
#> cat1gp3      -43.166     75.283 -0.5734  0.56664    
#> iv:mod         3.416      2.709  1.2608  0.20799    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 665 on 493 degrees of freedom
#> 
#> R-squared                : 0.6352
#> Adjusted R-squared       : 0.6307
#> ANOVA test of R-squared  : F(6, 493) = 143.047, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : iv:mod
#> R-squared increase adding this term: 0.0012
#> F test of R-squared increase       : F(1, 493) = 1.5895, p = 0.208
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - One or more variables are scaled by SD or standardized. OLS standard
#>   errors and confidence intervals may be biased for their coefficients.
#>   Please use `std_selected_boot()`.
#> 

# Standardize both mod and iv
lm_stdwx <- std_selected(lm_raw, to_scale = ~ mod + iv,
                               to_center = ~ mod + iv)
summary(lm_stdwx)
#> 
#> Call to std_selected():
#> std_selected(lm_out = lm_raw, to_scale = ~mod + iv, to_center = ~mod + 
#>     iv)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: mod iv
#> - Variable(s) scaled: mod iv
#> 
#>      centered_by scaled_by                            Note
#> dv       0.00000  1.000000                                
#> iv      15.01576  2.039154 Standardized (mean = 0, SD = 1)
#> mod    100.39502  5.040823 Standardized (mean = 0, SD = 1)
#> v1       0.00000  1.000000                                
#> cat1          NA        NA Nonnumeric                     
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat_mod)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -2146.0  -431.9   -25.0   411.2  2309.3 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  6765.07     115.86 58.3916  < 0.001 ***
#> iv            806.88      29.94 26.9480  < 0.001 ***
#> mod           284.39      29.95  9.4962  < 0.001 ***
#> v1            -12.76      10.17 -1.2542  0.21037    
#> cat1gp2      -158.67      71.83 -2.2089  0.02764 *  
#> cat1gp3       -43.17      75.28 -0.5734  0.56664    
#> iv:mod         35.11      27.85  1.2608  0.20799    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 665 on 493 degrees of freedom
#> 
#> R-squared                : 0.6352
#> Adjusted R-squared       : 0.6307
#> ANOVA test of R-squared  : F(6, 493) = 143.047, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : iv:mod
#> R-squared increase adding this term: 0.0012
#> F test of R-squared increase       : F(1, 493) = 1.5895, p = 0.208
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - One or more variables are scaled by SD or standardized. OLS standard
#>   errors and confidence intervals may be biased for their coefficients.
#>   Please use `std_selected_boot()`.
#> 

# Standardize all variables except for categorical variables.
# Interaction terms are formed after standardization.
lm_std <- std_selected(lm_raw, to_scale = ~ .,
                               to_center = ~ .)
summary(lm_std)
#> 
#> Call to std_selected():
#> std_selected(lm_out = lm_raw, to_scale = ~., to_center = ~.)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: dv iv mod v1 cat1
#> - Variable(s) scaled: dv iv mod v1 cat1
#> 
#>      centered_by   scaled_by                            Note
#> dv    6565.02965 1094.244465 Standardized (mean = 0, SD = 1)
#> iv      15.01576    2.039154 Standardized (mean = 0, SD = 1)
#> mod    100.39502    5.040823 Standardized (mean = 0, SD = 1)
#> v1      10.13884    2.938932 Standardized (mean = 0, SD = 1)
#> cat1          NA          NA Nonnumeric                     
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat_mod)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.96117 -0.39474 -0.02285  0.37579  2.11040 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   0.0646     0.0483  1.3385  0.18136    
#> iv            0.7374     0.0274 26.9480  < 0.001 ***
#> mod           0.2599     0.0274  9.4962  < 0.001 ***
#> v1           -0.0343     0.0273 -1.2542  0.21037    
#> cat1gp2      -0.1450     0.0656 -2.2089  0.02764 *  
#> cat1gp3      -0.0394     0.0688 -0.5734  0.56664    
#> iv:mod        0.0321     0.0255  1.2608  0.20799    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.6077 on 493 degrees of freedom
#> 
#> R-squared                : 0.6352
#> Adjusted R-squared       : 0.6307
#> ANOVA test of R-squared  : F(6, 493) = 143.047, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : iv:mod
#> R-squared increase adding this term: 0.0012
#> F test of R-squared increase       : F(1, 493) = 1.5895, p = 0.208
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - One or more variables are scaled by SD or standardized. OLS standard
#>   errors and confidence intervals may be biased for their coefficients.
#>   Please use `std_selected_boot()`.
#> 

# Use to_standardize as a shortcut
lm_stdwx2 <- std_selected(lm_raw, to_standardize = ~ mod + iv)
# The results are the same
coef(lm_stdwx)
#> (Intercept)          iv         mod          v1     cat1gp2     cat1gp3 
#>  6765.06947   806.87814   284.39275   -12.75999  -158.67255   -43.16606 
#>      iv:mod 
#>    35.11145 
coef(lm_stdwx2)
#> (Intercept)          iv         mod          v1     cat1gp2     cat1gp3 
#>  6765.06947   806.87814   284.39275   -12.75999  -158.67255   -43.16606 
#>      iv:mod 
#>    35.11145 
all.equal(coef(lm_stdwx), coef(lm_stdwx2))
#> [1] TRUE



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
# Standardize all variables as in std_selected above, and compute the
# nonparametric bootstrapping percentile confidence intervals.
set.seed(87053)
lm_std_boot <- std_selected_boot(lm_raw,
                                 to_scale = ~ .,
                                 to_center = ~ .,
                                 conf = .95,
                                 nboot = 100)
# In real analysis, nboot should be at least 2000.
summary(lm_std_boot)
#> 
#> Call to std_selected_boot():
#> std_selected_boot(lm_out = lm_raw, to_scale = ~., to_center = ~., 
#>     conf = 0.95, nboot = 100)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: dv iv mod v1 cat1
#> - Variable(s) scaled: dv iv mod v1 cat1
#> 
#>      centered_by   scaled_by                            Note
#> dv    6565.02965 1094.244465 Standardized (mean = 0, SD = 1)
#> iv      15.01576    2.039154 Standardized (mean = 0, SD = 1)
#> mod    100.39502    5.040823 Standardized (mean = 0, SD = 1)
#> v1      10.13884    2.938932 Standardized (mean = 0, SD = 1)
#> cat1          NA          NA Nonnumeric                     
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> - Nonparametric bootstrapping 95% confidence intervals computed.
#> - The number of bootstrap samples is 100.
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat_mod)
#> 
#> Residuals:
#>      Min       1Q   Median       3Q      Max 
#> -1.96117 -0.39474 -0.02285  0.37579  2.11040 
#> 
#> Coefficients:
#>             Estimate CI Lower CI Upper Std. Error t value Pr(>|t|)    
#> (Intercept)   0.0646  -0.0301   0.1683     0.0483  1.3385  0.18136    
#> iv            0.7374   0.7041   0.7948     0.0274 26.9480  < 0.001 ***
#> mod           0.2599   0.2129   0.3130     0.0274  9.4962  < 0.001 ***
#> v1           -0.0343  -0.0835   0.0392     0.0273 -1.2542  0.21037    
#> cat1gp2      -0.1450  -0.3127  -0.0046     0.0656 -2.2089  0.02764 *  
#> cat1gp3      -0.0394  -0.1845   0.1222     0.0688 -0.5734  0.56664    
#> iv:mod        0.0321  -0.0103   0.0817     0.0255  1.2608  0.20799    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.6077 on 493 degrees of freedom
#> 
#> R-squared                : 0.6352
#> Adjusted R-squared       : 0.6307
#> ANOVA test of R-squared  : F(6, 493) = 143.047, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : iv:mod
#> R-squared increase adding this term: 0.0012
#> F test of R-squared increase       : F(1, 493) = 1.5895, p = 0.208
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - [CI Lower, CI Upper] are bootstrap percentile confidence intervals.
#> - Std. Error are not bootstrap SEs.
#> 

# Use to_standardize as a shortcut
set.seed(87053)
lm_std_boot2 <- std_selected_boot(lm_raw,
                                  to_standardize = ~ .,
                                  conf = .95,
                                  nboot = 100)
# The results are the same
confint(lm_std_boot)
#>                   2.5 %      97.5 %
#> (Intercept) -0.03011698  0.16832106
#> iv           0.70408792  0.79477219
#> mod          0.21293423  0.31304112
#> v1          -0.08345938  0.03923478
#> cat1gp2     -0.31267483 -0.00464722
#> cat1gp3     -0.18447930  0.12223465
#> iv:mod      -0.01033317  0.08169839
confint(lm_std_boot2)
#>                   2.5 %      97.5 %
#> (Intercept) -0.03011698  0.16832106
#> iv           0.70408792  0.79477219
#> mod          0.21293423  0.31304112
#> v1          -0.08345938  0.03923478
#> cat1gp2     -0.31267483 -0.00464722
#> cat1gp3     -0.18447930  0.12223465
#> iv:mod      -0.01033317  0.08169839
all.equal(confint(lm_std_boot), confint(lm_std_boot2))
#> [1] TRUE

```
