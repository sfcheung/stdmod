# Print Basic Information of a 'std_selected' Class Object

Provide information of centering and scaling, along with basic model
information printed by the
[`print()`](https://rdrr.io/r/base/print.html) method of
[`lm()`](https://rdrr.io/r/stats/lm.html).

## Usage

``` r
# S3 method for class 'std_selected'
print(x, ...)
```

## Arguments

- x:

  The output of
  [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  or
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md).

- ...:

  Arguments to be passed to
  [`print()`](https://rdrr.io/r/base/print.html) method of
  [`lm()`](https://rdrr.io/r/stats/lm.html).

## Value

`x` is returned invisibly.

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

# Standardize all variables except for categorical variables.
# Interaction terms are formed after standardization.
lm_std <- std_selected(lm_raw, to_scale = ~ .,
                               to_center = ~ .)
lm_std
#> 
#> - Variable(s) requested to center: dv iv mod v1 cat1
#> - Variable(s) requested to scale: dv iv mod v1 cat1
#> Note: categorical variables will not be centered nor scaled even if requested to do so.
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat_mod)
#> 
#> Coefficients:
#> (Intercept)           iv          mod           v1      cat1gp2      cat1gp3  
#>     0.06458      0.73738      0.25990     -0.03427     -0.14501     -0.03945  
#>      iv:mod  
#>     0.03209  
#> 

# With bootstrapping
# nboot = 100 just for illustration. nboot >= 2000 should be used in read
# research.
lm_std_boot <- std_selected_boot(lm_raw, to_scale = ~ .,
                                         to_center = ~ .,
                                         nboot = 100)
lm_std_boot
#> 
#> - Variable(s) requested to center: dv iv mod v1 cat1
#> - Variable(s) requested to scale: dv iv mod v1 cat1
#> Note: categorical variables will not be centered nor scaled even if requested to do so.
#> - Nonparametric bootstrap 95% percentile confidence intervals computed.
#> 
#> Call:
#> lm(formula = dv ~ iv * mod + v1 + cat1, data = dat_mod)
#> 
#> Coefficients:
#> (Intercept)           iv          mod           v1      cat1gp2      cat1gp3  
#>     0.06458      0.73738      0.25990     -0.03427     -0.14501     -0.03945  
#>      iv:mod  
#>     0.03209  
#> 
```
