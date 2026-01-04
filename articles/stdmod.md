# A Quick Start Guide on Using std_selected()

## Introduction

This vignette illustrates how to use
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
the main function from the `stdmod` package. More about this package can
be found in
[`vignette("stdmod", package = "stdmod")`](https://sfcheung.github.io/stdmod/articles/stdmod.md)
or at <https://sfcheung.github.io/stdmod/>.

## This Guide Shows to use `std_selected()` to:

- get the correct standardized regression coefficients of a moderated
  regression model, and

- form the valid confidence intervals of the standardized regression
  coefficients using nonparametric bootstrapping that takes into account
  the sampling variation due to standardization.

## Sample Dataset

``` r
library(stdmod)
dat <- sleep_emo_con
head(dat, 3)
#> # A tibble: 3 × 6
#>   case_id sleep_duration  cons  emot   age gender
#>     <int>          <dbl> <dbl> <dbl> <dbl> <chr> 
#> 1       1              6   3.6   3.6    20 female
#> 2       2              4   3.8   2.4    20 female
#> 3       3              7   4.3   2.7    20 female
```

This dataset has 500 cases, with sleep duration (measured in average
hours), conscientiousness, emotional stability, age, and gender (a
`"female"` and `"male"`).

The names of some variables are shortened for readability:

``` r
colnames(dat)[2:4] <- c("sleep", "cons", "emot")
head(dat, 3)
#> # A tibble: 3 × 6
#>   case_id sleep  cons  emot   age gender
#>     <int> <dbl> <dbl> <dbl> <dbl> <chr> 
#> 1       1     6   3.6   3.6    20 female
#> 2       2     4   3.8   2.4    20 female
#> 3       3     7   4.3   2.7    20 female
```

## Model

Suppose this is the moderated regression model:

- Dependent variable (Outcome Variable): sleep duration (`sleep`)

- Independent variable (Predictor / Focal Variable): emotional stability
  (`emot`)

- Moderator: conscientiousness (`cons`)

- Control variables: `age` and `gender`

[`lm()`](https://rdrr.io/r/stats/lm.html) can be used to fit this model:

``` r
lm_out <- lm(sleep ~ age + gender + emot * cons,
             dat = dat)
summary(lm_out)
#> 
#> Call:
#> lm(formula = sleep ~ age + gender + emot * cons, data = dat)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -6.0841 -0.7882  0.0089  0.9440  6.1189 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)   
#> (Intercept)  1.85154    1.35224   1.369  0.17155   
#> age          0.01789    0.02133   0.838  0.40221   
#> gendermale  -0.26127    0.16579  -1.576  0.11570   
#> emot         1.32151    0.45039   2.934  0.00350 **
#> cons         1.20385    0.37062   3.248  0.00124 **
#> emot:cons   -0.33140    0.13273  -2.497  0.01286 * 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.384 on 494 degrees of freedom
#> Multiple R-squared:  0.0548, Adjusted R-squared:  0.04523 
#> F-statistic: 5.728 on 5 and 494 DF,  p-value: 3.768e-05
```

The unstandardized moderation effect is significant, B = -0.3314. For
each one unit increase of conscientiousness score, the effect of
emotional stability decreases by 0.3314.

## Correct Standardization For the Moderated Regression

Suppose we want to find the correct standardized solution for the
moderated regression, that is, all variables except for categorical
variables are standardized. In a moderated regression model, the product
term should be formed *after* standardization.

Instead of doing the standardization ourselves before calling
[`lm()`](https://rdrr.io/r/stats/lm.html), we can pass the
[`lm()`](https://rdrr.io/r/stats/lm.html) output to
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
and use `~ .` for the arguments `to_scale` and `to_center`.

``` r
lm_stdall <- std_selected(lm_out,
                          to_scale = ~ .,
                          to_center = ~ .)
```

Since 0.2.6.3, `to_standardize` can be used as a shortcut:

``` r
lm_stdall <- std_selected(lm_out,
                          to_standardize = ~ .)
```

``` r
summary(lm_stdall)
#> 
#> Call to std_selected():
#> std_selected(lm_out = lm_out, to_standardize = ~.)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: sleep age gender emot cons
#> - Variable(s) scaled: sleep age gender emot cons
#> 
#>        centered_by scaled_by                            Note
#> sleep     6.776333 1.4168291 Standardized (mean = 0, SD = 1)
#> age      22.274000 2.9407857 Standardized (mean = 0, SD = 1)
#> gender          NA        NA Nonnumeric                     
#> emot      2.713200 0.7629613 Standardized (mean = 0, SD = 1)
#> cons      3.343200 0.6068198 Standardized (mean = 0, SD = 1)
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> 
#> Call:
#> lm(formula = sleep ~ age + gender + emot * cons, data = dat_mod)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -4.2941 -0.5563  0.0063  0.6663  4.3187 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)   
#> (Intercept)   0.0549     0.0488  1.1248  0.26124   
#> age           0.0371     0.0443  0.8384  0.40221   
#> gendermale   -0.1844     0.1170 -1.5759  0.11570   
#> emot          0.1150     0.0449  2.5600  0.01076 * 
#> cons          0.1305     0.0452  2.8893  0.00403 **
#> emot:cons    -0.1083     0.0434 -2.4967  0.01286 * 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.9771 on 494 degrees of freedom
#> 
#> R-squared                : 0.0548
#> Adjusted R-squared       : 0.0452
#> ANOVA test of R-squared  : F(5, 494) = 5.7277, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : emot:cons
#> R-squared increase adding this term: 0.0119
#> F test of R-squared increase       : F(1, 494) = 6.2335, p = 0.013
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - One or more variables are scaled by SD or standardized. OLS standard
#>   errors and confidence intervals may be biased for their coefficients.
#>   Please use `std_selected_boot()`.
```

In this example, the coefficient of the product term, which naturally
can be called the **standardized moderation effect**, is significant, B
= -0.1083. For each one *standard deviation* increase of
conscientiousness score, the **standardized effect** of emotional
stability decreases by 0.1083.

### The Arguments

Standardization is equivalent to centering by mean and then scaling by
(dividing by) standard deviation. The argument `to_center` specifies the
variables to be centered by their means, and the argument `to_scale`
specifies the variables to be scaled by their standard deviations. The
formula interface of [`lm()`](https://rdrr.io/r/stats/lm.html) is used
in these two arguments, with the variables on the right hand side being
the variables to be centered and/or scaled.

The “`.`” on the right hand side represents all variables in the model,
including the outcome variable (sleep duration in this example).

[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
will also skip categorical variables automatically skipped because
standardizing them will make their coefficients not easy to interpret.

Since 0.2.6.3, `to_standardize` is added as a shortcut. Listing a
variable on `to_standardize` is equivalent to listing this variable on
both `to_center` and `to_scale`.

### Advantage

Using `std_selected` minimizes impact on the workflow. Do regression as
usual. Get the correct standardized coefficients only when we need to
interpret them.

### Nonparametric Bootstrap Confidence Intervals

There is one problem with standardized coefficients. The confidence
intervals based on ordinary least squares (OLS) fitted to the
standardized variables do not take into account the sampling variation
of the sample means and standard deviations ([Yuan & Chan,
2011](https://doi.org/10.1007/s11336-011-9224-6)). [Cheung, Cheung, Lau,
Hui, and Vong (2022)](https://doi.org/10.1037/hea0001188) suggest using
nonparametric bootstrapping, with standardization conducted in each
bootstrap sample.

This can be done by
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
a wrapper of
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md):

``` r
set.seed(870432)
lm_stdall_boot <- std_selected_boot(lm_out,
                        to_scale = ~ .,
                        to_center = ~ .,
                        nboot = 5000)
```

Since 0.2.6.3, `to_standardize` can be used as a shortcut:

``` r
lm_stdall_boot <- std_selected_boot(lm_out,
                        to_standardize = ~ .
                        nboot = 5000)
```

The minimum additional argument is `nboot`, the number of bootstrap
samples.

``` r
summary(lm_stdall_boot)
#> 
#> Call to std_selected_boot():
#> std_selected_boot(lm_out = lm_out, to_scale = ~., to_center = ~., 
#>     nboot = 5000)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: sleep age gender emot cons
#> - Variable(s) scaled: sleep age gender emot cons
#> 
#>        centered_by scaled_by                            Note
#> sleep     6.776333 1.4168291 Standardized (mean = 0, SD = 1)
#> age      22.274000 2.9407857 Standardized (mean = 0, SD = 1)
#> gender          NA        NA Nonnumeric                     
#> emot      2.713200 0.7629613 Standardized (mean = 0, SD = 1)
#> cons      3.343200 0.6068198 Standardized (mean = 0, SD = 1)
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> - Nonparametric bootstrapping 95% confidence intervals computed.
#> - The number of bootstrap samples is 5000.
#> 
#> Call:
#> lm(formula = sleep ~ age + gender + emot * cons, data = dat_mod)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -4.2941 -0.5563  0.0063  0.6663  4.3187 
#> 
#> Coefficients:
#>             Estimate CI Lower CI Upper Std. Error t value Pr(>|t|)   
#> (Intercept)   0.0549   0.0072   0.1045     0.0488  1.1248  0.26124   
#> age           0.0371  -0.0347   0.1072     0.0443  0.8384  0.40221   
#> gendermale   -0.1844  -0.4392   0.0783     0.1170 -1.5759  0.11570   
#> emot          0.1150   0.0291   0.2012     0.0449  2.5600  0.01076 * 
#> cons          0.1305   0.0288   0.2265     0.0452  2.8893  0.00403 **
#> emot:cons    -0.1083  -0.2043  -0.0090     0.0434 -2.4967  0.01286 * 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.9771 on 494 degrees of freedom
#> 
#> R-squared                : 0.0548
#> Adjusted R-squared       : 0.0452
#> ANOVA test of R-squared  : F(5, 494) = 5.7277, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : emot:cons
#> R-squared increase adding this term: 0.0119
#> F test of R-squared increase       : F(1, 494) = 6.2335, p = 0.013
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - [CI Lower, CI Upper] are bootstrap percentile confidence intervals.
#> - Std. Error are not bootstrap SEs.
```

The output is similar to that of
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md),
with additional information on the bootstrapping process.

The 95% bootstrap percentile confidence interval of the standardized
moderation effect is -0.2043 to -0.0090.

## Standardize Independent Variable (Focal Variable) and Moderator

[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
and
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
can also be used to standardize only selected variables. There are cases
in which we do not want to standardize some continuous variables because
they are measured on interpretable units, such as hours.

Suppose we want to standardize only emotional stability and
conscientiousness, and do not standardize sleep duration. We just list
`emot` and `cons` on `to_center` and `to_scale`:

``` r
lm_std1 <- std_selected(lm_out,
                        to_scale = ~ emot + cons,
                        to_center = ~ emot + cons)
```

Since 0.2.6.3, `to_standardize` can be used a shortuct:

``` r
lm_std1 <- std_selected(lm_out,
                        to_standardize = ~ emot + cons)
```

``` r
summary(lm_std1)
#> 
#> Call to std_selected():
#> std_selected(lm_out = lm_out, to_standardize = ~emot + cons)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: emot cons
#> - Variable(s) scaled: emot cons
#> 
#>        centered_by scaled_by                            Note
#> sleep       0.0000 1.0000000                                
#> age         0.0000 1.0000000                                
#> gender          NA        NA Nonnumeric                     
#> emot        2.7132 0.7629613 Standardized (mean = 0, SD = 1)
#> cons        3.3432 0.6068198 Standardized (mean = 0, SD = 1)
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> 
#> Call:
#> lm(formula = sleep ~ age + gender + emot * cons, data = dat_mod)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -6.0841 -0.7882  0.0089  0.9440  6.1189 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)   6.4557     0.4783 13.4979  < 0.001 ***
#> age           0.0179     0.0213  0.8384  0.40221    
#> gendermale   -0.2613     0.1658 -1.5759  0.11570    
#> emot          0.1630     0.0637  2.5600  0.01076 *  
#> cons          0.1849     0.0640  2.8893  0.00403 ** 
#> emot:cons    -0.1534     0.0615 -2.4967  0.01286 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.384 on 494 degrees of freedom
#> 
#> R-squared                : 0.0548
#> Adjusted R-squared       : 0.0452
#> ANOVA test of R-squared  : F(5, 494) = 5.7277, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : emot:cons
#> R-squared increase adding this term: 0.0119
#> F test of R-squared increase       : F(1, 494) = 6.2335, p = 0.013
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - One or more variables are scaled by SD or standardized. OLS standard
#>   errors and confidence intervals may be biased for their coefficients.
#>   Please use `std_selected_boot()`.
```

The *partially* standardized moderation effect is -0.1534. For each one
*standard deviation* increase of conscientiousness score, the
*partially* standardized effect of emotional stability decreases by
0.1534.

### Nonparametric Bootstrap Confidence Intervals

The function
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
can also be used to form the nonparametric bootstrap confidence interval
when only some of the variables are standardized:

``` r
set.seed(870432)
lm_std1_boot <- std_selected_boot(lm_out,
                        to_scale = ~ emot + cons,
                        to_center = ~ emot + cons,
                        nboot = 5000)
```

Since 0.2.6.3, `to_standardize` can be used as a shortcut:

``` r
lm_std1_boot <- std_selected_boot(lm_out,
                        to_standardize = ~ emot + cons,
                        nboot = 5000)
```

Again, the only additional argument is `nboot`.

``` r
summary(lm_std1_boot)
#> 
#> Call to std_selected_boot():
#> std_selected_boot(lm_out = lm_out, to_scale = ~emot + cons, to_center = ~emot + 
#>     cons, nboot = 5000)
#> 
#> Selected variable(s) are centered by mean and/or scaled by SD
#> - Variable(s) centered: emot cons
#> - Variable(s) scaled: emot cons
#> 
#>        centered_by scaled_by                            Note
#> sleep       0.0000 1.0000000                                
#> age         0.0000 1.0000000                                
#> gender          NA        NA Nonnumeric                     
#> emot        2.7132 0.7629613 Standardized (mean = 0, SD = 1)
#> cons        3.3432 0.6068198 Standardized (mean = 0, SD = 1)
#> 
#> Note:
#> - Categorical variables will not be centered or scaled even if
#>   requested.
#> - Nonparametric bootstrapping 95% confidence intervals computed.
#> - The number of bootstrap samples is 5000.
#> 
#> Call:
#> lm(formula = sleep ~ age + gender + emot * cons, data = dat_mod)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -6.0841 -0.7882  0.0089  0.9440  6.1189 
#> 
#> Coefficients:
#>             Estimate CI Lower CI Upper Std. Error t value Pr(>|t|)    
#> (Intercept)   6.4557   5.6487   7.2735     0.4783 13.4979  < 0.001 ***
#> age           0.0179  -0.0184   0.0544     0.0213  0.8384  0.40221    
#> gendermale   -0.2613  -0.6233   0.1105     0.1658 -1.5759  0.11570    
#> emot          0.1630   0.0405   0.2893     0.0637  2.5600  0.01076 *  
#> cons          0.1849   0.0415   0.3229     0.0640  2.8893  0.00403 ** 
#> emot:cons    -0.1534  -0.2915  -0.0124     0.0615 -2.4967  0.01286 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 1.384 on 494 degrees of freedom
#> 
#> R-squared                : 0.0548
#> Adjusted R-squared       : 0.0452
#> ANOVA test of R-squared  : F(5, 494) = 5.7277, p < 0.001
#> 
#> = Test the highest order term =
#> The highest order term             : emot:cons
#> R-squared increase adding this term: 0.0119
#> F test of R-squared increase       : F(1, 494) = 6.2335, p = 0.013
#> 
#> Note:
#> - Estimates and their statistics are based on the data after
#>   mean-centering, scaling, or standardization.
#> - [CI Lower, CI Upper] are bootstrap percentile confidence intervals.
#> - Std. Error are not bootstrap SEs.
```

The 95% bootstrap percentile confidence interval of the partially
standardized moderation effect is -0.2915 to -0.0124.

## Further Information

A more detailed illustration can be found at
[`vignette("moderation", package = "stdmod")`](https://sfcheung.github.io/stdmod/articles/moderation.md).

[`vignette("std_selected", package = "stdmod")`](https://sfcheung.github.io/stdmod/articles/std_selected.md)
illustrates how
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
can be used to form nonparametric bootstrap percentile confidence
interval for standardized regression coefficients (“betas”) for
regression models without a product term.

Further information on the functions can be found in their help pages
([`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
and
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)).
For example, parallel computation can be used when doing bootstrapping,
if the number of bootstrapping samples request is large.

## Reference(s)

Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
(2022) Improving an old way to measure moderation effect in standardized
units. *Health Psychology*, *41*(7), 502-505.
<https://doi.org/10.1037/hea0001188>.

Yuan, K.-H., & Chan, W. (2011). Biases and standard errors of
standardized regression coefficients. *Psychometrika, 76*(4), 670-690.
<https://doi.org/10.1007/s11336-011-9224-6>
