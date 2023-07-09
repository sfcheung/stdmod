skip("Test in interactive sessions")
library(testthat)
library(stdmod)

set.seed(64252)
n <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
y <- .2 * x1 + .3 * x2 + 0 * x3 + rnorm(n, 0, 1)

dat <- data.frame(x1, x2, x3 = x3 * 100, y)

lm_out <- lm(y ~ x1 + x2 + x3, dat)
lm_std <- std_selected(lm_out, to_standardize = ~ y + x1 + x2)
lm_std_boot <- std_selected_boot(lm_out, to_standardize = ~ y + x1 + x2)

summary(lm_std)
print(summary(lm_std), est_digits = 4, pvalue_less_than = 1e-3)
print(summary(lm_std), est_digits = 2, pvalue_less_than = 1e-3)
print(summary(lm_std), est_digits = 2, pvalue_less_than = 1e-4, t_digits = 2)

summary(lm_std_boot)
print(summary(lm_std_boot), est_digits = 4, pvalue_less_than = 1e-3)
print(summary(lm_std_boot), est_digits = 2, pvalue_less_than = 1e-2, t_digits = 1)
print(summary(lm_std_boot), est_digits = 2, pvalue_less_than = 1e-2, t_digits = 5)

print(summary(lm_std_boot), default_style = TRUE)
