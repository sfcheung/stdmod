skip("WIP")

#Load a test data of 500 cases
data(test_mod1)
library(lavaan)
mod <-
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
fit <- sem(mod, data = test_mod1)

# Standardize all variables

out <- std_selected_lavaan(fit, standardized = TRUE, std_se = "delta")
out
std <- standardizedSolution(fit)

out3 <- std_selected_lavaan(fit, standardized = TRUE, output = "text")
out3

# Difference
i <- which(!sapply(std$est.std / out$std.p, function(x) isTRUE(all.equal(x, 1))))
print(merge(out[i, c("lhs", "op", "rhs", "std.p")],
            std[i, c("lhs", "op", "rhs", "est.std")],
            sort = FALSE),
      digits = 3)

# Standardize some variables

out2 <- std_selected_lavaan(fit, standardized = TRUE, not_to_standardize = c("cov2", "dv"))
out2
i2 <- which(!sapply(std$est.std / out2$std.p, function(x) isTRUE(all.equal(x, 1))))

# Difference
out2[i2, c("lhs", "op", "rhs", "std.p")]
std[i2, c("lhs", "op", "rhs", "est.std")]

# Compare with results with variables standardized first
# Differences are expected because model implied SDs are used

n <- nrow(test_mod1)
test_mod1_cov <- cov(test_mod1)
test_mod1z <- as.data.frame(scale(test_mod1,
                                  scale = sqrt(diag(test_mod1_cov))))
fitz <- sem(mod, data = test_mod1z)
estz <- parameterEstimates(fitz)
estz[1:6, c("lhs", "op", "rhs", "est")]
out[1:6, c("lhs", "op", "rhs", "std.p")]
round(estz$est / out$std.p, 3)

observed_cov <- cov(test_mod1)
fit_implied_cov <- lavInspect(fit, "implied")$cov
(fit_implied_cov[colnames(observed_cov), colnames(observed_cov)] * n / (n - 1)) / observed_cov

fit_samptsta <- lavInspect(fit, "sampstat")$cov
(fit_samptsta[colnames(observed_cov), colnames(observed_cov)] * n / (n - 1) / observed_cov)

# Bootstrapping

fit_boot <- sem(mod, data = test_mod1,
                se = "boot",
                bootstrap = 100,
                iseed = 1234)
out_boot <- std_selected_lavaan(fit_boot, standardized = TRUE,
                                std_se = "boot",
                                std_ci = TRUE)
