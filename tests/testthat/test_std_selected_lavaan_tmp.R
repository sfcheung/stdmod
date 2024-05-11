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

out <- std_selected_lavaan(fit)
out
std <- standardizedSolution(fit)
i <- which(!sapply(std$est.std / out$std.p, function(x) isTRUE(all.equal(x, 1))))

# Difference
out[i, c("lhs", "op", "rhs", "std.p")]
std[i, c("lhs", "op", "rhs", "est.std")]

# Compare with results with variables standardized first

# Use ML cov to scale
n <- nrow(test_mod1)
test_mod1_cov <- cov(test_mod1) * (n - 1) / n
test_mod1z <- as.data.frame(scale(test_mod1,
                                  scale = sqrt(diag(test_mod1_cov))))
fitz <- sem(mod, data = test_mod1z)
estz <- parameterEstimates(fitz)
estz[1:6, c("lhs", "op", "rhs", "est")]
out[1:6, c("lhs", "op", "rhs", "std.p")]
lavInspect(fit, "implied")
cov(test_mod1)
