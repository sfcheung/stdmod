skip("WIP")

# testthat::test_file("./tests/testthat/test_std_selected_lavaan.R")

library(testthat)
library(lavaan)
library(manymome)

dat <- HolzingerSwineford1939

mod <-
"
f1 =~ x1 + x2 + x3
f2 =~ x4 + d5*x5 + d6*x6
f3 =~ x7 + d8*x8 + d9*x9
f2 ~ a*f1
f3 ~ b*f2
ab := a*b
d1 := d5 - d9
d2 := d8 - d6
dd := d1 * d2
"

fit <- cfa(mod,
           dat)

est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE)
std <- standardizedSolution(fit)
std_nox <- standardizedSolution(fit, type = "std.nox")
std_lv <- standardizedSolution(fit, type = "std.lv")

test_that("User parameters", {
  out <- std_selected_lavaan(fit, standardized = TRUE, std_se = "delta")
  expect_equal(out$std.all,
               out$std.p,
               ignore_attr = TRUE)
  std[, c("lhs", "op", "rhs", "se")]
  out[, c("lhs", "op", "rhs", "std.p.se")]
  expect_equal(out$std.p.se,
               std$se,
               ignore_attr = TRUE,
               tolerance = 1e-4)
})
