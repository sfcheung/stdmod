library(testthat)
library(stdmod)

context("Check confint for stdmod_lavaan")

dat <- test_mod1

# Results based on stdmod_lavaan

library(lavaan)
mod <- 
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
# Use likelihood = "wishart" to replicate results in lm
fit <- sem(mod, dat, likelihood = "wishart")
out_noboot <- stdmod_lavaan(fit = fit, x = "iv",
                            y = "med",
                            w = "mod",
                            x_w = "iv:mod")
out_noboot$ci

test_that("confint.stdmod_lavaan: No boot ci", {
  expect_warning(
      tmp <- confint(out_noboot)
    )
  expect_true(
      all(is.na(tmp))
    )
  })

set.seed(6589107)
system.time(out_boot <- stdmod_lavaan(fit = fit,
                                      x = "iv",
                                      y = "med",
                                      w = "mod",
                                      x_w = "iv:mod",
                                      boot_ci = TRUE,
                                      R = 100))
out_boot$ci
confint(out_boot)

test_that("confint.stdmod_lavaan: Has boot ci", {
  expect_equal(
      as.vector(confint(out_boot)),
      out_boot$ci,
      check.attributes = FALSE
    )
  })

set.seed(6589107)
system.time(out_boot <- stdmod_lavaan(fit = fit,
                                      x = "iv",
                                      y = "med",
                                      w = "mod",
                                      x_w = "iv:mod",
                                      boot_ci = TRUE,
                                      R = 100,
                                      conf = .90))
out_boot$ci
confint(out_boot, level = .90)

test_that("confint.stdmod_lavaan: Has boot ci, level .90", {
  expect_equal(
      as.vector(confint(out_boot, level = .90)),
      out_boot$ci,
      check.attributes = FALSE
    )
  })
