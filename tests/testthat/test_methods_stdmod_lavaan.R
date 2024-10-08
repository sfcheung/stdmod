library(testthat)
library(stdmod)

# context("Test methods for stdmod_lavaan")

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
                            x_w = "iv:mod",
                            use_old_version = TRUE)
out_noboot$ci
out_noboot$stdmod
coef(out_noboot)

test_that("confint.stdmod_lavaan: No boot ci", {
  expect_warning(
      tmp <- confint(out_noboot)
    )
  expect_true(
      all(is.na(tmp))
    )
  })

test_that("coef.stdmod_lavaan: No boot ci", {
  expect_equal(
      coef(out_noboot),
      out_noboot$stdmod,
      ignore_attr = TRUE
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
                                      use_old_version = TRUE))
out_boot$ci
confint(out_boot)
out_boot$stdmod
coef(out_boot)

test_that("confint.stdmod_lavaan: Has boot ci", {
  expect_equal(
      as.vector(confint(out_boot)),
      out_boot$ci,
      ignore_attr = TRUE
    )
  })

test_that("coef.stdmod_lavaan: Has boot ci", {
  expect_equal(
      coef(out_boot),
      out_boot$stdmod,
      ignore_attr = TRUE
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
                                      conf = .90,
                                      use_old_version = TRUE))
out_boot$ci
confint(out_boot, level = .90)

test_that("confint.stdmod_lavaan: Has boot ci, level .90", {
  expect_equal(
      as.vector(confint(out_boot, level = .90)),
      out_boot$ci,
      ignore_attr = TRUE
    )
  })

