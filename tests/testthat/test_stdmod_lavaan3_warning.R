skip("Disabled")

# Not reliable to hard core failures in convergence because
# lavaan may change the optimization process.

library(testthat)
library(stdmod)

# context("Check stdmod_lavaan when warning occurs")

dat <- test_mod3_miss

# Results based on stdmod_lavaan

library(lavaan)
mod <-
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
# No need to test no boot case.
# Users should know fit the warning message before calling stdmod_lavaan
# suppressWarnings(fit <- sem(mod, dat[-c(485:500), ], missing = "fiml.x"))
# test_that("No boot, warning on run", {
#     expect_warning(
#         out_noboot <- stdmod_lavaan(fit = fit, x = "iv",
#                                     y = "med",
#                                     w = "mod",
#                                     x_w = "iv:mod")
#       )
#   })

fit <- sem(mod, dat, missing = "fiml.x")
test_that("Boot", {
    expect_warning({
        set.seed(6589107)
        system.time(out_boot <- stdmod_lavaan(fit = fit,
                                              x = "iv",
                                              y = "med",
                                              w = "mod",
                                              x_w = "iv:mod",
                                              boot_ci = TRUE,
                                              R = 100,
                                              use_old_version = TRUE))
      })
    expect_equal(
        sum(is.na(out_boot$boot_out$t)),
        9,
        ignore_attr = TRUE
      )
    print_out <- capture.output(print(out_boot))
    expect_equal(
        as.numeric(strsplit(print_out[grepl("valid results",
                                            print_out)],
                                            ":")[[1]][2]),
        91,
        ignore_attr = TRUE
      )
  })
