skip_on_cran()
# Parallel processing
# Long test

# testthat::test_file("./tests/testthat/test_std_selected_lavaan_mod_boot.R")

library(manymome)

#Load a test data of 500 cases
data(test_mod1)
library(lavaan)
dat <- test_mod1
dat$group <- rep(c("gp1", "gp2", "gp3"), nrow(dat))[seq_len(nrow(dat))]
dat$iv_mod <- dat$iv * dat$mod
head(dat)

mod <-
"
med ~ iv + mod + iv_mod + cov1
dv ~ med + cov2
"
fit <- sem(mod,
           data = dat)
fit_gp <- sem(mod,
              data = dat,
              group = "group",
              group.label = c("gp1", "gp3", "gp2"))

test_that("Get do_boot results", {
  fit_boot <- sem(mod,
                  data = dat,
                  se = "boot",
                  bootstrap = 50,
                  iseed = 4567)
  fit_boot_gp <- sem(mod,
                    data = dat,
                    group = "group",
                    group.label = c("gp1", "gp3", "gp2"),
                    se = "boot",
                    bootstrap = 50,
                    iseed = 4567)

  boot_out <- do_boot(fit,
                      R = 50,
                      seed = 4567,
                      parallel = FALSE,
                      progress = FALSE)
  boot_out_gp <- do_boot(fit_gp,
                        R = 50,
                        seed = 4567,
                        parallel = FALSE,
                        progress = FALSE)

  out <- std_selected_lavaan(fit,
                            standardized = TRUE,
                            not_to_standardize = c("dv", "med", "cov2"),
                            progress = FALSE,
                            std_se = "bootstrap",
                            boot_out = boot_out)
  out_gp <- std_selected_lavaan(fit_gp,
                                standardized = TRUE,
                                not_to_standardize = c("dv", "med", "cov2"),
                                progress = FALSE,
                                std_se = "bootstrap",
                                boot_out = boot_out_gp)

  out_boot <- std_selected_lavaan(fit_boot,
                                  standardized = TRUE,
                                  not_to_standardize = c("dv", "med", "cov2"),
                                  progress = FALSE,
                                  std_se = "bootstrap")
  out_boot_gp <- std_selected_lavaan(fit_boot_gp,
                                    standardized = TRUE,
                                    not_to_standardize = c("dv", "med", "cov2"),
                                    progress = FALSE,
                                    std_se = "bootstrap")

  out_boot_lav <- std_selected_lavaan(fit,
                                      standardized = TRUE,
                                      not_to_standardize = c("dv", "med", "cov2"),
                                      progress = FALSE,
                                      std_se = "bootstrap",
                                      bootstrap = 50,
                                      iseed = 4567)
  out_boot_lav_gp <- std_selected_lavaan(fit_gp,
                                         standardized = TRUE,
                                         not_to_standardize = c("dv", "med", "cov2"),
                                         progress = FALSE,
                                         std_se = "bootstrap",
                                         bootstrap = 50,
                                         iseed = 4567)

  expect_equal(out[5:8, "std.p.se"],
              out_boot[5:8, "se"],
              tolerance = 1e-3)

  expect_equal(out_gp[c(5:8, 35:38), "std.p.se"],
              out_boot_gp[c(5:8, 35:38), "se"],
              tolerance = 1e-3)

  expect_equal(out_boot_lav[5:8, "std.p.se"],
              out_boot[5:8, "se"],
              tolerance = 1e-3)

  expect_equal(out_boot_lav_gp[c(5:8, 35:38), "std.p.se"],
              out_boot_gp[c(5:8, 35:38), "se"],
              tolerance = 1e-3)

})

test_that("boot_out error", {
  expect_error(std_selected_lavaan(fit,
                                   standardized = TRUE,
                                   not_to_standardize = c("dv", "med", "cov2"),
                                   std_se = "bootstrap",
                                   boot_out = 123),
               regexp = "boot_out")
})

