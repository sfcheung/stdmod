skip_on_cran()

library(testthat)
library(stdmod)

# context("Check setting vcov in summary if bootstrapping is done")

dat <- test_x_1_w_1_v_1_cat1_n_500

lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)

set.seed(649831074)
stdmod_wy <- std_selected_boot(lm_raw, to_scale = ~ mod + dv, to_center = ~ mod + dv,
                               nboot = 100,
                               full_output = TRUE)
vcov_method_def <- vcov(stdmod_wy)
vcov_method_boot <- vcov(stdmod_wy, type = "boot")
vcov_method_lm <- vcov(stdmod_wy, type = "lm")
stdmod_wy_lm <- stdmod_wy
class(stdmod_wy_lm) <- "lm"
vcov_lm <- vcov(stdmod_wy_lm)
vcov_check <- cov(stdmod_wy$boot_out$t)
colnames(vcov_check) <- rownames(vcov_check) <- colnames(vcov_lm)

stdmod_wy_no_boot <- std_selected(lm_raw, to_scale = ~ mod + dv, to_center = ~ mod + dv)

test_that("vcov by method with type boot  == vcov from boot estimates", {
    expect_equal(
        vcov_method_boot, vcov_check,
        ignore_attr = TRUE
      )
  })

test_that("vcov by method with type lm == vcov from lm", {
    expect_equal(
        vcov_method_lm, vcov_lm,
        ignore_attr = TRUE
      )
  })

test_that("vcov by method default == vcov from boot since 0.2.0.0", {
    expect_equal(
        vcov_method_def, vcov_check,
        ignore_attr = TRUE
      )
  })

test_that("No boot: vcov by method default == vcov from lm", {
    expect_equal(
        vcov(stdmod_wy_no_boot), vcov_lm,
        ignore_attr = TRUE
      )
  })

test_that("No boot: vcov by method with type boot returns error", {
    expect_error(
        vcov(stdmod_wy_no_boot, type = "boot")
      )
  })
