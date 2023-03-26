library(testthat)
library(stdmod)

context("Check setting confint in summary if bootstrapping is done")

dat <- test_x_1_w_1_v_1_cat1_n_500

lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)

set.seed(649831074)
stdmod_wy <- std_selected_boot(lm_raw, to_scale = ~ mod + dv, to_center = ~ mod + dv,
                               nboot = 100,
                               full_output = TRUE)
confint_def <- confint(stdmod_wy)
confint_boot <- confint(stdmod_wy, type = "boot")
confint_lm <- confint(stdmod_wy, type = "lm")
confint_boot_parm <- confint(stdmod_wy, parm = "iv:mod", type = "boot")
confint_boot_conf90 <- confint(stdmod_wy, parm = "iv:mod", level = .90, type = "boot")

stdmod_wy_lm <- std_selected(lm_raw, to_scale = ~ mod + dv, to_center = ~ mod + dv)
confint_lm_no_boot <- confint(stdmod_wy_lm)

confint_boot_check <- summary(stdmod_wy)$coefficients[, c("CI Lower", "CI Upper")]
confint_boot_conf90_check <- boot::boot.ci(stdmod_wy$boot_out, conf = .90,
                                           type = "perc", index = 7)$percent[4:5]

test_that("confint_def == confint by boot since 0.2.0.0", {
    expect_equivalent(
        confint_def, confint_boot
      )
  })

test_that("confint_boot == confint from summary", {
    expect_equivalent(
        confint_boot, confint_boot_check
      )
  })

test_that("confint_boot != confint from lm", {
    expect_false(
        identical(confint_boot, confint_lm_no_boot)
      )
  })

test_that("confint_boot with user confidence level", {
    expect_true(
        all(confint_boot_conf90 == confint_boot_conf90_check)
      )
  })

test_that("confint with type 'lm'", {
    expect_true(
        all(confint_lm == confint_lm_no_boot)
      )
  })
