library(testthat)
library(stdmod)

# context("Check to_standardize")

dat <- test_x_1_w_1_v_1_cat1_n_500

lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)

stdmod_x <- std_selected(lm_raw, to_scale = ~ iv,  to_center = ~ iv)
stdmod_x2 <- std_selected(lm_raw, to_standardize = ~ iv)
stdmod_y <- std_selected(lm_raw, to_scale = ~ dv,  to_center = ~ dv)
stdmod_y2 <- std_selected(lm_raw, to_standardize = ~ dv)
stdmod_w <- std_selected(lm_raw, to_scale = ~ mod, to_center = ~ mod)
stdmod_w2 <- std_selected(lm_raw, to_standardize = ~ mod)
stdmod_xw <- std_selected(lm_raw, to_scale = ~ mod + iv, to_center = ~ iv + mod)
stdmod_xw2 <- std_selected(lm_raw, to_standardize = ~ mod + iv, to_center = ~ iv + mod)
stdmod_yw <- std_selected(lm_raw, to_scale = ~ mod + dv, to_center = ~ dv + mod)
stdmod_yw2 <- std_selected(lm_raw, to_standardize = ~ mod + dv)
stdmod_xy <- std_selected(lm_raw, to_scale = ~ dv + iv,  to_center = ~ iv + dv)
stdmod_xy2 <- std_selected(lm_raw, to_standardize = ~ dv + iv,  to_center = ~ iv, to_scale = ~ dv)
stdmod_xyw <- std_selected(lm_raw, to_scale = ~ dv + iv + mod,  to_center = ~ mod + iv + dv)
stdmod_xyw2 <- std_selected(lm_raw, to_standardize = ~ dv + iv + mod)
stdmod_cxsw2 <- std_selected(lm_raw, to_scale = ~ mod, to_center = ~ iv, to_standardize = ~ v1)
stdmod_cxsw3 <- std_selected(lm_raw, to_scale = ~ mod + v1, to_center = ~ v1 + iv)

test_that("Standardize x", {
    expect_equal(
        coef(stdmod_x), coef(stdmod_x2),
        ignore_attr = TRUE
      )
  })

test_that("Standardize y", {
    expect_equal(
        coef(stdmod_y), coef(stdmod_y2),
        ignore_attr = TRUE
      )
  })

test_that("Standardize w", {
    expect_equal(
        coef(stdmod_w), coef(stdmod_w2),
        ignore_attr = TRUE
      )
  })

test_that("Standardize xy", {
    expect_equal(
        coef(stdmod_xy), coef(stdmod_xy2),
        ignore_attr = TRUE
      )
  })

test_that("Standardize xw", {
    expect_equal(
        coef(stdmod_xw), coef(stdmod_xw2),
        ignore_attr = TRUE
      )
  })

test_that("Standardize yw", {
    expect_equal(
        coef(stdmod_yw), coef(stdmod_yw2),
        ignore_attr = TRUE
      )
  })

test_that("Standardize x, y, and w", {
    expect_equal(
        coef(stdmod_xyw), coef(stdmod_xyw2),
        ignore_attr = TRUE
      )
  })

test_that("Center x and scale w", {
    expect_equal(
        coef(stdmod_cxsw2), coef(stdmod_cxsw3),
        ignore_attr = TRUE
      )
  })

# Test std_selected_boot with do_boot = FALSE

stdmod_nb_x <- std_selected_boot(lm_raw, to_scale = ~ iv,  to_center = ~ iv, do_boot = FALSE)
stdmod_nb_x2 <- std_selected_boot(lm_raw, to_standardize = ~ iv, do_boot = FALSE)
stdmod_nb_y <- std_selected_boot(lm_raw, to_scale = ~ dv,  to_center = ~ dv, do_boot = FALSE)
stdmod_nb_y2 <- std_selected_boot(lm_raw, to_standardize = ~ dv, do_boot = FALSE)
stdmod_nb_w <- std_selected_boot(lm_raw, to_scale = ~ mod, to_center = ~ mod, do_boot = FALSE)
stdmod_nb_w2 <- std_selected_boot(lm_raw, to_standardize = ~ mod, do_boot = FALSE)
stdmod_nb_xw <- std_selected_boot(lm_raw, to_scale = ~ mod + iv, to_center = ~ iv + mod, do_boot = FALSE)
stdmod_nb_xw2 <- std_selected_boot(lm_raw, to_standardize = ~ mod + iv, do_boot = FALSE)
stdmod_nb_yw <- std_selected_boot(lm_raw, to_scale = ~ mod + dv, to_center = ~ dv + mod, do_boot = FALSE)
stdmod_nb_yw2 <- std_selected_boot(lm_raw, to_standardize = ~ mod + dv, do_boot = FALSE)
stdmod_nb_xy <- std_selected_boot(lm_raw, to_scale = ~ dv + iv,  to_center = ~ iv + dv, do_boot = FALSE)
stdmod_nb_xy2 <- std_selected_boot(lm_raw, to_standardize = ~ dv + iv, do_boot = FALSE)
stdmod_nb_xyw <- std_selected_boot(lm_raw, to_scale = ~ dv + iv + mod,  to_center = ~ mod + iv + dv, do_boot = FALSE)
stdmod_nb_xyw2 <- std_selected_boot(lm_raw, to_scale = ~ dv + iv,  to_center = ~ iv + dv, to_standardize = ~ mod, do_boot = FALSE)
stdmod_nb_cxsw2 <- std_selected_boot(lm_raw, to_scale = ~ mod, to_center = ~ iv, to_standardize = ~ v1, do_boot = FALSE)
stdmod_nb_cxsw3 <- std_selected_boot(lm_raw, to_scale = ~ mod + v1, to_center = ~ v1 + iv, do_boot = FALSE)

test_that("std_selected_boot with do_boot = FALSE", {
    expect_equal(
        coef(stdmod_nb_x), coef(stdmod_nb_x2),
        ignore_attr = TRUE
      )
    expect_equal(
        coef(stdmod_nb_y), coef(stdmod_nb_y2),
        ignore_attr = TRUE
      )
    expect_equal(
        coef(stdmod_nb_w), coef(stdmod_nb_w2),
        ignore_attr = TRUE
      )
    expect_equal(
        coef(stdmod_nb_xw), coef(stdmod_nb_xw2),
        ignore_attr = TRUE
      )
    expect_equal(
        coef(stdmod_nb_yw), coef(stdmod_nb_yw2),
        ignore_attr = TRUE
      )
    expect_equal(
        coef(stdmod_nb_xy), coef(stdmod_nb_xy2),
        ignore_attr = TRUE
      )
    expect_equal(
        coef(stdmod_nb_xyw), coef(stdmod_nb_xyw2),
        ignore_attr = TRUE
      )
    expect_equal(
        coef(stdmod_nb_cxsw2), coef(stdmod_nb_cxsw3),
        ignore_attr = TRUE
      )
  })
