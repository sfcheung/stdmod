library(testthat)
library(stdmod)

context("Check to_standardize")

dat <- test_x_1_w_1_v_1_cat1_n_500

lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
# lm_zx  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1]))
# lm_zw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, mod = scale(mod)[, 1]))
# lm_zy  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, dv = scale(dv)[, 1]))
# lm_zxzw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1],
#                                                   mod = scale(mod)[, 1]))
# lm_zxzy  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1],
#                                                   dv = scale(dv)[, 1]))
# lm_zyzw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, dv = scale(dv)[, 1],
#                                                   mod = scale(mod)[, 1]))
# lm_zall  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1],
#                                                   mod = scale(mod)[, 1],
#                                                   dv = scale(dv)[, 1]))

# lm_cxsw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv, scale = FALSE, center = TRUE)[, 1],
#                                                   mod = scale(mod, scale = sd(dat$mod), center = FALSE)[, 1]))


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
    expect_equivalent(
        coef(stdmod_x), coef(stdmod_x2)
      )
  })

test_that("Standardize y", {
    expect_equivalent(
        coef(stdmod_y), coef(stdmod_y2)
      )
  })

test_that("Standardize w", {
    expect_equivalent(
        coef(stdmod_w), coef(stdmod_w2)
      )
  })

test_that("Standardize xy", {
    expect_equivalent(
        coef(stdmod_xy), coef(stdmod_xy2)
      )
  })

test_that("Standardize xw", {
    expect_equivalent(
        coef(stdmod_xw), coef(stdmod_xw2)
      )
  })

test_that("Standardize yw", {
    expect_equivalent(
        coef(stdmod_yw), coef(stdmod_yw2)
      )
  })

test_that("Standardize x, y, and w", {
    expect_equivalent(
        coef(stdmod_xyw), coef(stdmod_xyw2)
      )
  })

test_that("Center x and scale w", {
    expect_equivalent(
        coef(stdmod_cxsw2), coef(stdmod_cxsw3)
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
    expect_equivalent(
        coef(stdmod_nb_x), coef(stdmod_nb_x2)
      )
    expect_equivalent(
        coef(stdmod_nb_y), coef(stdmod_nb_y2)
      )
    expect_equivalent(
        coef(stdmod_nb_w), coef(stdmod_nb_w2)
      )
    expect_equivalent(
        coef(stdmod_nb_xw), coef(stdmod_nb_xw2)
      )
    expect_equivalent(
        coef(stdmod_nb_yw), coef(stdmod_nb_yw2)
      )
    expect_equivalent(
        coef(stdmod_nb_xy), coef(stdmod_nb_xy2)
      )
    expect_equivalent(
        coef(stdmod_nb_xyw), coef(stdmod_nb_xyw2)
      )
    expect_equivalent(
        coef(stdmod_nb_cxsw2), coef(stdmod_nb_cxsw3)
      )
  })
