library(testthat)
library(stdmod)

context("Check standardizing selected variables with std_selected for factors")

dat <- test_x_1_w_1_v_1_cat1_n_500
dat$cat1 <- as.factor(dat$cat1)

lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
lm_zx  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1]))
lm_zw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, mod = scale(mod)[, 1]))
lm_zy  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, dv = scale(dv)[, 1]))
lm_zxzw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1],
                                                  mod = scale(mod)[, 1]))
lm_zxzy  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1],
                                                  dv = scale(dv)[, 1]))
lm_zyzw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, dv = scale(dv)[, 1],
                                                  mod = scale(mod)[, 1]))
lm_zall  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv)[, 1],
                                                  mod = scale(mod)[, 1],
                                                  dv = scale(dv)[, 1]))

lm_cxsw  <- lm(dv ~ iv*mod + v1 + cat1, dplyr::mutate(dat, iv = scale(iv, scale = FALSE, center = TRUE)[, 1],
                                                  mod = scale(mod, scale = sd(dat$mod), center = FALSE)[, 1]))


stdmod_x <- std_selected(lm_raw, to_scale = ~ iv,  to_center = ~ iv)
stdmod_y <- std_selected(lm_raw, to_scale = ~ dv,  to_center = ~ dv)
stdmod_w <- std_selected(lm_raw, to_scale = ~ mod, to_center = ~ mod)
stdmod_xw <- std_selected(lm_raw, to_scale = ~ mod + iv, to_center = ~ iv + mod)
stdmod_yw <- std_selected(lm_raw, to_scale = ~ mod + dv, to_center = ~ dv + mod)
stdmod_xy <- std_selected(lm_raw, to_scale = ~ dv + iv,  to_center = ~ iv + dv)
stdmod_xyw <- std_selected(lm_raw, to_scale = ~ dv + iv + mod,  to_center = ~ mod + iv + dv)
stdmod_cxsw <- std_selected(lm_raw, to_scale = ~ mod, to_center = ~ iv)

test_that("Standardize x", {
    expect_equivalent(
        coef(stdmod_x), coef(lm_zx)
      )
  })

test_that("Standardize y", {
    expect_equivalent(
        coef(stdmod_y), coef(lm_zy)
      )
  })

test_that("Standardize w", {
    expect_equivalent(
        coef(stdmod_w), coef(lm_zw)
      )
  })

test_that("Standardize xy", {
    expect_equivalent(
        coef(stdmod_xy), coef(lm_zxzy)
      )
  })

test_that("Standardize xw", {
    expect_equivalent(
        coef(stdmod_xw), coef(lm_zxzw)
      )
  })

test_that("Standardize yw", {
    expect_equivalent(
        coef(stdmod_yw), coef(lm_zyzw)
      )
  })

test_that("Standardize x, y, and w", {
    expect_equivalent(
        coef(stdmod_xyw), coef(lm_zall)
      )
  })

test_that("Center x and scale w", {
    expect_equivalent(
        coef(stdmod_cxsw), coef(lm_cxsw)
      )
  })




