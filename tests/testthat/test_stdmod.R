library(testthat)
library(stdmod)

# context("Check standardized moderation effect")

dat <- test_x_1_w_1_v_2_n_500

transform0 <- function(data, vars) {
    for (x in vars) {
        data[x] <- scale(data[[x]])[, 1]
      }
    data
  }

lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
lm_zx  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv")))
lm_zw  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("mod")))
lm_zy  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("dv")))
lm_zxzw  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv", "mod")))
lm_zxzy  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv", "dv")))
lm_zyzw  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("dv", "mod")))
lm_zall  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv", "dv", "mod")))

stdmod_x <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = TRUE,  y_rescale = FALSE, w_rescale = FALSE)
stdmod_y <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = FALSE, y_rescale = TRUE,  w_rescale = FALSE)
stdmod_w <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = FALSE, y_rescale = FALSE, w_rescale = TRUE )
stdmod_xw <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = TRUE , y_rescale = FALSE, w_rescale = TRUE )
stdmod_yw <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = FALSE, y_rescale = TRUE , w_rescale = TRUE )
stdmod_xy <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                   x_rescale = TRUE , y_rescale = TRUE,  w_rescale = FALSE)
stdmod_xyw <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                     x_rescale = TRUE, y_rescale = TRUE, w_rescale = TRUE)
stdmod_xyw2 <- stdmod(lm_raw, x = iv, y = dv, w = mod)

test_that("Standardize x", {
    expect_equal(
        stdmod_x, coef(lm_zx)["iv:mod"],
        ignore_attr = TRUE
      )
  })

test_that("Standardize y", {
    expect_equal(
        stdmod_y, coef(lm_zy)["iv:mod"],
        ignore_attr = TRUE
      )
  })

test_that("Standardize w", {
    expect_equal(
        stdmod_w, coef(lm_zw)["iv:mod"],
        ignore_attr = TRUE
      )
  })

test_that("Standardize xy", {
    expect_equal(
        stdmod_xy, coef(lm_zxzy)["iv:mod"],
        ignore_attr = TRUE
      )
  })

test_that("Standardize xw", {
    expect_equal(
        stdmod_xw, coef(lm_zxzw)["iv:mod"],
        ignore_attr = TRUE
      )
  })

test_that("Standardize yw", {
    expect_equal(
        stdmod_yw, coef(lm_zyzw)["iv:mod"],
        ignore_attr = TRUE
      )
  })

test_that("Standardize x, y, and w", {
    expect_equal(
        stdmod_xyw, coef(lm_zall)["iv:mod"],
        ignore_attr = TRUE
      )
  })

test_that("Standardize x, y, and w with default", {
    expect_equal(
        stdmod_xyw2, coef(lm_zall)["iv:mod"],
        ignore_attr = TRUE
      )
  })

