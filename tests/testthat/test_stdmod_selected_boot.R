skip_on_cran()

library(testthat)
library(stdmod)

set.seed(8970808)

context("Check standardizing selected variables with std_selected with bootstrapping")

dat <- test_x_1_w_1_v_1_cat1_n_500

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
                                                  

stdmod_wy <- std_selected_boot(lm_raw, to_scale = ~ mod + dv, to_center = ~ mod + dv, nboot = 2000)
stdmod_xwy <- std_selected_boot(lm_raw, to_scale = ~ mod + iv + dv, to_center = ~ iv + mod + dv, nboot = 2000)

stdmod2_wy <- stdmod_boot(lm_raw, x = iv, y = dv, w =mod,
                           x_rescale = FALSE, y_rescale = TRUE, w_rescale = TRUE, nboot = 2000)
stdmod2_xwy <- stdmod_boot(lm_raw, x = iv, y = dv, w =mod,
                           x_rescale = TRUE, y_rescale = TRUE, w_rescale = TRUE, nboot = 2000)

stdmod3_wy <- std_selected(lm_raw, to_scale = ~ mod + dv, to_center = ~ dv + mod)
stdmod3_xwy <- std_selected(lm_raw, to_scale = ~ dv + iv + mod,  to_center = ~ mod + iv + dv)
                           
                           
test_that("Standardize w and y: Compare coefficients (selected_boot vs. noboot)", {
    expect_equivalent(
        coef(stdmod_wy), coef(stdmod3_wy)
      )
  })

test_that("Standardize w and y: Compare bootstrapping ci", {
    expect_equivalent(
        stdmod_wy$boot_ci["iv:mod",], stdmod2_wy$ci, tolerance = .01
      )
  })  
  

      
test_that("Standardize x, w and y: Compare coefficients (selected_boot vs. noboot)", {
    expect_equivalent(
        coef(stdmod_xwy), coef(stdmod3_xwy)
      )
  })

test_that("Standardize x, w and y: Compare bootstrapping ci", {
    expect_equivalent(
        stdmod_xwy$boot_ci["iv:mod",], stdmod2_xwy$ci, tolerance = .01
      )
  })  
  
