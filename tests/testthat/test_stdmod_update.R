library(testthat)
library(stdmod)

dat <- test_x_1_w_1_v_1_cat1_n_500

lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)


stdmod_x <- std_selected(lm_raw, to_scale = ~ iv,  to_center = ~ iv)

stdmod_x1 <- update(stdmod_x, ~ . - cat1)

test_that("Drop a term", {
    expect_false(
        "cat1" %in% attr(terms(stdmod_x1), "term.labels")
      )
  })

dat2 <- dat[-c(1:10), ]
lm_raw2 <- lm(dv ~ iv*mod + v1 + cat1, dat2)
stdmod_x2 <- std_selected(lm_raw2, to_scale = ~ iv,  to_center = ~ iv)
stdmod_x2b <- update(stdmod_x, data = dat2)
stdmod_x2b
stdmod_x2

test_that("Change dataset", {
    expect_equivalent(
        coef(stdmod_x2), coef(stdmod_x2b)
      )
  })

stdmod_x_bt <- std_selected_boot(lm_raw, to_scale = ~ iv,  to_center = ~ iv, nboot = 100)
set.seed(1234)
stdmod_x_bt2 <- update(stdmod_x_bt, data = dat2)
dat2 <- dat[-c(1:10), ]
lm_raw2 <- lm(dv ~ iv*mod + v1 + cat1, dat2)
set.seed(1234)
stdmod_x_bt2b <- std_selected_boot(lm_raw2, to_scale = ~ iv,  to_center = ~ iv, nboot = 100)

test_that("Change dataset", {
    expect_equivalent(
        stdmod_x_bt2$boot_ci, stdmod_x_bt2b$boot_ci
      )
  })
