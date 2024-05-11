skip_on_cran()
# A long test

library(testthat)
library(lavaan)
library(manymome)
library(numDeriv)

dat <- HolzingerSwineford1939
dat$age_gp <- dat$ageyr
dat <- dat[(dat$age_gp >= 12) & (dat$age_gp <= 15), ]
table(dat$age_gp)
tmp <- factor2var(dat$age_gp)
dat[, c("age13", "age14", "age15")] <- tmp
head(dat)
dat$age13b <- dat$age13

mod <-
"
f1 =~ x1 + x2 + x3
f1 ~ age13 + age14 + age15
"

fit <- sem(mod,
           dat,
           fixed.x = FALSE)

(est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE))
(std <- standardizedSolution(fit))
(std_nox <- standardizedSolution(fit, type = "std.nox"))
(std_lv <- standardizedSolution(fit, type = "std.lv"))

test_that("Standardized coefficient", {

  i <- 2
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

  i <- 5
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

  i <- 8
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

  i <- 11
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

  i <- 12
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

})

fit2 <- sem(mod,
            dat[c(1:100, 150:250), ],
            fixed.x = FALSE)
(std2 <- standardizedSolution(fit2))
(std2_nox <- standardizedSolution(fit2, type = "std.nox"))

test_that("Alternate values", {
  fit_std_2 <- gen_std_i(fit = fit, i = 2)
  expect_equal(as.vector(fit_std_2(coef(fit2))),
               std2[2, "est.std"])
})

# Delta method

test_that("All est", {
  i_std <- which((std$se > 0) &
                 (std$op != "~1"))
  fit_std_all <- lapply(i_std,
                        gen_std_i,
                        fit = fit)
  std_a <- lapply(fit_std_all, grad, x = coef(fit))
  std_se <- sapply(std_a, function(xx) {
                  sqrt(colSums(xx * (vcov(fit) %*% xx)))
                  #sqrt(xx %*% vcov(fit) %*% xx)
                })
  expect_equal(std_se / std$se[i_std],
               rep(1, length(std_se)),
               tolerance = 1e-5)
  fit_std_all_est <- sapply(fit_std_all, function(xx) xx(coef(fit)))
  expect_equal(fit_std_all_est / standardizedSolution(fit)$est.std[i_std],
               rep(1, length(fit_std_all_est)),
               tolerance = 1e-5)
})

# (which(std_nox$est.std != std$est.std))
#  4  5  6 11 12 13 14 15 16

test_that("Standardized coefficients: No X", {

  i <- 4
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_nox[i, "est.std"],
               tolerance = 1e-5)
  expect_equal(std_se,
               std_nox[i, "se"],
               tolerance = 1e-5)

  i <- 11
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_nox[i, "est.std"],
               tolerance = 1e-5)
  expect_equal(std_se,
               std_nox[i, "se"],
               tolerance = 1e-5)

  i <- 15
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_nox[i, "est.std"],
               tolerance = 1e-5)
  expect_equal(std_se,
               std_nox[i, "se"],
               tolerance = 1e-5)

})

# (which(std_lv$est.std != std$est.std))
#   1  2  3  4  5  6  7  8  9 11 12 13 14 15 16

test_that("Standardized coefficients: lv", {

  i <- 2
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_lv[i, "est.std"])
  expect_equal(std_se,
               std_lv[i, "se"])

})

