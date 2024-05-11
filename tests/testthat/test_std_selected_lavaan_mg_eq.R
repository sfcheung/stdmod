skip_on_cran()
# A long test
# With between-group constraints

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
           fixed.x = FALSE,
           group = "school",
           group.equal = "loadings")

(est <- parameterEstimates(fit,
                          standardized = TRUE,
                          ci = FALSE))
(std <- standardizedSolution(fit))
(std_nox <- standardizedSolution(fit, type = "std.nox"))
(std_lv <- standardizedSolution(fit, type = "std.lv"))

test_that("Standardized coefficient: Equal loadings", {

  i <- 2
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(as.vector(std_se),
               std[i, "se"])

  i <- 29
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

  i <- 38
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

  i <- 31
  fit_std_2 <- gen_std_i(fit = fit, i = i)
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std[i, "est.std"])
  expect_equal(std_se,
               std[i, "se"])

  i <- 33
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
            fixed.x = FALSE,
            group = "school",
            group.equal = "loadings")
(std2 <- standardizedSolution(fit2))
(std2_nox <- standardizedSolution(fit2, type = "std.nox"))

test_that("Alternate values", {
  fit_std_2 <- gen_std_i(fit = fit, i = 29)
  expect_equal(as.vector(fit_std_2(coef(fit2))),
               std2[29, "est.std"])
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
               rep(1, length(std_se)))
  fit_std_all_est <- sapply(fit_std_all, function(xx) xx(coef(fit)))
  expect_equal(fit_std_all_est / standardizedSolution(fit)$est.std[i_std],
               rep(1, length(fit_std_all_est)))
})

# (which(std_nox$est.std != std$est.std))
#  4  5  6 11 12 13 14 15 16 20 21 22 27 28 29 34 35 36 37 38 39 43 44 45

test_that("Standardized coefficients: No X", {

  i <- 4
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_nox[i, "est.std"])
  expect_equal(std_se,
               std_nox[i, "se"])

  i <- 28
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_nox[i, "est.std"])
  expect_equal(std_se,
               std_nox[i, "se"])

  i <- 38
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_nox[i, "est.std"])
  expect_equal(std_se,
               std_nox[i, "se"])

  i <- 25
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = c("f1", "x2"))
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_nox[i, "est.std"])
  expect_equal(std_se,
               std_nox[i, "se"])

})

# (which(std_lv$est.std != std$est.std))
#  1  2  3  4  5  6  7  8  9 11 12 13 14 15 16 17 18 19 20 21 22 24 25 26 27 28 29 30 31 32 34 35 36 37 38 39 40 41 42 43 44 45

test_that("Standardized coefficients: lv", {

  i <- 25
  fit_std_2 <- gen_std_i(fit = fit, i = i, to_standardize = "f1")
  std_a <- grad(fit_std_2, x = coef(fit))
  std_se <- sqrt(colSums(std_a * (vcov(fit) %*% std_a)))
  expect_equal(as.vector(fit_std_2(coef(fit))),
               std_lv[i, "est.std"])
  expect_equal(std_se,
               std_lv[i, "se"])

})

# Automatically skip ageXX
test_that("to_standardize", {
  std_out <- std_selected_lavaan(fit)
  std_nox_out1 <- std_selected_lavaan(fit,
                                      to_standardize = c("f1", "x1", "x2", "x3"))
  std_nox_out2 <- std_selected_lavaan(fit,
                                      not_to_standardize = c("age13", "age14", "age15"))
  expect_equal(std_nox_out1$std.p, std_nox_out2$std.p)
  expect_equal(std_nox_out1$std.p, std_out$std.p)
})
