library(testthat)
library(stdmod)

context("Check sweep2")

set.seed(864901374)
n <- 100
dat <- data.frame(x1 = rnorm(n, 10, 5), 
                  x2 = sample(c("A", "B", "C"), n, replace = TRUE),
                  x3 = runif(n, 2, 5))
dat2 <- data.frame(x1 = dat$x1 - 10,
                   x2 = dat$x2,
                   x3 = dat$x3/2)
                   
dat2_check1 <- sweep2(dat, c(10, 0, 0), "-")
dat2_check1 <- sweep2(dat2_check1, c( 1, 0, 2), "/")

test_that("Subtract a value then rescale by a factor", {
    expect_identical(
        dat2, dat2_check1
      )
  })
