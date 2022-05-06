#' #Create several datasets for testing purpose.
#' 
#' 

#' A path model
#' For testing

library(lavaan)
set.seed(6453442)
n <- 300
x1 <- rnorm(n)
c1 <- rnorm(n)
w1 <- .5 * x1 + rnorm(n, 0, sqrt(1 - .5^2))
m1 <- 10 + .2 * x1 + .1 * w1 + .2 * x1 * w1 + .1 * c1 + rnorm(n, 0, .5)
summary(lm(m1 ~ x1 * w1 + c1))
c2 <- rnorm(n)
y <- 20 + .3 * m1 + .2 * c2 + rnorm(n, 0, .4)
summary(lm(y ~ m1 + c2))
dat <- data.frame(dv = y,
                  iv = x1,
                  mod = w1,
                  med = m1,
                  cov1 = c1,
                  cov2 = c2)
mod <-
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
fit <- sem(mod, dat, fixed.x = FALSE)
parameterEstimates(fit)
test_mod1 <- dat
usethis::use_data(test_mod1, overwrite = TRUE)

#' #Create several datasets for testing purpose.
#'
#'
#' A path model
#' For testing

library(lavaan)
set.seed(534253)
n <- 300
x1 <- rnorm(n)
c1 <- rnorm(n)
w1 <- .5 * x1 + rnorm(n, 0, sqrt(1 - .5^2))
m1 <- 10 + .2 * x1 + .1 * w1 + .1 * c1 + rnorm(n, 0, .5)
summary(lm(m1 ~ x1 + c1))
c2 <- rnorm(n)
y <- 20 + .3 * m1 + .01 * w1 + 1 * m1 * w1 + .2 * c2 + rnorm(n, 0, 8)
summary(lm(y ~ m1 * w1 + c2))
dat <- data.frame(dv = y,
                  iv = x1,
                  mod = w1,
                  med = m1,
                  cov1 = c1,
                  cov2 = c2)
mod <-
"
med ~ iv + cov1
dv ~ med + mod + med:mod + cov2
"
fit <- sem(mod, dat, fixed.x = FALSE)
parameterEstimates(fit)
test_mod2 <- dat
usethis::use_data(test_mod2, overwrite = TRUE)

