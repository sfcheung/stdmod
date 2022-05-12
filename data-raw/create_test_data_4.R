#' #Create several datasets for testing purpose.
#'
#'

#' A path model
#' For testing

library(lavaan)
set.seed(164442)
n <- 500
x1 <- rnorm(n)
c1 <- rnorm(n)
w1 <- .5 * x1 + rnorm(n, 0, sqrt(1 - .5^2))
m1 <- 10 + .2 * x1 + .1 * w1 + .2 * x1 * w1 + .1 * c1 + rnorm(n, 0, .5)
summary(lm(m1 ~ x1 * w1 + c1))
c2 <- rnorm(n)
y <- 20 + .3 * m1 + .2 * c2 + rnorm(n, 0, .5)
summary(lm(y ~ m1 + c2))
dat <- data.frame(dv = y,
                  iv = x1,
                  mod = w1,
                  med = m1,
                  cov1 = c1,
                  cov2 = c2)
dat[1:(n * .88), "dv"] <- NA
mod <-
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
fit <- sem(mod, dat, fixed.x = FALSE, missing = "fiml.x")
tmpfct <- function(d, i) {
    update(fit, data = d[i, ])
  }
# temp <- replicate(10, tmpfct(dat, sample.int(n, replace = TRUE)))
test_mod3_miss <- dat
usethis::use_data(test_mod3_miss, overwrite = TRUE)
