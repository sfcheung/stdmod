#' #Create several datasets for testing purpose.
#' 
#' 

#' One IV, one moderation, two covariates, n 100
#' For testing

set.seed(686453)
n <- 500
x <- rnorm(n, 15, 2)
w <- rnorm(n, 100, 5)
vs <- MASS::mvrnorm(n, c(v1 = 0, v2 = 0), diag(2))
vs <- vs %*% diag(c(3, 5)) + matrix(c(10, 50), n, 2, byrow = TRUE)
colnames(vs) <- paste0("v", seq_len(ncol(vs)))
y <- 10 + 3*x + 5*w + 4*x*w + vs %*% matrix(c(1, 1), 2, 1) + rnorm(n, 0, 700)
dat <- data.frame(dv = y, iv = x, mod = w, vs)
out <- lm(dv ~ iv*mod + v1 + v2, dat)
summary(lm.beta::lm.beta(out))
test_x_1_w_1_v_2_n_100 <- dat
usethis::use_data(test_x_1_w_1_v_2_n_100, overwrite = TRUE)
