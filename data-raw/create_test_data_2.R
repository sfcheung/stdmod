#' #Create several datasets for testing purpose.
#' 
#' 

#' One IV, one moderator, two covariates, n 500
#' One of the covariates is string variable (categorical) with three categories.
#' IV and moderator has a nonlinear relation
#' For testing

set.seed(64534432)
n <- 500
x <- runif(n, -3, 3)
tmp <- as.character(as.numeric(cut(x, 3)))
w <- sapply(tmp, function(x) switch(x, "1" = 0, "2" = .25, "3" = .0)) + 
      sapply(tmp,
        function(x) switch(x, "1" = 0, "2" = .15, "3" = .50)) *
        x + rnorm(n, 0, .45)
cor(x, w)
plot(x, w)
vs <- MASS::mvrnorm(n, c(v1 = 0, v2 = 0), diag(2))
vs <- vs %*% diag(c(3, 5)) + matrix(c(10, 50), n, 2, byrow = TRUE)
v1 <- vs[, 1]
cat1 <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
colnames(vs) <- paste0("v", seq_len(ncol(vs)))
y <- 10 + 3*x + 35*w + 15*x*w + 2*v1 + 
      sapply(cat1, switch, gp1 = 1, gp2 = 4, gp3 = 2) + rnorm(n, 0, 100)
dat <- data.frame(dv = y, iv = x, mod = w, v1, cat1, stringsAsFactors = FALSE)
out <- lm(dv ~ iv*mod + v1 + cat1, dat)
summary(lm.beta::lm.beta(out))
plotmod(out, x = "iv", w = "mod")
test_x_1_w_1_v_1_cat1__xw_cov_n_500 <- dat
usethis::use_data(test_x_1_w_1_v_1_cat1__xw_cov_n_500, overwrite = TRUE)
