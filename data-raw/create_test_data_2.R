#' #Create several datasets for testing purpose.
#' 
#' 

#' One IV, one moderator, two covariates, n 500
#' One of the covariates is string variable (categorical) with three categories.
#' IV and moderator has a nonlinear relation
#' For testing

library(ggplot2)
set.seed(6453442)
n <- 500
x <- runif(n, -3, 3)
tmp <- as.character(as.numeric(cut(x, 3)))
w <- sapply(tmp, function(x) switch(x, "1" = .2, "2" = .25, "3" = .0)) + 
      sapply(tmp,
        function(x) switch(x, "1" = 0, "2" = .15, "3" = .50)) *
        x + rnorm(n, 0, 1)
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
ptb <- plotmod_tumble(out, x = "iv", w = "mod")
ptb
p0 <- plotmod(out, x = "iv", w = "mod")
p0
ptbylim <- layer_scales(ptb)$y$range$range
ptbxlim <- layer_scales(ptb)$x$range$range
p0ylim <- layer_scales(p0)$y$range$range
p0xlim <- layer_scales(p0)$x$range$range
pyl <- min(ptbylim, p0ylim)
pyh <- max(ptbylim, p0ylim)
pxl <- min(ptbxlim, p0xlim)
pxh <- max(ptbxlim, p0xlim)
p0 + geom_vline(xintercept = ptbxlim) +
     geom_hline(yintercept = ptbylim) +
     xlim(pxl, pxh) + ylim(pyl, pyh)
ptb + geom_vline(xintercept = p0xlim) +
     geom_hline(yintercept = p0ylim) +
     xlim(pxl, pxh) + ylim(pyl, pyh)
test_x_1_w_1_v_1_cat1_xw_cov_n_500 <- dat
usethis::use_data(test_x_1_w_1_v_1_cat1_xw_cov_n_500, overwrite = TRUE)


#' One IV, one moderator, two covariates, n 500
#' One of the covariates is string variable (categorical) with three categories.
#' Moderator is a string variable with three categories
#' IV associated with the moderator
#' For testing

library(ggplot2)
set.seed(6453442)
n <- 500
w <- sample(c("City Gamma", "City Beta", "City Alpha"), n, replace = TRUE)
x <- sapply(w, function(xi) switch(xi,
                                  "City Gamma" = runif(1, -1, 0),
                                  "City Beta" = runif(1, -.5, .5),
                                  "City Alpha" = runif(1, 0, 1)),
                                  USE.NAMES = FALSE)
vs <- MASS::mvrnorm(n, c(v1 = 0, v2 = 0), diag(2))
vs <- vs %*% diag(c(3, 5)) + matrix(c(10, 50), n, 2, byrow = TRUE)
v1 <- vs[, 1]
cat1 <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
colnames(vs) <- paste0("v", seq_len(ncol(vs)))
bx <- sapply(w, function(xi) switch(xi,
                                  "City Gamma" = 8,
                                  "City Beta" = 4,
                                  "City Alpha" = 0),
                                  USE.NAMES = FALSE)
bw <- sapply(w, function(xi) switch(xi,
                                  "City Gamma" = 10,
                                  "City Beta" = 10,
                                  "City Alpha" = 12),
                                  USE.NAMES = FALSE)
y <- 10 + bx*x + bw + 2*v1 + 
      sapply(cat1, switch, gp1 = 1, gp2 = 4, gp3 = 2) + rnorm(n, 0, 10)
dat <- data.frame(dv = y, iv = x, mod = w, v1, cat1, stringsAsFactors = FALSE)
out <- lm(dv ~ iv*mod + v1 + cat1, dat)
summary(out)
summary(lm.beta::lm.beta(out))
ptb <- plotmod_tumble(out, x = "iv", w = "mod")
ptb
p0 <- plotmod(out, x = "iv", w = "mod")
p0
ptbylim <- layer_scales(ptb)$y$range$range
ptbxlim <- layer_scales(ptb)$x$range$range
p0ylim <- layer_scales(p0)$y$range$range
p0xlim <- layer_scales(p0)$x$range$range
pyl <- min(ptbylim, p0ylim)
pyh <- max(ptbylim, p0ylim)
pxl <- min(ptbxlim, p0xlim)
pxh <- max(ptbxlim, p0xlim)
p0 + geom_vline(xintercept = ptbxlim) +
     geom_hline(yintercept = ptbylim) +
     xlim(pxl, pxh) + ylim(pyl, pyh)
ptb + geom_vline(xintercept = p0xlim) +
     geom_hline(yintercept = p0ylim) + 
     xlim(pxl, pxh) + ylim(pyl, pyh)
test_x_1_w_1_v_1_cat1_xw_cov_wcat3_n_500 <- dat
usethis::use_data(test_x_1_w_1_v_1_cat1_xw_cov_wcat3_n_500, overwrite = TRUE)

