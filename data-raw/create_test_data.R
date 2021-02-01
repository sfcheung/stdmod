#' #Create several datasets for testing purpose.
#' 
#' 

#' One IV, one moderator, two covariates, n 500
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
test_x_1_w_1_v_2_n_500 <- dat
usethis::use_data(test_x_1_w_1_v_2_n_500, overwrite = TRUE)

#' One IV, one moderator, two covariates, n 500
#' One of the covariates is string variable (categorical) with three categories.
#' For testing

set.seed(64534432)
n <- 500
x <- rnorm(n, 15, 2)
w <- rnorm(n, 100, 5)
vs <- MASS::mvrnorm(n, c(v1 = 0, v2 = 0), diag(2))
vs <- vs %*% diag(c(3, 5)) + matrix(c(10, 50), n, 2, byrow = TRUE)
v1 <- vs[, 1]
cat1 <- sample(c("gp1", "gp2", "gp3"), n, replace = TRUE)
colnames(vs) <- paste0("v", seq_len(ncol(vs)))
y <- 10 + 3*x + 5*w + 4*x*w + 2*v1 + 
      sapply(cat1, switch, gp1 = 1, gp2 = 4, gp3 = 2) + rnorm(n, 0, 700)
dat <- data.frame(dv = y, iv = x, mod = w, v1, cat1, stringsAsFactors = FALSE)
out <- lm(dv ~ iv*mod + v1 + cat1, dat)
summary(lm.beta::lm.beta(out))
test_x_1_w_1_v_1_cat1_n_500 <- dat
usethis::use_data(test_x_1_w_1_v_1_cat1_n_500, overwrite = TRUE)

#' A real dataset extracted from a study on personality traits and sleep duration

library(dplyr)
sleep_emo_con <- readRDS("./data-raw/sleep_duration_emotional_stability_conscientiousness.RDS")
sleep_emo_con <- sleep_emo_con %>% rename(emotional_stability = emotionality_stability)
sleep_emo_con <- sleep_emo_con %>%
      mutate(gender = recode(gender, `0` = "male", `1` = "female"),
             case_id = seq_len(nrow(sleep_emo_con))) %>%
      select(case_id, everything())
head(sleep_emo_con)
usethis::use_data(sleep_emo_con, overwrite = TRUE)
