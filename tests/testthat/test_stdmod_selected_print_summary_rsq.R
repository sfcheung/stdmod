skip("WIP")
skip("Test in interactive sessions")
library(testthat)
library(stdmod)

set.seed(64252)
dat <- test_x_1_w_1_v_1_cat1_n_500

lm1_out <- lm(dv ~ iv * mod + v1 + cat1, dat)
lm2_out <- lm(dv ~ iv * cat1 + mod + v1, dat)
lm3_out <- lm(dv ~ mod * cat1 + iv + v1, dat)
lm1_std <- std_selected(lm1_out, to_standardize = ~ .)
lm2_std <- std_selected(lm2_out, to_standardize = ~ .)
lm3_std <- std_selected(lm3_out, to_standardize = ~ .)
lm1_std_boot <- std_selected_boot(lm1_out, to_standardize = ~ .)
lm2_std_boot <- std_selected_boot(lm2_out, to_standardize = ~ .)
lm3_std_boot <- std_selected_boot(lm3_out, to_standardize = ~ .)

summary(lm1_std)
summary(lm2_std)
summary(lm3_std)
summary(lm1_std_boot)
summary(lm2_std_boot)
summary(lm3_std_boot)

test_highest2 <- function(lm_out) {
    lm_highest <- lmhelprs::highest_order(lm_out)
    lm_out2 <- stats::update(lm_out,
                           paste0("~ .-", lm_highest),
                           evaluate = TRUE)
    # lm_out2 <- eval(call2,
    #                 envir = parent.frame())
    lmhelprs::hierarchical_lm(lm_out, lm_out2)
  }

lm1_std
update(lm1_std,  ~ . -iv:mod)
update(lm1_std,  ~ . -iv:mod, evaluate = FALSE)
lm1_out
update(lm1_out,  ~ . -iv:mod)
update(lm1_out,  ~ . -iv:mod, evaluate = FALSE)
test_highest2(lm1_out)
test_highest2(lm1_std)
test_highest2(lm2_out)
test_highest2(lm2_std)
test_highest2(lm3_out)
test_highest2(lm3_std)


library(lmhelprs)
test_highest(lm1_out)
test_highest(lm1_std)
test_highest(lm2_out)
test_highest(lm2_std)
test_highest(lm3_out)
test_highest(lm3_std)


