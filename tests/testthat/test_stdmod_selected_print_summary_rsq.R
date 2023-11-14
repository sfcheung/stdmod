library(testthat)

set.seed(64252)
dat <- test_x_1_w_1_v_1_cat1_n_500

lm0_out <- lm(dv ~ iv + mod + v1 + cat1, dat)
lm1_out <- lm(dv ~ iv * mod + v1 + cat1, dat)
lm2_out <- lm(dv ~ iv * cat1 + mod + v1, dat)
lm3_out <- lm(dv ~ mod * cat1 + iv + v1, dat)
lm4_out <- lm(dv ~ mod * cat1 + iv*v1, dat)
lm5_out <- lm(dv ~ v1, dat)
lm0_std <- std_selected(lm0_out, to_standardize = ~ .)
lm1_std <- std_selected(lm1_out, to_standardize = ~ .)
lm2_std <- std_selected(lm2_out, to_standardize = ~ .)
lm3_std <- std_selected(lm3_out, to_standardize = ~ .)
lm4_std <- std_selected(lm4_out, to_standardize = ~ .)
lm5_std <- std_selected(lm5_out, to_standardize = ~ .)
lm0_std_boot <- std_selected_boot(lm0_out, to_standardize = ~ .)
lm1_std_boot <- std_selected_boot(lm1_out, to_standardize = ~ .)
lm2_std_boot <- std_selected_boot(lm2_out, to_standardize = ~ .)
lm3_std_boot <- std_selected_boot(lm3_out, to_standardize = ~ .)
lm4_std_boot <- std_selected_boot(lm4_out, to_standardize = ~ .)
lm5_std_boot <- std_selected_boot(lm5_out, to_standardize = ~ .)

test_that("summary.std_select with Rsq", {
    expect_false(any(grepl("adding this term",
                     capture.output(print(summary(lm0_std))))))
    expect_true(any(grepl("adding this term",
                    capture.output(print(summary(lm1_std))))))
    expect_true(any(grepl("adding this term",
                    capture.output(print(summary(lm2_std))))))
    expect_true(any(grepl("adding this term",
                    capture.output(print(summary(lm3_std))))))
    expect_false(any(grepl("adding this term",
                     capture.output(print(summary(lm4_std))))))
    expect_false(any(grepl("adding this term",
                     capture.output(print(summary(lm5_std))))))
    expect_false(any(grepl("adding this term",
                     capture.output(print(summary(lm0_std_boot))))))
    expect_true(any(grepl("adding this term",
                    capture.output(print(summary(lm1_std_boot))))))
    expect_true(any(grepl("adding this term",
                    capture.output(print(summary(lm2_std_boot))))))
    expect_true(any(grepl("adding this term",
                    capture.output(print(summary(lm3_std_boot))))))
    expect_false(any(grepl("adding this term",
                     capture.output(print(summary(lm4_std_boot))))))
    expect_false(any(grepl("adding this term",
                     capture.output(print(summary(lm5_std_boot))))))
  })
