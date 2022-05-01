#' @title Print the Summary of a 'cond_effect' Class Object
#'
#' @description Print the output of [cond_effect()].
#'
#' @return
#'  Nothing
#'
#' @param x The output of [cond_effect()].
#' @param nd The number of digits for the variables.
#' @param nd_stat The number of digits for test statistics (e.g., t).
#' @param nd_p The number of digits for p-values.
#' @param ...  Arguments to be passed to [cond_effect()].
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Load a sample data set
#' # It has one predictor (iv), one moderator (mod), on covariate (v1),
#' # one categorical covariate (cat1) with three groups, and one dv (dv).
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#' cond_effect(lm_raw, x = iv, w = mod)
#'
#' lm_std <- std_selected(lm_raw, to_scale = ~ iv + mod, to_center = ~ iv + mod)
#' cond_effect(lm_std, x = iv, w = mod)
#'
#' @export

print.cond_effect <- function(x,
                              nd = 3,
                              nd_stat = 3,
                              nd_p = 3,
                              ...) {
    xdf <- as.data.frame(x)
    orgcall <- attr(x, "call")
    orgoutput <- attr(x, "output")
    w_numeric <- is.numeric(x[, 2])
    iv <- attr(x, "x")
    w <- attr(x, "w")
    y <- attr(x, "y")
    cnames <- colnames(x)
    ci_se <- which(cnames == "Std. Error")
    ci_t <- which(cnames == "t value")
    ci_p <- which(cnames == "Pr(>|t|)")
    cat("The effects of ",
        iv,
        " on ",
        y,
        ", conditional on ",
        w, ":\n\n", sep = "")

    colnames(xdf)[3] <- paste0(iv, " Effect")
    colnames(xdf)[ci_se] <- "S.E."
    colnames(xdf)[ci_t] <- "t"
    colnames(xdf)[ci_p] <- "p"
    if (w_numeric) {
        xdf[, w] <- formatC(x[, w], nd, format = "f")
      }
    xdf[, 3] <- formatC(x[, 3], nd, format = "f")
    xdf[, ci_se] <- formatC(x[, ci_se], nd, format = "f")
    xdf[, ci_t] <- formatC(x[, ci_t], nd_stat, format = "f")
    xdf[, ci_p] <- formatC(x[, ci_p], nd_p, format = "f")
    sig <- stats::symnum(x[, ci_p],
                         corr = FALSE,
                         na = FALSE,
                         cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                         symbols = c("***", "**", "*", " "))
    xdf$Sig <- format(sig)
    print(xdf, row.names = FALSE)

    cat("\nThe regression model:\n")
    cat("\n\t", deparse(stats::formula(orgoutput)), "\n", sep = "")

    if (w_numeric) {
        w_df <- xdf[, 1:2]
        w_df$`% Below` <- formatC(100 * attr(x, "w_empirical_percentiles"), 2, format = "f")
        w_df$`From Mean (in SD)` <- formatC(attr(x, "w_empirical_z"), 2, format = "f")
        cat(paste0("\nInterpreting the levels of ", w, ":\n\n"))
        print(w_df, row.names = FALSE)
        cat("\n")
        cat("- % Below: The percent of cases equal to or less than a level.\n")
        cat("- From Mean (in SD): Distance of a level from the mean,\n",
            "  in standard deviation (+ve above, -ve below).\n", sep = "")
      }
    y_std <- attr(x, "y_standardized")
    x_std <- attr(x, "x_standardized")
    w_std <- attr(x, "w_standardized")
    if (any(y_std, x_std, w_std)) {
        tmp <- ifelse(c(y_std, x_std, w_std),
                      c(y, iv, w),
                      c(NA, NA, NA))
        v_std <- paste0(stats::na.omit(tmp), collapse = ", ")
        v_txt <- paste0("- The variable(s) ", v_std, " is/are standardized.")
        cat("\nNote:\n\n")
        cat(v_txt, "\n", sep = "")
        if (y_std & x_std) {
            cat("- The conditional effects are the standardized effects of ",
                iv,
                " on ",
                y, ".", sep = "")
          }
      }
    cat("\n")
  }