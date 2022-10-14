#' @title Print a 'cond_effect' Class Object
#'
#' @description Print the output of [cond_effect()] or [cond_effect_boot()].
#'
#' @return
#'  `x` is returned invisibility.
#'
#' @param x The output of [cond_effect()] or [cond_effect_boot()].
#' @param nd The number of digits for the variables.
#' @param nd_stat The number of digits for test statistics (e.g., *t*).
#' @param nd_p The number of digits for *p*-values.
#' @param title If `TRUE`, print a title. Default is `TRUE`.
#' @param model If `TRUE`, print the regression model. Default is `TRUE`.
#' @param level_info If `TRUE`, print information for interpreting the
#'                    levels of the moderator, such as the values of
#'                     the levels and distance from the mean. Default
#'                      is `TRUE`.
#' @param standardized If `TRUE` and one or more variables are
#'                     standardized, report it. Default is `TRUE`.`
#' @param boot_info If `TRUE` and bootstrap estimates are in `x`,
#'                  print information about the bootstrapping,
#'                  such as the number of bootstrap samples.
#'                  Default is `TRUE`.
#' @param table_only If `TRUE`, will suppress of other elements except
#'                    for the table of conditional effects. Override
#'                    arguments such as `title`, `model`, and `level_info`.
#' @param t_ci If `TRUE`, will print the confidence intervals based on
#'             t statistics. These confidence intervals should not be
#'             used if some variables are standardized.
#' @param t_ci_level The level of confidence of the confidence intervals
#'                   based on t statistics. Default is .95.
#' @param ...  Additional arguments. Ignored by this function.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Load a sample data set
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#'
#' cond_effect(lm_raw, x = iv, w = mod)
#'
#' lm_std <- std_selected(lm_raw, to_scale = ~ iv + mod,
#'                                to_center = ~ iv + mod)
#'
#' cond_effect(lm_std, x = iv, w = mod)
#'
#' @export

print.cond_effect <- function(x,
                              nd = 3,
                              nd_stat = 3,
                              nd_p = 3,
                              title = TRUE,
                              model = TRUE,
                              level_info = TRUE,
                              standardized = TRUE,
                              boot_info = TRUE,
                              table_only = FALSE,
                              t_ci = FALSE,
                              t_ci_level = .95,
                              ...) {
    xdf <- as.data.frame(x)
    orgcall <- attr(x, "call")
    orgoutput <- attr(x, "output")
    has_bootci <- is.call(attr(x, "cond_effect_boot_call"))
    w_numeric <- is.numeric(x[, 2])
    iv <- attr(x, "x")
    w <- attr(x, "w")
    y <- attr(x, "y")
    cnames <- colnames(x)
    ci_se <- which(cnames == "Std. Error")
    ci_t <- which(cnames == "t value")
    ci_p <- which(cnames == "Pr(>|t|)")
    if (has_bootci) {
        ci_bcilo <- which(cnames == "CI Lower")
        ci_bcihi <- which(cnames == "CI Upper")
      } else {
        ci_bcilo <- ci_bcihi <- NA
      }
    if (title & !table_only) {
        cat("The effects of ",
            iv,
            " on ",
            y,
            ", conditional on ",
            w, ":\n\n", sep = "")
      }
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
    if (has_bootci) {
        xdf[, ci_bcilo] <- formatC(x[, ci_bcilo], nd, format = "f")
        xdf[, ci_bcihi] <- formatC(x[, ci_bcihi], nd, format = "f")
      }
    sig <- stats::symnum(x[, ci_p],
                         corr = FALSE,
                         na = FALSE,
                         cutpoints = c(0, 0.001, 0.01, 0.05, 1),
                         symbols = c("***", "**", "*", " "))
    xdf$Sig <- format(sig)

    if (t_ci) {
        out_all <- attr(x, "out_all")
        ci_all <- lapply(out_all, stats::confint, level = t_ci_level)
        ci_x <- t(sapply(ci_all, function(y) y[iv, ]))
        xdf$`CI.Lo(t)` <- formatC(ci_x[, 1], nd, format = "f")
        xdf$`CI.Hi(t)` <- formatC(ci_x[, 2], nd, format = "f")
      }

    print(xdf, row.names = FALSE)

    y_std <- attr(x, "y_standardized")
    x_std <- attr(x, "x_standardized")
    w_std <- attr(x, "w_standardized")

    if (has_bootci) {
        nboot <- attr(x, "nboot")
        conf <- attr(x, "conf")
        if (boot_info &  !table_only) {
            cat("\n[CI Lower, CI Upper] shows the ",
                round(conf* 100, 2),
                "% nonparametric bootstrap confidence interval(s)",
                sep = "")
            cat("\n (based on", nboot, "bootstrap samples)")
            cat("\n")
          }
      }

    if (t_ci) {
        t_ci_level_txt <- paste0(as.character(t_ci_level * 100), "%")
        cat("\n[CI.Lo(t), CI.Hi(t)] shows the ",
            t_ci_level_txt, " confidence interval(s)",
            " based on t statistics.",
            sep = "")
        if (any(y_std, x_std, w_std)) {
            cat("\nThey should not be used when one or more",
                " variables are standardized.",
                sep = "")
          }
      }

    if (model & !table_only) {
        cat("\n\nThe regression model:\n")
        cat("\n\t", deparse(stats::formula(orgoutput)), "\n", sep = "")
      }

    if (w_numeric) {
        w_df <- xdf[, 1:2]
        w_df$`% Below` <- formatC(100 * attr(x, "w_empirical_percentiles"), 2, format = "f")
        w_df$`From Mean (in SD)` <- formatC(attr(x, "w_empirical_z"), 2, format = "f")
        if (level_info & !table_only) {
            cat(paste0("\nInterpreting the levels of ", w, ":\n\n"))
            print(w_df, row.names = FALSE)
            cat("\n")
            cat("- % Below: The percent of cases equal to or less than a level.\n")
            cat("- From Mean (in SD): Distance of a level from the mean,\n",
                "  in standard deviation (+ve above, -ve below).\n", sep = "")
          }
      }
    if (any(y_std, x_std, w_std)) {
        tmp <- ifelse(c(y_std, x_std, w_std),
                      c(y, iv, w),
                      c(NA, NA, NA))
        v_std <- paste0(stats::na.omit(tmp), collapse = ", ")
        v_txt <- paste0("- The variable(s) ", v_std, " is/are standardized.")
        if (standardized & !table_only) {
            cat("\nNote:\n\n")
            cat(v_txt, "\n", sep = "")
            if (y_std & x_std) {
                cat("- The conditional effects are the standardized effects of ",
                    iv,
                    " on ",
                    y, ".", "\n", sep = "")
              }
          }
      }
    invisible(x)
  }