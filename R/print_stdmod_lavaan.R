#' @title Print a 'stdmod_lavaan' Class Object
#'
#' @description Print the output of [stdmod_lavaan()].
#'
#' @return
#'  `x` is returned invisibly.
#'
#' @param x The output of [stdmod_lavaan()].
#' @param conf If nonparametric bootstrapping has been conducted by
#'              [stdmod_lavaan()],
#'             this is the level of confidence in proportion (.95 denotes 95%),
#'             of the confidence interval. Default is .95.
#' @param nd The number of digits to be printed.
#' @param ...  Optional arguments. Ignored.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Load a test data of 500 cases
#'
#' dat <- test_mod1
#' library(lavaan)
#'
#' mod <-
#' "
#' med ~ iv + mod + iv:mod + cov1
#' dv ~ med + cov2
#' "
#' fit <- sem(mod, dat)
#' coef(fit)
#'
#' # Compute the standardized moderation effect
#' out_noboot <- stdmod_lavaan(fit = fit,
#'                             x = "iv",
#'                             y = "med",
#'                             w = "mod",
#'                             x_w = "iv:mod")
#' out_noboot
#'
#' # Compute the standardized moderation effect and
#' # its percentile confidence interval based on nonparametric bootstrapping
#' # Fit the model with bootstrap confidence intervals
#' # At least 2000 bootstrap samples should be used
#' # in real research. 50 is used here only for
#' # illustration.
#' fit <- sem(mod, dat, se = "boot", bootstrap = 50,
#'            iseed = 89574)
#' out_boot <- stdmod_lavaan(fit = fit,
#'                           x = "iv",
#'                           y = "med",
#'                           w = "mod",
#'                           x_w = "iv:mod",
#'                           boot_ci = TRUE)
#'
#' out_boot
#'
#'
#' @export

print.stdmod_lavaan <- function(x,
                                conf = .95,
                                nd = 3,
                                ...) {
    if (!requireNamespace("lavaan", quietly = TRUE)) {
        stop(paste("lavaan needs to be installed to run this function."))
      }
    if (isTRUE(x$call$boot_ci)) {
        has_bootci <- TRUE
      } else {
        has_bootci <- FALSE
      }
    cat("\nCall:\n")
    print(x$call)
    roles <- c("Focal Variable", "Moderator", "Outcome Variable", "Product Term")
    c_x <- x$call$x
    c_w <- x$call$w
    c_y <- x$call$y
    c_x_w <- x$call$x_w
    vars <- c(c_x, c_w, c_y, c_x_w)
    tmpdf <- data.frame(Variable = vars)
    rownames(tmpdf) <- roles
    cat("\n")
    print(tmpdf)
    fit <- x$fit
    ptable <- lavaan::parameterEstimates(fit, level = conf)
    mod_org <- ptable[(ptable$lhs == c_y) &
                      (ptable$op == "~") &
                      (ptable$rhs == c_x_w), ]
    mod_table <- rbind(mod_org, mod_org)
    rownames(mod_table) <- c("Original", "Standardized")
    mod_table[2, ] <- NA
    mod_table[2, 1:3] <- mod_table[1, 1:3]
    mod_table[2, "est"] <- x$stdmod
    if (has_bootci) {
        stdmod_ci_new <- boot::boot.ci(x$boot_out,
                                        type = "perc",
                                        conf = conf)$percent[4:5]
        mod_table[2, c("ci.lower", "ci.upper")] <- stdmod_ci_new
      }
    cat("\n")
    print(mod_table, nd = nd)
    if (has_bootci) {
        nboot_real <- sum(!is.na(x$boot_out$t))
        tmp <- paste0("Confidence interval of standardized ",
                      "moderation effect:\n",
                      "- Level of confidence: ",
                         round(conf, 2) * 100, "%\n",
                      "- Bootstrapping Method: Nonparametric\n",
                      "- Type: Percentile\n",
                      "- Number of bootstrap samples requests: ",
                      x$call$R, "\n",
                      "- Number of bootstrap samples with valid results: ",
                      nboot_real)
        cat("\n", tmp, "\n", sep = "")
      }
    if (has_bootci) {
        if (isTRUE(x$old_version) || is.null(x$old_version)) {
            cat(strwrap(paste0("NOTE: Bootstrapping conducted by the old version method (0.2.7.4 or before). ",
                              "The method in the newer version is recommended.")), sep = "\n")
          } else {
            cat("\n")
            cat(strwrap(paste0("NOTE: Bootstrapping conducted by the method in 0.2.7.5 or later. ",
                              "To use the method in the older versions for reproducing previous results, ",
                              "set 'use_old_version' to 'TRUE'.")), sep = "\n")

          }
      }
    invisible(x)
  }