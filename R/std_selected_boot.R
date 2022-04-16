# Compute the standardized moderation effect and betas for other predictors given the \code{lm} output, with bootstrapping CIs
#'
# Compute the standardized moderation effect and betas for other predictors given the \code{lm} output, with bootstrapping CIs
#'
#'
#' @details
#' This function is a wrapper of [std_selected()]. It calls [std_selected()] once 
#' for each bootstrap sample, and then compute the nonparametric bootstrapping
#' percentile confidence intervals.
#'
#' This function intentionally does not have an argument for setting the seed for
#' random number. Users are recommended to set the seed, e.g., using [set.seed()]
#' before calling it, to ensure reproducibility.
#'
#  The updated \code{lm} output, with the class \code{std_selected} added. It will be
#  treated as a usual \code{lm} object by most functions. It has these additional elements:
#
#  - \code{scaled}: The terms scaled.
#
#  - \code{centered}: The terms centered.
#
#  - \code{scaled_by}: The scaling factors. The value is 1 for terms not scaled.
#
#  - \code{centered}: The values used for centering. The value is 0 for terms not centered.
#'
#'
#' @param lm_out The output from \code{lm}.
#' @param ...  Arguments to be passed to \code{std_selected}.
#' @param conf The level of confidence for the confidence interval. Default is .95.
#' @param nboot The number of bootstrap samples. Default is 100.
#' @param boot_args A named list of arguments to be passed to [boot::boot()]. Default
#'                 is `NULL`.
#' @param save_boot_est If `TRUE`, the default, the bootstrap estimates will be saved
#'                      in the element
#'                      `boot_est` of the output.
#' @param full_output Whether the full output from [boot::boot()] is return. Default is 
#'                   `FALSE`.
#'
#' @examples
#' \donttest{
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#' head(dat)
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#' # Standardize all variables as in std_selected above, and compute the
#' # nonparametric boostrapping percentile confidence intervals.
#' lm_std_boot <- std_selected_boot(lm_raw, 
#'                                     to_scale = ~ .,
#'                                     to_center = ~ .,
#'                                     conf = .95,
#'                                     nboot = 500)
#' # In read analysis, nboot should be at least 2000.
#' summary(lm_std_boot)
#' }
#' @export
#' @describeIn std_selected A wrapper of [std_selected()] that forms
#'                           nonparametrc bootstrapping confidence intervals.
#' @order 2

std_selected_boot <- function(lm_out,
                            ...,
                            conf = .95,
                            nboot = 100,
                            boot_args = NULL,
                            save_boot_est = TRUE,
                            full_output = FALSE) {
    if (missing(lm_out)) {
        stop("The arguments lm_out cannot be empty.")
      }

    # Get the data frame.
    # Form the bootstrapping function.
    # Do the bootstrapping.
    # Collect the results.
    # Return the results.


    # Get the data frame

    dat <- lm_out$model
    k <- ncol(dat)
    n <- nrow(dat)

    # Create the boot function
    
    bootfct <- create_boot_selected(lm_out, ...)

    # Do bootstrapping
    
    boot_out <- do.call(boot::boot, 
                  c(list(data = dat, statistic = bootfct, R = nboot), 
                  boot_args)) 
    
    # Collect output
    
    p <- length(boot_out$t0)
    
    cis <- t(sapply(seq_len(p), function(x) {
                boot::boot.ci(boot_out, conf = conf,
                              type = "perc", index = x)$percent[4:5]
              }))
    rownames(cis) <- names(boot_out$t0)
    colnames(cis) <- c("CI Lower", "CI Upper")
    
    # Do std_selected
    
    std_selected_out <- std_selected(lm_out, ...)
    
    # Append bootstrapping output
    
    std_selected_out$boot_ci <- cis
    std_selected_out$nboot <- nboot
    tmp <- boot_out$t
    colnames(tmp) <- names(boot_out$t0)
    std_selected_out$boot_est <- tmp
    if (full_output) {
        std_selected_out$boot_out <- boot_out
      }
     
    std_selected_out
  }
  
create_boot_selected <- function(lm_out, ...) {
  function(d, ind) {
        force(lm_out)
        lm_out_i <- lm_out
        lm_out_i$model <- d[ind, ]
        out <- std_selected(lm_out_i, ...)
        stats::coef(out)
      }
  }
  