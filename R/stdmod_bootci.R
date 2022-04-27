#' @details
#'
#' [stdmod_boot()] is a wrapper of [stdmod()]. It computes the nonparametric
#' bootstrap confidence interval of the standardized moderation effect, as
#' suggested by Cheung, Cheung, Lau, Hui, and Vong (2022), given
#' the output of [lm()]
#'
#' Percentile interval from [boot::boot.ci()] is returned by this function.
#' If other types of
#' confidence intervals are desired, set `full_output = TRUE` and use
#' [boot::boot.ci()] on the element `boot_out` in the output of this
#' function.
#'
#' @param lm_out The output from [lm()].
#' @param ...    Parameters to be passed to [stdmod()].
#' @param nboot  The number of bootstrap samples. Default is 100.
#' @param conf The level of confidence for the confidence interval.
#'             Default is .95.
#' @param boot_args A named list of arguments to be passed to [boot::boot()].
#'                  Default is `NULL`.
#' @param full_output Whether the full output from [boot::boot()] is returned.
#'                   Default is `FALSE`.
#'
#'
#' @examples
#'
#' dat <- test_x_1_w_1_v_2_n_500
#' # Do regression as usual:
#' lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
#'
#' # Compute the standardized moderation effect.
#' # Form its confidence interval by nonparametric bootstrapping.
#' set.seed(85740917)
#' stdmod_xyw_boot <- stdmod_boot(lm_raw, x = iv, w = mod, y = dv, nboot = 100)
#' # In real analysis, nboot should be at least 2000.
#'
#' # Print the ci
#' stdmod_xyw_boot$ci
#'
#' # Repeat the analysis but keep the results from boot:
#' set.seed(85740917)
#' stdmod_xyw_boot <- stdmod_boot(lm_raw, x = iv, w = mod, y = dv,
#'                                  nboot = 200, full_output = TRUE)
#' # In real analysis, nboot should be at least 2000.
#'
#' # Print the 95% percentile confidence interval
#' stdmod_xyw_boot$ci
#'
#'
#' @export
#' @describeIn stdmod A wrapper of [stdmod()] that computes the nonparametric
#'                     bootstrap confidence interval of the standardized
#'                     moderation effect.
#' @order 2

stdmod_boot <- function(lm_out, ...,
                          nboot = 100, conf = .95,
                          boot_args = NULL, full_output = FALSE) {
    dat <- lm_out$model
    bootfct <- create_boot(lm_out, ...)
    boot_out <- do.call(boot::boot,
                  c(list(data = dat, statistic = bootfct, R = nboot),
                  boot_args))
#    boot_out <- boot::boot(dat, bootfct, R = nboot)
    ci <- boot::boot.ci(boot_out, conf = conf, type = "perc")$percent[4:5]
    out <- list(ci = ci)
    if (full_output) {
        out$boot_out <- boot_out
      } else {
        out$boot_out <- NA
      }
    out
  }
   
create_boot <- function(lm_out, ...) {
  function(d, ind) {
#        print(head(ind))
#        print(head(d[ind, ]))
#        print(colMeans(d[ind, ]))
        stdmod(stats::update(lm_out, data = d[ind, ]), ...)
      }
  }