#' @details
#' [std_selected_boot()] is a wrapper of [std_selected()]. It calls
#' [std_selected()]
#' once
#' for each bootstrap sample, and then computes the nonparametric
#' bootstrap
#' percentile confidence intervals (Cheung, Cheung, Lau, Hui, & Vong, 2022).
#'
#' If `do_boot` is `FALSE`, then the object it returns is identical to that
#' by [std_selected()].
#'
#' This function intentionally does not have an argument for setting the seed
#' for
#' random number. Users are recommended to set the seed, e.g., using
#' [set.seed()]
#' before calling it, to ensure reproducibility.
#'
#' @return
#' Like [std_selected()], [std_selected_boot()] returns the updated [lm()]
#' output, with the class `std_selected` added. The output of [std_selected_boot()]
#' contain these additional elements in the list:
#'
#' - `boot_ci`: A data frame of the bootstrap confidence intervals of the
#'              regression coefficient.
#'
#' - `nboot`: The number of bootstrap samples requested.
#'
#' - `conf`: The level of confidence, in proportion.
#'
#' - `boot_est`: A matrix of the bootstrap estimates of the regression coefficients.
#'               The number of rows equal to `nboot`, and the number of columns
#'               equal to the number of terms in the regression model.
#'
#' - `std_selected_boot_call`: The call to `std_selected_boot()`.
#'
#' - `boot_out`: If available, the original output from `boot::boot()`.
#'
#' @param lm_out The output from [lm()].
#' @param conf The level of confidence for the confidence interval.
#'              Default is .95.
#' @param nboot The number of bootstrap samples. Default is 100.
#' @param boot_args A named list of arguments to be passed to [boot::boot()].
#'                 Default is `NULL`.
#' @param save_boot_est If `TRUE`, the default, the bootstrap estimates will
#'                      be saved in the element
#'                      `boot_est` of the output.
#' @param full_output Whether the full output from [boot::boot()] is returned.
#'                    Default is `FALSE`. If `TRUE`, the full output from
#'                    [boot::boot()] will be saved in the element `boot_out`
#'                    of the output.
#' @param do_boot Whether bootstrapping confidence intervals will be formed.
#'                Default is `TRUE`. If `FALSE`, all arguments related to
#'                bootstrapping will be ignored.
#'
#' @examples
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#' head(dat)
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#' # Standardize all variables as in std_selected above, and compute the
#' # nonparametric bootstrapping percentile confidence intervals.
#' set.seed(87053)
#' lm_std_boot <- std_selected_boot(lm_raw,
#'                                  to_scale = ~ .,
#'                                  to_center = ~ .,
#'                                  conf = .95,
#'                                  nboot = 100)
#' # In real analysis, nboot should be at least 2000.
#' summary(lm_std_boot)
#'
#' # Use to_standardize as a shortcut
#' set.seed(87053)
#' lm_std_boot2 <- std_selected_boot(lm_raw,
#'                                   to_standardize = ~ .,
#'                                   conf = .95,
#'                                   nboot = 100)
#' # The results are the same
#' confint(lm_std_boot)
#' confint(lm_std_boot2)
#' all.equal(confint(lm_std_boot), confint(lm_std_boot2))
#'
#'
#' @export
#' @describeIn std_selected A wrapper of [std_selected()] that forms
#'                           nonparametric bootstrap confidence intervals.
#' @order 2

std_selected_boot <- function(lm_out,
                              to_scale = NULL,
                              to_center = NULL,
                              to_standardize = NULL,
                              conf = .95,
                              nboot = 100,
                              boot_args = NULL,
                              save_boot_est = TRUE,
                              full_output = FALSE,
                              do_boot = TRUE) {
    if (missing(lm_out)) {
        stop("The arguments lm_out cannot be empty.")
      }
    my_call <- match.call()
    my_call[[1]] <- quote(std_selected)
    args0 <- formals(std_selected_boot)
    my_call_names <- names(my_call[-1])
    for (xx in names(args0)) {
        if (!(xx %in% my_call_names)) {
            if (!is.null(args0[[xx]])) {
                my_call[[xx]] <- args0[[xx]]
              }
          }
      }
    eval(my_call, envir = parent.frame())
  }

create_boot_selected <- function(lm_out,
                                 to_scale,
                                 to_center,
                                 to_standardize) {
  function(d, ind) {
        force(lm_out)
        lm_out_i <- lm_out
        lm_out_i$model <- d[ind, ]
        out <- std_selected(lm_out = lm_out_i,
                            to_scale = to_scale,
                            to_center = to_center,
                            to_standardize = to_standardize)
        stats::coef(out)
      }
  }