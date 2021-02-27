# Compute the bootstrapping CI of the standardized moderation effect given the \code{lm} output
#'
# Compute the bootstrapping CI of the standardized moderation effect given the \code{lm} output
#'
#' @details
#' 
#' [stdmod_bootci()] is a wrapper of [stdmod()]. It computes the bootstrapping
#'  confidence interval of the standardized moderation effect given 
#'  the \code{lm} output.
#'
# The standardized moderation effect.
#'
#' @param lm_out The output from \code{lm}.
#' @param ...    Parameters to be passed to stdmod
#' @param nboot  The number of bootstrapping samples. Default is 100
#' @param boot_args A named list of arguments to be passed to \code{boot}. Default
#'                 is NULL.
#' @param full_output Whether the full output from \code{boot} is return. Default is 
#'                   FALSE.
#'
#' @examples
#'
#' \donttest{
#'
#' dat <- test_x_1_w_1_v_2_n_500
#' # Do regression as usual:
#' lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
#' 
#' # Compute the standardized moderation effect, generates its confidence interval 
#' # by nonparametric bootstrappinng.
#' set.seed(85740917)
#' stdmod_xyw_boot <- stdmod_bootci(lm_raw, x = iv, w = mod, y = dv, nboot = 500)
#' 
#' # Print the ci
#' stdmod_xyw_boot$ci
#' 
#' # Repeat the analysis but keep the results from boot:
#' set.seed(85740917)
#' stdmod_xyw_boot <- stdmod_bootci(lm_raw, x = iv, w = mod, y = dv, 
#'                                  nboot = 500, full_output = TRUE)
#' 
#' # Print the 95% percential confidence interval
#' stdmod_xyw_boot$ci
#' 
#' # Print the boot output, default is the point estimate:
#' stdmod_xyw_boot$boot_out
#' } 
#'
#' @export
#' @describeIn stdmod A wrapper of [std_mod()] that compute the nonparametric
#'                     bootstrapping confidence interval of the standardized
#'                     moderation effect
#' @order 2

stdmod_bootci <- function(lm_out, ...,
                          nboot = 100, boot_args = NULL, full_output = FALSE) {
    dat <- lm_out$model
    bootfct <- create_boot(lm_out, ...)
    boot_out <- do.call(boot::boot, 
                  c(list(data = dat, statistic = bootfct, R = nboot), 
                  boot_args)) 
#    boot_out <- boot::boot(dat, bootfct, R = nboot)
    ci <- boot::boot.ci(boot_out, type = "perc")$percent[4:5]
    list(ci = ci,
         boot_out = ifelse(full_output, boot_out, NA))
  }
  
  
create_boot <- function(lm_out, ...) {
  function(d, ind) {
#        print(head(ind))
#        print(head(d[ind, ]))
#        print(colMeans(d[ind, ]))
        stdmod(stats::update(lm_out, data = d[ind, ]), ...)
      }
  }
  