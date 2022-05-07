#' @details
#' This function is a wrapper of [cond_effect()]. It calls [cond_effect()]
#' once for each bootstrap sample, and then computes the nonparametric
#' bootstrap percentile confidence intervals (Cheung, Cheung, Lau, Hui,
#' & Vong, 2022). If the output object is the output of [std_selected()]
#' or [std_selected_boot()], in which mean-centering and/or standardization
#' have been conducted, they will be repeated in each bootstrap sample.
#' Therefore, like [std_selected_boot()], it can be used for form
#' nonparametric bootstrap confidence intervals for standardized
#' effects, though [cond_effect_boot()] does this for the standardized
#' conditional effects.
#'
#' This function intentionally does not have an argument for setting the seed
#' for
#' random number. Users are recommended to set the seed, e.g., using
#' [set.seed()]
#' before calling it, to ensure reproducibility.
#'
#' @return
#' A data-frame-like object of the conditional effects. The class is
#' `cond_effect` and the print method will print additional information of
#' the conditional effects.
#'
#' @param output The output from [stats::lm()]. It can also accept the output
#'               from
#'               [std_selected()] or [std_selected_boot()].
#' @param ...  Arguments to be passed to [cond_effect()].
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
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#'
#' # Load a sample data set
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#'
#' lm_std <- std_selected(lm_raw, to_scale = ~ iv + mod, to_center = ~ iv + mod)
#' cond_effect(lm_std, x = iv, w = mod)
#'
#' # Form nonparametric bootstrap confidence intervals
#' out <- cond_effect_boot(lm_std, x = iv, w = mod, nboot = 50)
#' out
#'
#' @export
#' @describeIn cond_effect A wrapper of [cond_effect()] that forms
#'             nonparametric bootstrap confidence intervals.
#' @order 2

cond_effect_boot <- function(output, ...,
                              conf = .95,
                              nboot = 100,
                              boot_args = NULL,
                              save_boot_est = TRUE,
                              full_output = FALSE) {
    dat <- stats::model.frame(output)
    bootfct <- create_boot_cond_effect(output, ...)
    boot_out <- do.call(boot::boot,
              c(list(data = dat,
                     statistic = bootfct,
                     R = nboot),
                     boot_args))
    p <- length(boot_out$t0)
    cis <- t(sapply(seq_len(p), function(x) {
                boot::boot.ci(boot_out, conf = conf,
                              type = "perc", index = x)$percent[4:5]
              }))
    colnames(cis) <- c("CI Lower", "CI Upper")
    cond_effect_out <- cond_effect(output, ...)
    rownames(cis) <- cond_effect_out$Level
    attr(cond_effect_out, "boot_ci") <- cis
    cse <- which(colnames(cond_effect_out) == "Std. Error")
    cond_effect_out <- insert_columns(dat = cond_effect_out,
                                      x = cis,
                                      after = cse - 1)
    attr(cond_effect_out, "nboot") <- nboot
    attr(cond_effect_out, "conf") <- conf
    tmp <- boot_out$t
    colnames(tmp) <- cond_effect_out$Level
    attr(cond_effect_out, "boot_est") <- tmp
    attr(cond_effect_out, "cond_effect_boot_call") <- match.call()
    if (full_output) {
        attr(cond_effect_out, "boot_out") <- boot_out
      }
    cond_effect_out
  }

create_boot_cond_effect <- function(output, ...) {
  function(d, ind) {
        force(output)
        dat_i <- d[ind, ]
        out_i <- stats::update(output, data = dat_i)
        cond_effect(out_i, ...)[, 3]
      }
  }

insert_columns <- function(dat, x, after = 1) {
    k0 <- ncol(dat)
    names0 <- colnames(dat)
    if (is.null(dim(x))) {
        x <- cbind(x)
      }
    names1 <- colnames(x)
    k1 <- ncol(x)
    out <- dat
    names2 <- c(names0[1:after], names1, names0[(after + 1):k0])
    out[, (after + k1):(k0 + k1)] <- out[, after:k0]
    out[, (after + 1):(after + k1)] <- x
    colnames(out) <- names2
    out
  }
