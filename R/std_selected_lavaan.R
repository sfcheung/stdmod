#' @title Standardize coefficients in a lavaan-Model
#'
#' @description Can standardize selected
#' variables in a `lavaan` model without
#' refitting the models, can handle
#' product term correctly and skip
#' categorical predictors in
#' standardization.
#'
#' @details This function lets users
#' select which variables to be standardized
#' when computing the standardized
#' solution. It has the following
#' features:
#'
#' - It automatically skips predictors
#' which has only two unique values,
#' assuming that they are dummy variables.
#'
#' - It does not standardize product
#' term, which is incorrect. Instead,
#' it compute the product term with
#' its component variables standardized.
#'
#' - It can be used to generate bootstrap
#' confidence intervals for the
#' standardized solution. Bootstrap
#' confidence interval is better than
#' doing standardization *before* fitting
#' a model because it correctly takes
#' into account the sampling variance
#' of the standard deviations. It is
#' also better than delta method
#' confidence interval because it takes
#' into account the usually asymmetric
#' distribution of parameters after
#' standardization.
#'
#' - For comparison, it can also report
#' delta method standard errors and
#' confidence intervals.
#'
#' ## Problems With Common Approaches
#'
#' In most SEM programs, users
#' have limited control on which
#' variables to standardize when
#' requesting the standardized solution.
#' The solution may be uninterpretable
#' or misleading in these conditions:
#'
#' - Dummy variables are standardized
#' and can not be interpreted as the
#' difference between two groups on the
#' outcome variables.
#'
#' - Product terms (interaction terms)
#' are standardized and they cannot be
#' interpreted as the changes in the
#' effects of focal variables when the
#' moderators change (Cheung, Cheung,
#' Lau, Hui, & Vong, 2022).
#'
#' - Variables with meaningful units can
#' be more difficult to interpret when
#' they are standardized (e.g., age).
#'
#' Moreover, the delta method is usually
#' used, which is suboptimal for
#' standardization unless the sample
#' size is large. For example, the
#' covariance with variables standardized
#' is a correlation, and its sampling
#' distribution is skewed unless its
#' population value is zero. However,
#' delta method confidence interval
#' for the correlation is necessarily
#' symmetric around the point estimate.
#'
#' ## Limitations
#'
#' - It only supports observed variable
#' interaction terms, and only support
#' two-way interactions.
#'
#' - It only supports models fitted to
#' raw data.
#'
#' - It does not yet support ordinal
#' endogenous variables (e.g.,
#' ordinal indicators).
#'
#' - Intercepts not supported.
#'
#' - User-defined parameters not
#' supported. Users should take care of
#' appropriate standardization themselves
#' when defining the user parameters.
#'
#' @return
#' A data frame storing the parameter
#' estimates, similar in form to the
#' output of [lavaan::parameterEstimates()].
#'
#' @param object The output of
#' `lavaan` model fit functions, such
#' as [lavaan::sem()] and [lavaan::cfa()].
#'
#' @param to_standardize A string vector,
#' which should be the names of the
#' variables to be standardized.
#' Default is `".all"`, indicating all
#' variables are to be standardized
#' (but see `skip_categorical`).
#'
#' @param not_to_standardize A string
#' vector, which should be the names
#' of the variables that should not be
#' standardized. This argument is useful
#' when most variables, except for a few,
#' are to be standardized. This argument
#' cannot be ued with `to_standardize`
#' at the same time. Default is `NULL`,
#' and only `to_standardize` is used.
#'
#' @param skip_categorical_x Logical.
#' If `TRUE`, the default, all
#' categorical predictors, defined as
#' variables with only two possible
#' values in the data analyzed, will
#' be skipped in standardization. This
#' overrides the argument
#' `to_standardize`. That is, a
#' categorical predictor will not be
#' standardized even if listed in
#' `to_standardize`, unless uses set
#' this argument to `FALSE`.
#'
#' @param output The format of the
#' output. If set to `"data.frame"`,
#' the output will be a data frame like
#' the one from [lavaan::parameterEstimates()].
#' If set to `"text"`, it will be printed
#' in a format similar to the one in
#' the `summary`-method of `lavaan`.
#' (NOTE: `"text"` supported but not
#' fully implemented.)
#'
#' @param std_se String. If set to `"none"`,
#' the default, standard errors will not
#' be computed for the standardized
#' solution. If set to `"delta"`,
#' delta method will be used to compute
#' the standard errors. If est to
#' `"bootstrap"` *and* bootstrap
#' estimates are available in `object`
#' (e.g., bootstrapping is requested
#' when fitting the model in `lavaan`),
#' then bootstrap standard errors and
#' confidence intervals will be
#' computed. (NOTE: Bootstrap SEs
#' and CIs are not yet available.)
#'
#' @param std_z Logical. If `TRUE` and
#' `std_se` is not set to `"none"`,
#' standard error will be computed
#' using the method specified in
#' `std_se`. Default is `TRUE`.
#'
#' @param std_pvalue Logical. If `TRUE`,
#' `std_se` is not set to `"none"`,
#' and `std_z` is `TRUE`, *p*-values
#' will be computed using the method
#' specified in `std_se`. For
#' bootstrapping, the method proposed by
#' Asparouhov and Muthén (2021) is used.
#' Default is `TRUE`.
#'
#' @param std_ci Logical. If `TRUE` and
#' `std_se` is not set to `"none"`,
#' confidence intervals will be
#' computed using the method specified in
#' `std_se`. Default is `FALSE.`
#'
#' @param level The level of confidence
#' of the confidence intervals. Default
#' is .95. It will be used in the
#' confidence intervals of both
#' the unstandardized and
#' standardized solution.
#'
#' @param ... Optional arguments to be
#' passed to the [lavaan::parameterEstimates()],
#' which will be use to generate the
#' output.
#'
#' @param delta_method The method used
#' to compute delta method standard
#' errors. For internal use and should
#' not be changed.
#'
#' @param progress Logical. If `TRUE`,
#' progress bars will be displayed
#' for long process.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Asparouhov, A., & Muthén, B. (2021). Bootstrap p-value computation.
#' Retrieved from https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
#'
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. *Health Psychology*, *41*(7), 502-505.
#' \doi{10.1037/hea0001188}
#'
#' @seealso [std_selected()] and [std_selected_boot()]
#' for multiple regression conducted by [lm()].
#'
#' @examples
#' \donttest{
#' # TO ADD
#' }
#'
#' @export
#'

std_selected_lavaan <- function(object,
                                to_standardize = ".all.",
                                not_to_standardize = NULL,
                                skip_categorical_x = TRUE,
                                output = c("data.frame", "text"),
                                std_se = c("none", "delta", "bootstrap"),
                                std_z = TRUE,
                                std_pvalue = TRUE,
                                std_ci = TRUE,
                                level = .95,
                                progress = TRUE,
                                ...,
                                delta_method = c("numDeriv", "lavaan")) {
    if (!isTRUE(requireNamespace("pbapply", quietly = TRUE)) ||
        !interactive()) {
        progress <- FALSE
      }
    if (!inherits(object, "lavaan")) {
        stop("'object' is not a lavaan-class object.")
      }
    output <- match.arg(output)
    std_se <- tolower(match.arg(std_se))
    has_se <- !identical("none", std_se)
    ngroups <- lavaan::lavTech(object, what = "ngroups")

    # Get the variables to be standardized
    prods <- find_all_products(object)
    to_standardize <- fix_to_standardize(object = object,
                                         to_standardize = to_standardize,
                                         not_to_standardize = not_to_standardize,
                                         skip_categorical_x = skip_categorical_x,
                                         prods = prods)

    # Prepare the tables for the results
    ptable <- lavaan::parameterTable(object)
    est <- lavaan::parameterEstimates(object,
                                      ...,
                                      level = level,
                                      output = output)
    est[, "std.lv"] <- NULL
    est[, "std.nox"] <- NULL
    std <- lavaan::standardizedSolution(object,
                                        se = TRUE,
                                        zstat = TRUE,
                                        pvalue = TRUE,
                                        ci = TRUE,
                                        partable = ptable)
    if (is.null(std$group)) {
        std$group <- ptable$group
      }
    # Generate the function for each parameter with
    # a standardized solution.
    i <- which(!(std$op %in% c("~1", "==", ":=")))
    std_fct <- gen_std(object = object,
                       i = i,
                       to_standardize = to_standardize,
                       prods = prods)

    # Compute the standardized solution
    fit_est <- methods::getMethod("coef",
                  signature = "lavaan",
                  where = asNamespace("lavaan"))(object)
    fit_vcov <- lavaan::lavInspect(object, what = "vcov")
    est_std_full <- lapply(std_fct, function(xx) xx(fit_est))
    est_std <- unlist(est_std_full)
    est_std_by <- lapply(est_std_full,
                         FUN = attr,
                         which = "std_by",
                         exact = TRUE)
    est_std_by <- sapply(est_std_by,
                         function(x) {paste0(x, collapse = ",")})
    # TODO:
    # - Add support for user-defined parameters
    std[i, "std.p"] <- est_std
    std[i, "std.p.by"] <- est_std_by

    # Standard errors
    if (has_se) (
        if ("bootstrap" %in% std_se) {
            # TODO:
            # - Add the option to retrieve do_boot() output
            # - Add the option to call do_boot() internally
            # - Add the option to call bootstrapLavaan internally
            boot_est <- std_boot(object = object,
                                 std_fct = std_fct,
                                 progress = progress)
            # TODO:
            # - Store the boot_est
            est_std_se <- std_se_boot_all(boot_est)
          }
        if ("delta" %in% std_se) {
            est_std_se <- std_se_delta_all(std_fct = std_fct,
                                          fit_est = fit_est,
                                          fit_vcov = fit_vcov,
                                          method = delta_method,
                                          progress = progress)
          }
        std[i, "std.p.se"] <- est_std_se
      }

    # z statistic
    if (has_se && std_z) {
        # Same for delta and bootstrap
        est_std_z <- std[i, "std.p"] / std[i, "std.p.se"]
        est_std_z[std[i, "std.p.se"] < sqrt(.Machine$double.eps)] <- NA
        std[i, "std.p.z"] <- est_std_z
      }

    # p-values
    if (has_se && std_pvalue && std_z) {
        # TODO:
        # - Need to decide how to compute bootstrap p-value
        if ("bootstrap" %in% std_se) {
            est_pvalue <- std_pvalue_boot_all(boot_est)
          }
        if ("delta" %in% std_se) {
            est_pvalue <- std_pvalue_delta_all(est_std_z)
          }
        std[i, "std.p.pvalue"] <- est_pvalue
      }

    # Confidence intervals
    if (has_se && std_ci) {
        if ("bootstrap" %in% std_se) {
            ci <- std_ci_boot_all(x_est = est_std,
                                  x_est_boot = boot_est,
                                  level = level)
          }
        if ("delta" %in% std_se) {
            ci <- std_ci_delta_all(x_est = est_std,
                                   x_se = est_std_se)
          }
        std[i, "std.p.ci.lower"] <- ci[, "ci.lower"]
        std[i, "std.p.ci.upper"] <- ci[, "ci.upper"]
      }

    # Store results in the parameter estimates table
    if (is.null(est$group)) {
        est$group <- 1
        est[est$op %in% c("==", ":="), "group"] <- 0
      }
    std_names <- colnames(std)
    std_to_keep <- std_names[startsWith(std_names, "std.p")]
    est$est_id <- seq_len(nrow(est))
    out <- merge(est,
                 std[, c("lhs", "op", "rhs", "group", std_to_keep)],
                 all.x = TRUE,
                 all.y = FALSE,
                 sort = FALSE)
    out <- out[order(out$est_id), ]
    est[out$est_id, std_to_keep] <- out[, std_to_keep]
    est$est_id <- NULL
    if (ngroups == 1) {
        est$group <- NULL
      }
    class(est) <- c("std_selected_lavaan", class(est))
    est
  }

#' @noRd

fix_to_standardize <- function(object,
                               to_standardize = ".all.",
                               not_to_standardize = NULL,
                               skip_categorical_x = TRUE,
                               prods = NULL) {
    # TODO:
    # - Handle ordinal endogenous variables.
    if (!identical(to_standardize, ".all.") && !is.null(not_to_standardize)) {
        stop("Do not specify both to_standardize and not_to_standardize.")
      }
    if (is.null(prods)) {
        prods <- find_all_products(object)
      }
    all_names <- unique(union(lavaan::lavNames(object, "ov"),
                              lavaan::lavNames(object, "lv")))
    cat_vars <- find_categorical(object)
    if (is.null(not_to_standardize)) {
        if (identical(to_standardize, ".all.")) {
            to_standardize <- all_names
          } else {
            to_standardize <- intersect(all_names, to_standardize)
            if (length(to_standardize) == 0) {
                stop("All variables in to_standardize not in the model.")
              }
          }
      } else {
        to_standardize <- setdiff(all_names, not_to_standardize)
      }
    if ((length(cat_vars) > 0) && skip_categorical_x) {
        to_standardize <- setdiff(to_standardize, cat_vars)
      }
    if (length(prods) > 0) {
        to_standardize <- setdiff(to_standardize, names(prods))
      }
    to_standardize
  }

#' @noRd

std_boot <- function(object,
                     std_fct,
                     progress = FALSE) {
    # TODO:
    # - Can retrieve estimates from do_boot()
    boot_est <- tryCatch(lavaan::lavInspect(object,
                                            what = "boot"),
                         error = function(e) e)
    if (inherits(boot_est, "error")) {
        stop("Bootstrap SEs/CIs requested but bootstrap not used when fitting the model.")
      }
    boot_est_err <- attr(boot_est, "error.idx")
    if (length(boot_est_err) > 0) {
        boot_est <- boot_est[-boot_est_err, ]
      }
    if (progress) {
        cat("\nCompute bootstrapping standardized solution:\n")
        est_std_boot <- pbapply::pbsapply(asplit(boot_est, 1),
                          function(yy) {
                              sapply(std_fct, function(xx) xx(yy))
                            },
                          simplify = TRUE)
      } else {
        est_std_boot <- sapply(asplit(boot_est, 1),
                          function(yy) {
                              sapply(std_fct, function(xx) xx(yy))
                            },
                          simplify = TRUE)
      }
    est_std_boot <- t(est_std_boot)
    est_std_boot
  }

#' @noRd

std_se_boot_all <- function(boot_est) {
    out <- apply(boot_est,
                 MARGIN = 2,
                 FUN = stats::sd)
    out
  }

#' @noRd

std_ci_boot_all <- function(x_est,
                            x_est_boot,
                            level = .95) {
    if (is.null(dim(x_est_boot))) {
        x_est_boot <- matrix(x_est_boot, ncol = 1)
      }
    boot0 <- list(t0 = x_est,
                  t = x_est_boot,
                  R = nrow(x_est_boot))
    p <- ncol(x_est_boot)
    boot_ci_out0 <- lapply(seq_len(p),
        function(xx) {
            tmp <- range(x_est_boot[, xx])
            if (isTRUE(all.equal(tmp[1], tmp[2]))) {
                return(c(x_est[xx], x_est[xx]))
              }
            boot::boot.ci(boot0,
                          type = "perc",
                          index = xx,
                          conf = level)$percent[4:5]
          })
    out <- do.call(rbind, boot_ci_out0)
    colnames(out) <- c("ci.lower", "ci.upper")
    out
  }

#' @noRd

std_pvalue_boot_all <- function(x_est_boot,
                                h0 = 0,
                                min_size = 100,
                                warn = FALSE) {
    if (is.null(dim(x_est_boot))) {
        x_est_boot <- matrix(x_est_boot, ncol = 1)
      }
    p <- ncol(x_est_boot)
    out <- sapply(asplit(x_est_boot, MARGIN = 2),
                  std_pvalue_boot_i,
                  h0 = h0,
                  min_size = min_size,
                  warn = warn)
    out
  }

#' @noRd

std_pvalue_boot_i <- function(x,
                              h0 = 0,
                              min_size = 100,
                              warn = FALSE) {
    # Adapted from manymome:::est2p()
    # Based on the method in
    # https://www.statmodel.com/download/FAQ-Bootstrap%20-%20Pvalue.pdf
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    if (length(x) < min_size) {
        if (warn) {
          warning(paste("Bootstrap p-value not computed. Less than ",
                        min_size,
                        "bootstrap estimates."))
        }
        return(NA)
      }
    b <- length(x)
    m0 <- sum((x < h0))
    out <- 2 * min(m0 / b, 1 - m0 / b)
    out
  }

#' @noRd

std_se_delta_all <- function(std_fct,
                             fit_est,
                             fit_vcov,
                             method = "numDeriv",
                             progress = FALSE) {
    if (progress) {
        cat("\nCompute delta method standard errors:\n")
        out <- pbapply::pbsapply(std_fct,
                                 FUN = std_se_delta,
                                 fit_est = fit_est,
                                 fit_vcov = fit_vcov,
                                 method = method)
      } else {
        out <- sapply(std_fct,
                      FUN = std_se_delta,
                      fit_est = fit_est,
                      fit_vcov = fit_vcov,
                      method = method)
      }
    out
  }

#' @noRd

std_se_delta <- function(std_fct,
                         fit_est,
                         fit_vcov,
                         method = c("numDeriv", "lavaan")) {
    method <- match.arg(method)
    std_a <- switch(method,
      numDeriv = numDeriv::grad(std_fct,
                                x = fit_est),
      lavaan = suppressWarnings(lavaan::lav_func_gradient_complex(std_fct,
                                  x = fit_est)))
    out <- sqrt(colSums(std_a * (fit_vcov %*% std_a)))
    out
  }

#' @noRd

std_ci_delta_all <- function(x_est,
                             x_se,
                             level = .95) {
    pcrit <- abs(stats::qnorm(1 - level) / 2)
    cilo <- x_est - pcrit * x_se
    cihi <- x_est + pcrit * x_se
    out <- cbind(ci.lower = cilo,
                 ci.upper = cihi)
    out
  }

#' @noRd

std_pvalue_delta_all <- function(est_std_z) {
    out <- stats::pnorm(abs(est_std_z), lower.tail = FALSE) * 2
    out
  }

#' @noRd

pt_id_2_est_id <- function(est_table,
                           ptable) {
    if (is.null(est_table$group)) {
        est_table$group <- 1
        est_table[est_table$op %in% c(":=", "=="), "group"] <- 0
      }
    est_table$est_id <- seq_len(nrow(est_table))
    ptable$pt_id <- seq_len(nrow(ptable))
    out <- merge(est_table[, c("lhs", "op", "rhs", "group", "est_id")],
                 ptable[, c("lhs", "op", "rhs", "group", "pt_id")],
                 sort = FALSE)
    out <- out[order(out$est_id), ]
    out
  }

#' @noRd

std_rows <- function(object) {
  ptable <- lavaan::parameterTable(object)
  std <- lavaan::standardizedSolution(object,
                                      se = TRUE,
                                      zstat = TRUE,
                                      pvalue = TRUE,
                                      ci = FALSE,
                                      partable = ptable)
  out <- which(!is.na(std$z) &
              (std$op != "~1"))
  out
}

#' @noRd

gen_std <- function(object,
                    i = NULL,
                    to_standardize = ".all.",
                    prods = NULL) {
    if (is.null(i)) {
        i <- std_rows(object)
      }
    if (is.null(prods)) {
        prods <- find_all_products(object)
      }
    out <- lapply(i,
                  gen_std_i,
                  fit = object,
                  to_standardize = to_standardize,
                  prods = prods)
    out
  }

#' @noRd
# Adapted from semhelpinghands::standardizedSolution_boot_ci()

gen_std_i <- function(fit,
                      i,
                      to_standardize = ".all.",
                      prods = list()) {
    pt <- lavaan::parameterTable(fit)
    p_free <- pt$free > 0
    pt_i <- pt[i, ]

    tmp <- to_standardize_for_i(prods = prods,
                                to_standardize = to_standardize,
                                pt_i = pt_i)
    prod_names <- tmp$prod_names
    to_standardize_i <- tmp$to_standardize_i

    slot_opt <- fit@Options
    slot_pat <- fit@ParTable
    slot_mod <- fit@Model
    slot_smp <- fit@SampleStats
    slot_dat <- fit@Data

    slot_opt1 <- slot_opt
    slot_opt1$do.fit <- FALSE
    slot_opt1$se <- "none"
    slot_opt1$test <- "none"
    slot_opt1$baseline <- FALSE
    slot_opt1$h1 <- FALSE

    std_i_internal <- gen_std_i_internal(fit = fit,
                                         i = i,
                                         to_standardize_i = to_standardize_i,
                                         prod_names = prod_names,
                                         prods = prods)
    out <- function(par) {
        if (missing(par)) {
            par <- get(".x.", parent.frame())
          }
        # Just in case ...
        force(p_free)
        force(slot_opt)
        force(slot_pat)
        force(slot_mod)
        force(slot_smp)
        force(slot_dat)
        force(slot_opt1)
        force(std_i_internal)
        slot_mod1 <- lavaan::lav_model_set_parameters(slot_mod,
                                                      par)
        slot_pat1 <- slot_pat
        slot_pat1$est[p_free] <- par
        slot_pat1$start[p_free] <- par
        slot_opt1$start <- par
        fit_new <- lavaan::lavaan(slotOptions = slot_opt1,
                                  slotParTable = slot_pat1,
                                  slotModel = slot_mod1,
                                  slotSampleStats = slot_smp,
                                  slotData = slot_dat)
        fit_cov_all <- lavaan::lavInspect(fit_new,
                                          what = "cov.all",
                                          drop.list.single.group = FALSE)
        fit_sd_all <- lapply(fit_cov_all, function(x) sqrt(diag(x)))
        i_group <- pt[i, "group"]
        fit_sd_all <- fit_sd_all[[i_group]]
        std_out_i <- lavaan::parameterTable(fit_new)[i, ]
        out0 <- std_i_internal(std_out_i = std_out_i,
                               fit_sd_all = fit_sd_all)
        out0
      }
    out
  }

#' @noRd

gen_std_i_internal <- function(fit,
                               i,
                               to_standardize_i,
                               prod_names,
                               prods) {
    std_out_i <- lavaan::parameterTable(fit)[i, ]
    std_by <- character(0)
    lhs <- std_out_i$lhs
    rhs <- std_out_i$rhs
    op <- std_out_i$op

    n_x_s <- NULL
    n_w_s <- NULL
    d_x_s <- NULL
    d_w_s <- NULL
    n_x_p <- 1
    n_w_p <- 1
    d_x_p <- 1
    d_w_p <- 1

    if (op == "~") {
        if (lhs %in% prod_names) {
            # x-term?
            if (prods[[lhs]]$x %in% to_standardize_i) {
                d_x_s <- prods[[lhs]]$x
                d_x_p <- -1
              }
            # w-term?
            if (prods[[lhs]]$w %in% to_standardize_i) {
                d_w_s <- prods[[lhs]]$w
                d_w_p <- -1
              }
          } else {
            # Product term not in LHS
            if (lhs %in% to_standardize_i) {
                d_x_s <- lhs
                d_x_p <- -1
              }
          }
        if (rhs %in% prod_names) {
            # x-term?
            if (prods[[rhs]]$x %in% to_standardize_i) {
                n_x_s <- prods[[rhs]]$x
                n_x_p <- 1
              }
            # w-term?
            if (prods[[rhs]]$w %in% to_standardize_i) {
                n_w_s <- prods[[rhs]]$w
                n_w_p <- 1
              }
          } else {
            # Product term not in RHS
            if (rhs %in% to_standardize_i) {
                n_x_s <- rhs
                n_x_p <- 1
              }
          }
        std_by <- c(std_by,
                    n_x_s,
                    n_w_s,
                    d_x_s,
                    d_w_s)
      }

    # Covariance or variance
    if (op == "~~") {
        if (lhs %in% prod_names) {
            # x-term?
            if (prods[[lhs]]$x %in% to_standardize_i) {
                d_x_s <- prods[[lhs]]$x
                d_x_p <- -1
              }
            # w-term?
            if (prods[[lhs]]$w %in% to_standardize_i) {
                d_w_s <- prods[[lhs]]$w
                d_w_p <- -1
              }
          } else {
            # Product term not in LHS
            if (lhs %in% to_standardize_i) {
                d_x_s <- lhs
                d_x_p <- -1
              }
          }
        if (rhs %in% prod_names) {
            # x-term?
            if (prods[[rhs]]$x %in% to_standardize_i) {
                n_x_s <- prods[[rhs]]$x
                n_x_p <- -1
              }
            # w-term?
            if (prods[[rhs]]$w %in% to_standardize_i) {
                n_w_s <- prods[[rhs]]$w
                n_w_p <- -1
              }
          } else {
            # Product term not in RHS
            if (rhs %in% to_standardize_i) {
                n_x_s <- rhs
                n_x_p <- -1
              }
          }
        std_by <- c(std_by,
                    n_x_s,
                    n_w_s,
                    d_x_s,
                    d_w_s)
      }

    # Factor loading
    if (op == "=~") {
        if (lhs %in% prod_names) {
            # x-term?
            if (prods[[lhs]]$x %in% to_standardize_i) {
                d_x_s <- prods[[lhs]]$x
                d_x_p <- 1
              }
            # w-term?
            if (prods[[lhs]]$w %in% to_standardize_i) {
                d_w_s <- prods[[lhs]]$w
                d_w_p <- 1
              }
          } else {
            # Product term not in LHS
            if (lhs %in% to_standardize_i) {
                d_x_s <- lhs
                d_x_p <- 1
              }
          }
        if (rhs %in% prod_names) {
            # x-term?
            if (prods[[rhs]]$x %in% to_standardize_i) {
                n_x_s <- prods[[rhs]]$x
                n_x_p <- -1
              }
            # w-term?
            if (prods[[rhs]]$w %in% to_standardize_i) {
                n_w_s <- prods[[rhs]]$w
                n_w_p <- -1
              }
          } else {
            # Product term not in RHS
            if (rhs %in% to_standardize_i) {
                n_x_s <- rhs
                n_x_p <- -1
              }
          }
        std_by <- c(std_by,
                    n_x_s,
                    n_w_s,
                    d_x_s,
                    d_w_s)
      }

    std_by <- unique(std_by)
    out_fct <- function(std_out_i,
                        fit_sd_all) {
        # Just in case ...
        force(n_x_s)
        force(n_w_s)
        force(d_x_s)
        force(d_w_s)
        force(n_x_p)
        force(n_w_p)
        force(d_x_p)
        force(d_w_p)
        force(prods)
        a <- ifelse(is.null(n_x_s),
                    1,
                    fit_sd_all[n_x_s]^n_x_p) *
             ifelse(is.null(n_w_s),
                    1,
                    fit_sd_all[n_w_s]^n_w_p) *
             ifelse(is.null(d_x_s),
                    1,
                    fit_sd_all[d_x_s]^d_x_p) *
             ifelse(is.null(d_w_s),
                    1,
                    fit_sd_all[d_w_s]^d_w_p)
        out0 <- std_out_i$est * a
        attr(out0, "std_by") <- std_by
        out0
      }
    return(out_fct)
  }

#' @noRd

to_standardize_for_i <- function(prods,
                                 to_standardize,
                                 pt_i) {
    if (length(prods) > 0) {
        prod_names <- names(prods)
        tmp1 <- pt_i$lhs
        if (tmp1 %in% prod_names) {
            tmp1 <- c(prods[[tmp1]]$w, prods[[tmp1]]$x)
          }
        tmp2 <- pt_i$rhs
        if (tmp2 %in% prod_names) {
            tmp2 <- c(prods[[tmp2]]$w, prods[[tmp2]]$x)
          }
      } else {
        prod_names <- character(0)
        tmp1 <- pt_i$lhs
        tmp2 <- pt_i$rhs
      }

    to_standardize_i <- unique(union(tmp1, tmp2))
    if (isFALSE(identical(to_standardize, ".all."))) {
        to_standardize_i <- intersect(to_standardize_i, to_standardize)
      }
    list(to_standardize_i = to_standardize_i,
         prod_names = prod_names)
  }

#' @noRd

find_all_products <- function(fit) {
    ptable <- lavaan::parameterTable(fit)
    reg_paths <- all_reg_paths(ptable)
    if (length(reg_paths) == 0) return(list())
    prods <- mapply(manymome::get_prod,
                    x = reg_paths[, "rhs"],
                    y = reg_paths[, "lhs"],
                    MoreArgs = list(fit = fit,
                                    expand = TRUE),
                    SIMPLIFY = FALSE)
    prods_ok <- sapply(prods, function(x) is.list(x))
    if (!any(prods_ok)) {
        return(list())
      }
    prods <- prods[prods_ok]
    prods_ok <- sapply(prods, function(x) !is.null(x$prod))
    if (!any(prods_ok)) {
        return(list())
      }
    prods <- prods[prods_ok]
    prods <- keep_prod_1(prods)
    prods_dup <- !duplicated(sapply(prods, function(x) x$prod))
    prods <- prods[prods_dup]
    prods_names <- sapply(prods, function(x) x$prod)
    names(prods) <- prods_names
    prods
  }

#' @noRd

all_reg_paths <- function(ptable) {
    i <- ptable$op %in% c("~")
    if (length(i) == 0) return(list())
    out <- as.matrix(ptable[i, c("lhs", "rhs")])
    out <- unique(out)
    out
  }

#' @noRd

keep_prod_1 <- function(prods) {
    i <- sapply(prods, function(x) length(x$b))
    out <- prods[i == 1]
    out
  }

#' @noRd

find_categorical <- function(fit,
                             k = 2) {
    fit_datas <- lavaan::lavInspect(fit,
                                    what = "data",
                                    drop.list.single.group = FALSE)
    out <- lapply(fit_datas,
                  find_categorical_i,
                  k = 2)
    out1 <- lapply(out,
                   function(x) names(x[which(x)]))
    out2 <- unlist(out1, use.names = FALSE)
    out2 <- unique(out2)
    out2
  }

#' @noRd

find_categorical_i <- function(fit_data,
                               k = 2) {
    out <- apply(fit_data,
                 MARGIN = 2,
                 find_k,
                 k = k)
    out
  }

#' @noRd
# Fast in identifying a column with more than k unique values
# Slow in identifying a column with k or few unique values

find_k <- function(x,
                   k = 2) {
    x <- stats::na.omit(x)
    if (length(x) < 2) return(NA)
    xs <- numeric(k)
    xs[] <- NA
    j <- 0
    for (i in seq_along(x)) {
        if (x[i] %in% xs) {
            next
          } else {
            j <- j + 1
            if (j > k) {
                return(FALSE)
              } else {
                xs[j] <- x[i]
              }
          }
      }
    return(TRUE)
  }
