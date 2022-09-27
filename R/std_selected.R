#' @title Standardize Variables in a Regression Model
#'
#' @description Standardize, mean center, or scale by standard deviation
#'              selected variables in a regression model and refit the model
#'
#' @details
#' [std_selected()] was originally developed to compute the standardized
#' moderation effect and the standardized coefficients for other predictors
#' given an [lm()] output (Cheung, Cheung, Lau, Hui, & Vong, 2022).
#' It has been extended such that users can specify
#' which variables in a regression model are to be mean-centered and/or
#' rescaled by
#' their standard deviations. If the model has one or more interaction terms,
#' they will be formed after the transformation, yielding the correct
#' standardized solution for a moderated regression model. Moreover,
#' categorical predictors will be automatically skipped in mean-centering
#' and rescaling.
#'
#' Standardization is conducted when a variable is mean-centered and then
#' rescaled by its standard deviation. Therefore, if the goal is to get the
#' standardized solution of a moderated regression, users
#' just instruct the function to standardize all non-categorical variables
#' in the regression model.
#'
#' @return
#' The updated [lm()] output, with the class `std_selected` added. It will be
#' treated as a usual [lm()] object by most functions. These are the major
#' additional element in the list:
#'
#'  - `scaled_terms`: If not `NULL`, a character vector of the variables scaled.
#'
#'  - `centered_terms`: If not `NULL`, a character vector of the variables mean-centered.
#'
#'  - `scaled_by`: A numeric vector of the scaling factors for all the variables in
#'                 the model. The value is 1 for terms not scaled.
#'
#'  - `centered_by`: A numeric vector of the numbers used for centering for all
#'                  the variables in the model. The value is 0 for
#'                   terms not centered.
#'
#'  - `std_selected_call`: The original call.
#'
#'  - `lm_out_call`: The call in `lm_out`.
#'
#' @param lm_out The output from [lm()]
#' @param to_scale The terms to be rescaled by standard deviation,
#'       specified by a formula as in [lm()]. For example, if the terms to be scaled
#'       are `x1` and `x3`, use `~ x1 + x3`. No need to specify the
#'       interaction
#'       term. To scale the outcome variable, list it on the *right hand side*
#'       as a predictor.
#'       Specify only the original variables. If `NULL`, then no terms
#'       will be rescaled by their standard deviations. Variables that are not
#'       numeric will be ignored. Default is `NULL`.
#' @param to_center The terms to be mean centered, specified by a formula
#'        as in [lm()]. For example, if the terms to be centered
#'        is `x1` and `x3`, use `~ x1 + x3`. No need to specify the
#'        interaction term. To center  the outcome variable, list it on the
#'        *right hand side*
#'        as a predictor.
#'        Specify only the original variables. If `NULL`, then no term
#'        will be centered. Default is `NULL`.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. *Health Psychology*, *41*(7), 502-505.
#' \doi{10.1037/hea0001188}
#'
#' @examples
#'
#' # Load a sample data set
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#' head(dat)
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#'
#' # Mean center mod only
#' lm_cw <- std_selected(lm_raw, to_center = ~ mod)
#' summary(lm_cw)
#'
#' # Mean center mod and iv
#' lm_cwx <- std_selected(lm_raw, to_center = ~ mod + iv)
#' summary(lm_cwx)
#'
#' # Standardize both mod and iv
#' lm_stdwx <- std_selected(lm_raw, to_scale = ~ mod + iv,
#'                                to_center = ~ mod + iv)
#' summary(lm_stdwx)
#'
#' # Standardize all variables except for categorical variables.
#' # Interaction terms are formed after standardization.
#' lm_std <- std_selected(lm_raw, to_scale = ~ .,
#'                                to_center = ~ .)
#' summary(lm_std)
#'
#' @export
#' @describeIn std_selected The base function to center or
#'             scale selected variables in a regression model
#' @order 1

std_selected <- function(lm_out,
                         to_scale = NULL,
                         to_center = NULL) {
    if (missing(lm_out)) {
        stop("The arguments lm_out cannot be empty.")
      }

    # Collect the terms.
    # Get the data frame.
    # Do the scaling and centering.
    # Update the results.
    # Return the results.

    # Get the data frame

    dat <- lm_out$model
    k <- ncol(dat)

    # Collect the terms

    if (is.null(to_scale)) {
#        scale_terms <- colnames(lm_out$model)
        scale_terms <- NULL
      } else {
        scale_f <- stats::as.formula(to_scale)
        scale_terms  <- attr(stats::terms(scale_f, data = dat),  "term.labels")
      }

    if (is.null(to_center)) {
#        center_terms <- colnames(lm_out$model)
        center_terms <- NULL
      } else {
        center_f <- stats::as.formula(to_center)
        center_terms <- attr(stats::terms(center_f, data = dat), "term.labels")
      }

    # Check if the terms are valid

    varnames <- colnames(dat)

    if (!all(scale_terms %in% varnames)) {
        stop("Not all terms in to_scale is in the data frame.")
      }

    if (!all(center_terms %in% varnames)) {
        stop("Not all terms in to_center is in the data frame.")
      }

    # Prepare for Centering

#    var_means <- colMeans(dat)
    var_means <- suppressWarnings(sapply(dat, mean))

    var_a <- rep(0, k)
    names(var_a) <- colnames(dat)

    var_a[center_terms] <- var_means[center_terms]

    # Prepare for Scaling

#    var_sds <- apply(dat, 2, sd)
    var_sds <- suppressWarnings(sapply(dat, sd2))

    var_b <- rep(1, k)
    names(var_b) <- colnames(dat)

    var_b[scale_terms] <- var_sds[scale_terms]

    # Do centering and scaling

    # dat_mod <- sweep(dat,     2, var_a, FUN = "-")
    # dat_mod <- sweep(dat_mod, 2, var_b, FUN = "/")

    dat_mod <- sweep2(dat, var_a, FUN = "-")
    dat_mod <- sweep2(dat_mod, var_b, FUN = "/")

    # Do the regression

    lm_out_mod <- stats::update(lm_out, data = dat_mod)

    class(lm_out_mod) <- c("std_selected", class(lm_out))

    lm_out_mod$scaled_terms   <- scale_terms
    lm_out_mod$centered_terms <- center_terms
    lm_out_mod$scaled_by   <- var_b
    lm_out_mod$centered_by <- var_a
    lm_out_mod$std_selected_call <- match.call()
    lm_out_mod$lm_out_call <- stats::getCall(lm_out)
    lm_out_mod

  }