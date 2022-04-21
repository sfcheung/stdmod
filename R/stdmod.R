#' @title Standardized Moderation Effect Given an [lm()] Output
#'
#' @description Compute the standardized moderation effect in a moderated
#'                regression model.
#'
#' @details Compute the standardized moderation effect given an
#' [lm()] output. Users specify
#' the moderator, the predictor, the outcome variable, and the corresponding
#' standardized moderation
#' effect. Users can also select which variable(s) will be standardized.
#'
#' @return
#' The standardized moderation effect.
#'
#' @param lm_out The output from [lm()].
#' @param x      The independent variable, that is, the variable with its effect
#'              being moderated. If supplied, its standard deviation will be
#'              used
#'              for rescaling. Default is `NULL`.
#' @param w      The moderator. If supplied, its standard deviation will be
#'              used
#'              for rescaling. Default is `NULL`.
#' @param y      The dependent (outcome) variable. If supplied, its standard
#'              deviation will be used for rescaling. Default is NULL.
#' @param x_rescale  If `TRUE`, will rescale x by its standard deviation.
#'                   Default is `TRUE`.
#' @param w_rescale  If `TRUE`, will rescale w by its SD. Default is TRUE.
#' @param y_rescale  If `TRUE`, will rescale y by its standard deviation.
#'                   Default is `TRUE`.
#'
#' @examples
#'
#' # Load a test data of 500 cases
#' # It has one predictor (iv), one moderator (mod), two covariates (v1 and v2),
#' # and one dv (dv). All variables continuous.
#' dat <- test_x_1_w_1_v_2_n_500
#'
#' # Do regression as usual:
#' lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
#' summary(lm_raw)
#'
#' # The standard deviations of iv, dv, and mod:
#' sds <- apply(dat, 2, sd)
#' sds
#'
#' # Compute the standardized moderation effect:
#' stdmod_xyw <- stdmod(lm_raw, x = iv, y = dv, w = mod)
#' stdmod_xyw
#' # By default, all three variables will be standardized.
#'
#' # Check against self-computed standardized moderation effect:
#' coef(lm_raw)["iv:mod"] * sds["iv"] * sds["mod"] / sds["dv"]
#'
#' # Standardize only the iv, i.e., do not standardized dv and the moderator:
#' stdmod_x <- stdmod(lm_raw, x = iv, y = dv, w = mod,
#'                    x_rescale = TRUE,  y_rescale = FALSE, w_rescale = FALSE)
#' stdmod_x
#' # Check against self-computed moderation effect with only iv standardized:
#' coef(lm_raw)["iv:mod"] * sds["iv"]
#'
#' @export
#' @describeIn stdmod The base function for computing standardized
#'             moderation effect
#' @order 1

stdmod <- function(lm_out, x = NULL, w = NULL, y = NULL,
                           x_rescale = TRUE,
                           w_rescale = TRUE,
                           y_rescale = TRUE) {
    mycall <- match.call()
    if (any(c(is.null(mycall$x), is.null(mycall$w), is.null(mycall$y)))) {
        stop("The arguments x, w, and y cannot be NULL.")
      }
    x_name <- deparse(substitute(x))
    w_name <- deparse(substitute(w))
    y_name <- deparse(substitute(y))
    b_names <- names(stats::coef(lm_out))
    mod_name1 <- paste0(x_name, ":", w_name)
    mod_name2 <- paste0(w_name, ":", x_name)
    mod_pos <- grepl(mod_name1, b_names) | grepl(mod_name2, b_names)
    if (!any(mod_pos)) {
        stop("The product term not found in the lm output.")
      }
    b_xw <- stats::coef(lm_out)[mod_pos]
    model_names <- colnames(lm_out$model)
    if (x_rescale) {
        if (!(x_name %in% model_names)) {
            stop("x not in the data frame")
          }
        x_dat <- eval(substitute(x),lm_out$model, parent.frame())
        x_sd  <- stats::sd(x_dat)
      } else {
        x_dat <- NULL
        x_sd  <- 1
      }
     if (w_rescale) {
        if (!(w_name %in% model_names)) {stop("w not in the data frame")}
        w_dat <- eval(substitute(w), lm_out$model, parent.frame())
        w_sd  <- stats::sd(w_dat)
      } else {
        w_dat <- NULL
        w_sd  <- 1
      }
    if (y_rescale) {
        if (!(y_name %in% model_names)) {stop("y not in the data frame")}
        y_dat <- eval(substitute(y), lm_out$model, parent.frame())
        y_sd  <- stats::sd(y_dat)
      } else {
        y_dat <- NULL
        y_sd  <- 1
      }
    out <- ((x_sd) * (w_sd) / (y_sd)) * b_xw
    out
  }