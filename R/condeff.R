#' @title Conditional Effects
#'
#' @description Compute the conditional effects in a moderated regression model.
#'
#'
#' @return
#' A data frame of the conditional effects.
#'
#' @param lm_out The output from [stats::lm()]. It can also accept the output
#'               from
#'               [std_selected()] or [std_selected_boot()].
#' @param x      The independent variable, that is, the variable with its effect
#'              being moderated. It must be a numeric variable.
#' @param w      The moderator. Both numeric variables and categorical variables
#'               (character or factor) are supported.
#' @param w_method How to define "low", "medium", and "high" for the moderator
#'                  levels.
#'                  Default is in terms of mean and
#'                  standard deviation (SD) of the moderator, `"sd"`:
#'                  "low", "medium", and "high" are one SD below mean,
#'                  mean, and one SD above mean, respectively.
#'                  If equal to
#'                  `"percentile"`, then percentiles of the moderator in
#'                  the
#'                  dataset are used: "low", "medium", and "high" are
#'                  16th, 50th (median), and 84th percentiles, respectively.
#'                  Ignored if `w` is categorical.
#' @param w_percentiles If `w_method` is `"percentile"`, then this
#'                      argument
#'                      specifies the three percentiles to be used,
#'                      divided by 100.
#'                        It must be a
#'                      vector of two numbers. The default is
#'                      `c(.16, .50, .84)`,
#'                      the 16th, 50th, and 84th percentiles,
#'                      which corresponds approximately
#'                      to one SD below and above mean in a
#'                      normal distribution, respectively.
#'                      Ignored if `w` is categorical.
#' @param w_sd_to_percentiles If `w_method` is `"percentile"` and
#'                            this argument is
#'                            set to a number, this number will be
#'                            used to
#'                            to determine the percentiles to be used.
#'                            The
#'                            lower percentile is the percentile in a
#'                            normal
#'                            distribution
#'                            that is `w_sd_to_percentiles` SD below
#'                            the mean.
#'                            The upper percentile is the percentile in
#'                            a normal
#'                            distribution that is `w_sd_to_percentiles`
#'                            SD
#'                            above the mean. Therefore, if
#'                            `w_sd_to_percentiles` is set to 1, then the
#'                            lower
#'                            and upper percentiles are 16th and 84th,
#'                            respectively. Default is `NA`.
#' @param w_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the moderator. Default is 1.
#'                          Ignored if `w` is categorical.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#' # TODO
#'
#' @export

cond_effect <- function(output,
                      x = NULL,
                      w = NULL,
                      w_method = c("sd", "percentile"),
                      w_percentiles = c(.16, .50, .84),
                      w_sd_to_percentiles = NA,
                      w_from_mean_in_sd = 1
                      ) {
    mf0 <- model.frame(output)
    w_method <- match.arg(w_method)
    x0 <- deparse(substitute(x))
    if (inherits(tryCatch(x00 <- as.character(x), error = function(e) e),
                 "simpleError")) {
        x <- x0
      } else {
        x <- x00
      }
    w0 <- deparse(substitute(w))
    if (inherits(tryCatch(w00 <- as.character(w), error = function(e) e),
                 "simpleError")) {
        w <- w0
      } else {
        w <- w00
      }
    y <- colnames(stats::model.frame(output))[
                    attr(stats::terms(output), "response")
                  ]
    x_numeric <- is.numeric(mf0[, x])
    w_numeric <- is.numeric(mf0[, w])
    if (!x_numeric) {
        stop("x variable must be a numeric variable.")
      }
    output0 <- output
    if (inherits(output, "std_selected")) {
        tmp <- class(output0)
        class(output0) <- tmp[!(tmp %in% "std_selected")]
      }
    if (w_numeric) {
        w_levels <- gen_levels(mf0[, w],
                              method = w_method,
                              from_mean_in_sd = w_from_mean_in_sd,
                              levels = c(-1, 0, 1),
                              sd_to_percentiles = w_sd_to_percentiles,
                              sd_levels = w_from_mean_in_sd,
                              percentiles = w_percentiles)
        w_levels <- sort(w_levels, decreasing = TRUE)
        w_levels_labels <- c("High", "Medium", "Low")
      } else {
        w_levels <- levels(as.factor(mf0[, w]))
        w_levels_labels <- w_levels
      }
    if (w_numeric) {
        out_all <- lapply(w_levels, cond_numeric_i,
                          output = output0,
                          w = w)
      } else {
        out_all <- lapply(w_levels, cond_categorical_i,
                          output = output0,
                          w = w)
      }
    coef_all <- lapply(out_all, function(xi) summary(xi)$coefficients[x, ])
    out0 <- do.call(rbind, coef_all)
    out0 <- data.frame(Level = w_levels_labels,
                       w = w_levels,
                       out0, check.names = FALSE)
    colnames(out0)[2] <- w
    colnames(out0)[3] <- "x's Effect"
    out0
  }

cond_numeric_i <- function(w_i, output, w) {
    mf_i <- stats::model.frame(output)
    mf_i[, w] <- mf_i[, w] - w_i
    out <- stats::update(output, data = mf_i)
    out
  }

cond_categorical_i <- function(w_i, output, w) {
    mf_i <- stats::model.frame(output)
    mf_i[, w] <- stats::relevel(as.factor(mf_i[, w]),
                                ref = w_i)
    out <- stats::update(output, data = mf_i)
    out
  }