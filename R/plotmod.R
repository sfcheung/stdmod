#' @title Plot the moderation effect in a path model
#'
#' @description Plot the moderation effect in a path model
#'
#' @details This function extracts the information stored
#'          in the `lavaan` fit object to plot a two-line
#'          graph, one for the relation between the focal variale (`x`) and
#'          the outcome variable (`y`) when the moderator (`w`) is one stanard
#'          deviation below mean, and one when the moderator is one standard
#'          deviation above mean.
#'
#' @return
#'  A [ggplot2] graph.
#'
#' @param fit The output from [lavaan::lavaan] or its wrapper, such as
#'            [lavaan::sem].
#' @param y The name of the outcome variable as in the data set in `fit`. It
#'          can be the name of the variable, with or without quotes. 
#' @param x The name of the focal variable as in the data set in
#'           It
#'          can be the name of the variable, with or without quotes.`fit`.
#' @param w The name of the moderator as in the data set in `fit`.
#'           It
#'          can be the name of the variable, with or without quotes.
#' @param xw The name of the product term, `x * w`. If not supplied,
#'           The function will try to find it in the data set.
#'             It
#'          can be the name of the variable, with or without quotes.
#' @param x_label The label for the X-axis. Default is the vlaues of `x`.
#' @param w_label The label for the legend for the lines. Default is the value of`w`.
#' @param y_label The label for the Y-axis. Default is the value of `y`.
#' @param title The title of the graph. If not supplied, will be generated from the variable
#'               names.
#' @param a_shift Default is 0. Can be ignored for now.
#' @param expansion How much tha lower and upper limits of the axis will be adjusted.
#' @param standardized Logical. Plot the moderation effect in standardized metric. All three
#'                     variables, `x`, `w`, and `y` will be standardized. Default
#'                     is `FALSE`
#' @param digits Number of decimal digits to print. Default is 3.
#' @param x_from_mean_in_sd How many SD from mean is used to define "low" and
#'                          "high" for the focal variable. Default is 1.
#' @param w_from_mean_in_sd How many SD from mean is used to define "low" and
#'                          "high" for the moderator. Default is 1.
#' @param w_method How to define "high" and "low" for the moderator levels.
#'                  Default is in terms of the
#'                  standard deviation of the moderator, "sd". If equal to
#'                  "percentile", then percentiles of the moderator in the
#'                  dataset is used.
#' @param w_percentiles If `w_method` is "percentile", then this argument
#'                      specifies the two percentiles to be used, divided by 100.
#'                        It must be a
#'                      vector of two numbers. The default is `c(.16, .84)`,
#'                      the 16th and 84th percentiles,
#'                      which corresponds approximately
#'                      to one SD below and above mean for a
#'                      normal distributoin, respectively.
#' @param x_method How to define "high" and "low" for the focal variable levels.
#'                  Default is in terms of the
#'                  standard deviation of the focal variable, "sd". If equal to
#'                  "percentile", then percentiles of the focal variable in the
#'                  dataset is used.
#' @param x_percentiles If `x_method` is "percentile", then this argument
#'                      specifies the two percentiles to be used, divided by 100.
#'                        It must be a
#'                      vector of two numbers. The default is `c(.16, .84)`,
#'                      the 16th and 84th percentiles,
#'                      which corresponds approximately
#'                      to one SD below and above mean for a
#'                      normal distributoin, respectively.
#' @param w_sd_to_percentiles If `w_method` is "percentile" and this argument is
#'                            set to a number, this number will be used to
#'                            to determine the percentiles to be used. The
#'                            lower percentile is the percentile in a normal
#'                            distribution
#'                            that is `w_sd_to_percentiles` SD below the mean.
#'                            The upper percentile is the percentil in a normal
#'                            distribution that is `w_sd_to_percentiles` SD
#'                            above the mean. Therefore, if
#'                            `w_sd_to_percentiles` is set to 1, then the lower
#'                            and upper percentiles are 16th and 84th,
#'                            respectively.
#' @param x_sd_to_percentiles If `x_method` is "percentile" and this argument is
#'                            set to a number, this number will be used to
#'                            to determine the percentiles to be used. The
#'                            lower percentile is the percentile in a normal
#'                            distribution
#'                            that is `x_sd_to_percentiles` SD below the mean.
#'                            The upper percentile is the percentil in a normal
#'                            distribution that is `x_sd_to_percentiles` SD
#'                            above the mean. Therefore, if
#'                            `x_sd_to_percentiles` is set to 1, then the lower
#'                            and upper percentiles are 16th and 84th,
#'                            respectively.
#' @param plot_x_vlines If supplied, vertical lines to indicate the levels of
#'                       the focal variable will be plotted. This should be a 
#'                       vector of numbers, indicating the levels to be plotted.
#'                       How these numbers are interpreted depends on
#'                       `x_vlines_unit`.
#' @param x_vlines_unit If equal to "sd", then the values of `plot_x_vlines`
#'                       will be interpreted as the deviation from the mean.
#'                       For example, 1 is 1 SD above mean, and -1 is 1 SD
#'                       below mean. If equal to "percentile", then the numbers,
#'                       mulipltied by 100, are the precentiles. For example,
#'                       .25 is the 25th percentile, and .75 is the 75th
#'                       percentile.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
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
#' lm_std_boot <- std_selected_boot(lm_raw,
#'                                  to_scale = ~ .,
#'                                  to_center = ~ .,
#'                                  conf = .95,
#'                                  nboot = 100)
#' # In real analysis, nboot should be at least 2000.
#' summary(lm_std_boot)
#'
#' @export

plotmod <- function(output, y, x, w, xw,
                            x_label,
                            w_label,
                            y_label,
                            title,
                            a_shift = 0,
                            expansion = .1,
                            standardized = FALSE,
                            digits = 3,
                            x_from_mean_in_sd = 1,
                            w_from_mean_in_sd = 1,
                            w_method = "sd",
                            w_percentiles = c(.16, .84),
                            x_method = "sd",
                            x_percentiles = c(.16, .84),
                            w_sd_to_percentiles,
                            x_sd_to_percentiles,
                            plot_x_vlines,
                            x_vlines_unit = "sd"
                    ) {
    w_method <- tolower(w_method)
    if (!w_method %in% c("sd", "percentile")) {
        stop('The argument w_method must either be "sd" or "percentile".')
      }
    # TOFIX: If for percentile
    # if ((w_method == "percentile") | (x_method == "percentile")) {
    #     if (inherits(tryCatch(fit_data <- lavaan::lavInspect(fit, "data"),
    #                           error = function(e) e), "simpleError")) {
    #         stop('w_method and/or x_method = "percentile" but raw data cannot be retrieved from fit.')
    #       }
    #     if (!missing(w_sd_to_percentiles)) {
    #         w_percentiles <- c(stats::pnorm(-1 * abs(w_sd_to_percentiles)),
    #                            stats::pnorm(abs(w_sd_to_percentiles)))
    #       }
    #     if (!missing(x_sd_to_percentiles)) {
    #         x_percentiles <- c(stats::pnorm(-1 * abs(x_sd_to_percentiles)),
    #                            stats::pnorm(abs(x_sd_to_percentiles)))
    #       }
    #     if (w_percentiles[1] > w_percentiles[2]) {
    #         stop("The first perecentile in w_percentiles is higher than the second percentile.")
    #       }
    #     if (x_percentiles[1] > x_percentiles[2]) {
    #         stop("The first perecentile is x_percentiles is higher than the second percentile.")
    #       }
    #   }
    x_vlines_unit <- tolower(x_vlines_unit)
    # TOFIX: If for percentile
    # if ((x_vlines_unit == "percentile") & !missing(plot_x_vlines)) {
    #     if (inherits(tryCatch(fit_data <- lavaan::lavInspect(fit, "data"),
    #                           error = function(e) e), "simpleError")) {
    #         stop('x_vlines_unit = "percentile" but raw data cannot be retrieved from fit.')
    #       }
    #   }
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
    if (missing(x_label)) x_label <- x
    if (missing(w_label)) w_label <- w
    if (missing(y_label)) y_label <- y
    if (missing(title)) {
        if (standardized) {
            title <- paste0("The Moderation Effect of ", w_label,
                            " on ", x_label, "'s effect on ", y_label,
                            " (Standardized)")
          } else {
            title <- paste0("The Moderation Effect of ", w_label,
                            " on ", x_label, "'s effect on ", y_label)
          }
      }
    # if (missing(xw)) {
    #     all_prods <- find_all_products(lavaan::lavInspect(fit, "data"))
    #     tmp <- sapply(all_prods, function(a) {
    #                       if (length(a) != 2) {
    #                           return(FALSE)
    #                         } else {
    #                           if (all(c(x, w) %in% a)) {
    #                               return(TRUE)
    #                             } else {
    #                               return(FALSE)
    #                             }
    #                         }
    #                     })
    #     if (all(!tmp)) {
    #         stop("xw was not supplied but the product term could be found.")
    #       }
    #     if (sum(tmp) != 1) {
    #         stop("xw was not supplied but more than one possible product term was found.")
    #       }
    #     xw <- names(tmp[tmp])
    #   } else {
    #     xw0 <- deparse(substitute(xw))
    #     if (inherits(tryCatch(xw00 <- as.character(xw), error = function(e) e),
    #                 "simpleError")) {
    #         xw <- xw0
    #       } else {
    #         xw <- xw00
    #       }
    #   }
    xw1 <- paste(x, w, sep = ":")
    xw2 <- paste(w, x, sep = ":")
    coef_names <- names(stats::coef(output))
    tmp <- c(xw1, xw2) %in% coef_names
    if (any(tmp)) {
        xw <- c(xw1, xw2)[tmp]
      } else {
        stop("The interaction term was not found in output.")
      }
    coef0 <- stats::coef(output)
    mf0 <- stats::model.frame(output)
    x_sd_raw <- sd(mf0[, x])
    w_sd_raw <- sd(mf0[, w])
    y_sd_raw <- sd(mf0[, y])
    x_mean_raw <- mean(mf0[, x])
    w_mean_raw <- mean(mf0[, w])
    bx_raw <- coef0[x]
    bw_raw <- coef0[w]
    bxw_raw <- coef0[xw]
    # Standardization should be done before calling plotmod
    # if (standardized) {
    #     std_t <- lavaan::standardizedSolution(fit)
    #     x_sd <- 1
    #     w_sd <- 1
    #     x_mean <- 0
    #     w_mean <- 0
    #     bx <- (bx_raw + bxw_raw * w_mean_raw) * x_sd_raw / y_sd_raw
    #     bw <- (bw_raw + bxw_raw * x_mean_raw) * w_sd_raw / y_sd_raw
    #     bxw <- par_t[par_t$lhs == y & par_t$rhs == xw, "est"] *
    #                   x_sd_raw * w_sd_raw / y_sd_raw
    #   } else {
    x_sd <- x_sd_raw
    w_sd <- w_sd_raw
    x_mean <- x_mean_raw
    w_mean <- w_mean_raw
    bx <- bx_raw
    bw <- bw_raw
    bxw <- bxw_raw
    #   }
    if (x_method == "sd") {
        x_lo <- x_mean - x_from_mean_in_sd * x_sd
        x_hi <- x_mean + x_from_mean_in_sd * x_sd
      } else {
        # TOFIX: If for percentile
        x_percs <- stats::quantile(mf0[, x], x_percentiles, na.rm = TRUE)
        x_lo <- x_percs[1]
        x_hi <- x_percs[2]
        # Standardization should be done before calling plotmod
        # if (standardized) {
        #     x_lo <- (x_lo - x_mean_raw) / x_sd_raw
        #     x_hi <- (x_hi - x_mean_raw) / x_sd_raw
        #   }
      }
    if (w_method == "sd") {
        w_lo <- w_mean - w_from_mean_in_sd * w_sd
        w_hi <- w_mean + w_from_mean_in_sd * w_sd
      } else {
        # TOFIX: If for percentile
        w_percs <- stats::quantile(fit_data[, w], w_percentiles, na.rm = TRUE)
        w_lo <- w_percs[1]
        w_hi <- w_percs[2]
        # # Standardization should be done before calling plotmod
        # if (standardized) {
        #     w_lo <- (w_lo - w_mean_raw) / w_sd_raw
        #     w_hi <- (w_hi - w_mean_raw) / w_sd_raw
        #   }
      }
    mf1 <- mf0
    tmpfct <- function(x, type = "mean") {
        if (is.numeric(x)) {
            if (type == "mean") {
                return(mean(x))
              }
          }
        if (is.factor(x)) {
            return(as.ordered(x)[1])
          }
        if (is.character(x)) {
            return(as.ordered(as.factor(x)[1]))
          }
        return(mean(as.numeric(x)))
      }
    mf2 <- data.frame(lapply(mf1, tmpfct))
    mf2 <- rbind(mf2, mf2, mf2, mf2)
    mf2 <- data.frame(x_level = c("Low", "Low", "High", "High"),
                      w_level = c("Low", "High", "Low", "High"),
                      mf2)
    mf2[, x] <- c(x_lo, x_lo, x_hi, x_hi)
    mf2[, w] <- c(w_lo, w_hi, w_lo, w_hi)
    mf2$predicted <- stats::predict(output, mf2)

    p <- ggplot2::ggplot() +
          ggplot2::geom_point(ggplot2::aes_string(x = x,
                                                  y = "predicted",
                                                  colour = "w_level"),
                              data = mf2) +
          ggplot2::geom_segment(ggplot2::aes(x = mf2[mf2$x_level == "Low", x],
                                             xend = mf2[mf2$x_level == "High", x],
                                             y = mf2[mf2$x_level == "Low", "predicted"],
                                             yend = mf2[mf2$x_level == "High", "predicted"],
                                             colour = mf2[mf2$x_level == "Low", "w_level"]))
    dat_plot <- data.frame(w = c("Low", "High"),
                           a = c(a_shift + bw * w_lo,
                                 a_shift + bw * w_hi),
                           b = c(bx + bxw * w_lo,
                                 bx + bxw * w_hi),
                           stringsAsFactors = FALSE)
    y_x_lo_w_lo <- a_shift + bx * x_lo + bw * w_lo + bxw * x_lo * w_lo
    y_x_lo_w_hi <- a_shift + bx * x_lo + bw * w_hi + bxw * x_lo * w_hi
    y_x_hi_w_lo <- a_shift + bx * x_hi + bw * w_lo + bxw * x_hi * w_lo
    y_x_hi_w_hi <- a_shift + bx * x_hi + bw * w_hi + bxw * x_hi * w_hi
    y_min <- min(y_x_lo_w_lo, y_x_lo_w_hi, y_x_hi_w_lo, y_x_hi_w_hi)
    y_max <- max(y_x_lo_w_lo, y_x_lo_w_hi, y_x_hi_w_lo, y_x_hi_w_hi)
    y_range <- y_max - y_min
    x_range <- x_hi - x_lo
    b_format <- paste0("%.", digits, "f")
    subtxt <- paste0(w_label, " low: ", x_label, " effect = ",
                     sprintf(b_format,
                             dat_plot[dat_plot$w == "Low", "b"]),
                     "; ",
                     w_label, " high: ", x_label, " effect = ",
                     sprintf(b_format,
                             dat_plot[dat_plot$w == "High", "b"])
                     )
    # TOFIX: If for percentile
    if (w_method == "percentile") {
        cap_txt <- paste0(w_label, " levels: ",
                          "Low: ", 100 * w_percentiles[1],
                          "th percentile; Hi: ",
                          100*w_percentiles[2], "th percentile")
      }
    if (w_method == "sd") {
        cap_txt <- paste0(w_label, " levels: ",
                          "Low: ", w_from_mean_in_sd,
                          "SD below mean; Hi: ",
                          w_from_mean_in_sd, " SD above mean")
      }
    # out <- ggplot2::ggplot() +
    #   ggplot2::scale_x_continuous(name = x_label,
    #                               limits = c(x_lo - expansion * x_range,
    #                                          x_hi + expansion * x_range)) +
    #   ggplot2::scale_y_continuous(name = y_label,
    #                               limits = c(y_min - expansion * y_range,
    #                                          y_max + expansion * y_range)) +
    #   ggplot2::scale_linetype(name = w_label) +
    #   ggplot2::geom_abline(data = dat_plot,
    #               mapping = ggplot2::aes(slope = b,
    #                                      intercept = a,
    #                                      linetype = factor(w)),
    #                                      size = 1) +
    out <- p +
      ggplot2::labs(title = title,
                    subtitle = subtxt,
                    caption = cap_txt) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
    # TOFIX: plot_x_vlines
    if (!missing(plot_x_vlines)) {
        if (x_vlines_unit == "sd") {
            x_vline_levels <- plot_x_vlines * x_sd + x_mean
          }
        if (x_vlines_unit == "percentile") {
            x_vline_levels <- stats::quantile(fit_data[, x], plot_x_vlines,
                                                na.rm = TRUE)
            if (standardized) {
                x_vline_levels <- (x_vline_levels - x_mean_raw) / x_sd_raw
              }
          }
        for (i in x_vline_levels) {
            out <- out + ggplot2::geom_vline(
                          xintercept = i,
                          linetype = "dotted",
                          size = .5)
            out <- out + ggplot2::annotate(
                          "text",
                          x = i,
                          y = y_min - expansion * y_range +
                                .1 * (y_range * 2 * expansion),
                          label = paste0(x_label, "=",
                                    sprintf(b_format, i)),
                          size = 4)
          }
      }
    out
  }
