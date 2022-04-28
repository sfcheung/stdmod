#' @title Moderation Effect Plot
#'
#' @description Plot the moderation effect in a regression model
#'
#' @details This function generate a basic [ggplot2] graph
#'          typically found psychology manuscripts. It tries to
#'          check whether one or more variables are standardized, and
#'          report this in the plot if required.
#'
#' @return
#'  A [ggplot2] graph. Plotted if not assigned to a name. It can
#'  be further modified like a usual [ggplot2] graph.
#'
#' @param output An object of the class `lm`, such as the output
#'                  of [stats::lm()], [std_selected()], or
#'                  [std_selected_boot()].
#'
#' @param x The name of the focal variable (x-axis) in the output. It
#'          can be the name of the variable, with or without quotes.
#'          Currently only numeric variables are supported.
#' @param w The name of the moderator in the output. It
#'          can be the name of the variable, with or without quotes.
#'          Currently only numeric variables are supported.
#' @param x_label The label for the X-axis. Default is the value of `x`.
#' @param w_label The label for the legend for the lines.
#'                Default is the value of`w`.
#' @param y_label The label for the Y-axis. Default is the
#'                name of the response variable in the model.
#' @param title The title of the graph. If not supplied, will be
#'               generated from the variable
#'               names or labels (in `x_label`, `y_label`,
#'               and `w_label`). If `""`, no title will be printed.
#'               This can be used when the plot is for manuscript
#'               submission and figures are requried to have no
#'               titles.
#' @param expansion How much tha lower and upper limits of the axis
#'                  will be adjusted. Default is .1
#' @param digits Number of decimal digits to print. Default is 3.
#' @param x_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the focal variable.
#'                          Default is 1.
#' @param w_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the moderator. Default is 1.
#' @param w_method How to define "high" and "low" for the moderator levels.
#'                  Default is in terms of the
#'                  standard deviation of the moderator, `"sd".
#'                  If equal to
#'                  `"percentile"`, then percentiles of the moderator in
#'                  the
#'                  dataset are used.
#' @param w_percentiles If `w_method` is `"percentile"`, then this
#'                      argument
#'                      specifies the two percentiles to be used,
#'                      divided by 100.
#'                        It must be a
#'                      vector of two numbers. The default is
#'                      `c(.16, .84)`,
#'                      the 16th and 84th percentiles,
#'                      which corresponds approximately
#'                      to one SD below and above mean for a
#'                      normal distribution, respectively.
#' @param x_method How to define "high" and "low" for the focal
#'                  variable levels.
#'                  Default is in terms of the
#'                  standard deviation of the focal variable, `"sd"`.
#'                  If equal to
#'                  `"percentile"`, then the percentiles of the
#'                  focal variable in the
#'                  dataset is used.
#' @param x_percentiles If `x_method` is `"percentile"`, then this
#'                      argument
#'                      specifies the two percentiles to be used,
#'                      divided by 100.
#'                        It must be a
#'                      vector of two numbers. The default is
#'                      `c(.16, .84)`,
#'                      the 16th and 84th percentiles,
#'                      which corresponds approximately
#'                      to one SD below and above mean for a
#'                      normal distribution, respectively.
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
#'                            respectively.
#' @param x_sd_to_percentiles If `x_method` is `"percentile"` and this 
#'                            argument is
#'                            set to a number, this number will be used
#'                            to
#'                            to determine the percentiles to be used. The
#'                            lower percentile is the percentile in a
#'                            normal
#'                            distribution
#'                            that is `x_sd_to_percentiles` SD below the mean.
#'                            The upper percentile is the percentile in a normal
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
#' @param x_vlines_unit If equal to `"sd"`, then the values of `plot_x_vlines`
#'                       will be interpreted as the deviation from the mean.
#'                       For example, 1 is 1 SD above mean, and -1 is 1 SD
#'                       below mean. If equal to "percentile", then the numbers,
#'                       multiplied by 100, are the percentiles. For example,
#'                       .25 is the 25th percentile, and .75 is the 75th
#'                       percentile.
#' @param note_standardized If `TRUE`, will check whether a variable has SD
#'                          nearly equal to one. If yes, will report this in the
#'                          plot.
#'                          Default is `TRUE`.
#' @param a_shift Default is 0. Ignored for now.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Do a moderated regression by lm
#' lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, sleep_emo_con)
#' summary(lm_out)
#'
#' # Standardize all variables except for categorical variables
#' lm_std <- std_selected(lm_out,
#'                        to_scale = ~ .,
#'                        to_center = ~ .)
#' summary(lm_std)z
#'
#' @export

plotmod <- function(output, y, x, w, xw,
                            x_label,
                            w_label,
                            y_label,
                            title,
                            expansion = .1,
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
                            x_vlines_unit = "sd",
                            note_standardized = TRUE,
                            a_shift = 0
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
      }
    if (w_method == "sd") {
        w_lo <- w_mean - w_from_mean_in_sd * w_sd
        w_hi <- w_mean + w_from_mean_in_sd * w_sd
      } else {
        # TOFIX: If for percentile
        w_percs <- stats::quantile(fit_data[, w], w_percentiles, na.rm = TRUE)
        w_lo <- w_percs[1]
        w_hi <- w_percs[2]
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

    if (missing(x_label)) x_label <- x
    if (missing(w_label)) w_label <- w
    if (missing(y_label)) y_label <- y
    if (note_standardized) {
        if (isTRUE(all.equal(sd(mf0[, x]), 1))) {
            x_label <- paste0(x_label, " (Standardized)")
          }
        if (isTRUE(all.equal(sd(mf0[, w]), 1))) {
            w_label <- paste0(w_label, " (Standardized)")
          }
        if (isTRUE(all.equal(sd(mf0[, y]), 1))) {
            y_label <- paste0(y_label, " (Standardized)")
          }
      }
    if (missing(title)) {
        title <- "Moderation Effect"
      }

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
                     "\n",
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
    out <- p +
      ggplot2::labs(title = title,
                    subtitle = subtxt,
                    caption = cap_txt) +
      # ggplot2::theme(axis.text.y = ggplot2::element_blank()) +
      ggplot2::theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5)) +
      ggplot2::xlab(x_label) +
      ggplot2::ylab(y_label) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = w_label))
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
