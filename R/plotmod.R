#' @title Moderation Effect Plot
#'
#' @description Plot the moderation effect in a regression model
#'
#' @details This function generate a basic [ggplot2] graph
#'          typically found psychology manuscripts. It tries to
#'          check whether one or more variables are standardized, and
#'          report this in the plot if required.
#'
#' This function only has features for typical plots of moderation effects.
#' It is not intended to be a flexible tool for a fine control on the plots.
#'
#' @return
#'  A [ggplot2] graph. Plotted if not assigned to a name. It can
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
#'               submission and figures are required to have no
#'               titles.
#' @param digits Number of decimal digits to print. Default is 3.
#' @param x_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the focal variable.
#'                          Default is 1.
#' @param w_from_mean_in_sd How many SD from mean is used to define
#'                          "low" and
#'                          "high" for the moderator. Default is 1.
#'                          Ignored if `w` is categorical.
#' @param w_method How to define "high" and "low" for the moderator levels.
#'                  Default is in terms of the
#'                  standard deviation of the moderator, `"sd".
#'                  If equal to
#'                  `"percentile"`, then percentiles of the moderator in
#'                  the
#'                  dataset are used.
#'                  Ignored if `w` is categorical.
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
#'                      Ignored if `w` is categorical.
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
#' @param note_standardized If `TRUE`, will check whether a variable has SD
#'                          nearly equal to one. If yes, will report this in the
#'                          plot.
#'                          Default is `TRUE`.
#' @param no_title If `TRUE`, title will be suppressed. Default is `FALSE`.
#' @param line_width The width of the lines as used in [ggplot2::geom_segment()].
#'                   Default is 1.
#' @param point_size The size of the points as used in [ggplot2::geom_point()].
#'                    Default is 5.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Do a moderated regression by lm
#' lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, sleep_emo_con)
#' summary(lm_out)
#' plotmod(lm_out,
#'         x = emotional_stability,
#'         w = conscientiousness,
#'         x_label = "Emotional Stability",
#'         w_label = "Conscientiousness",
#'         y_label = "Sleep Duration")
#'
#' # Standardize all variables except for categorical variables
#' lm_std <- std_selected(lm_out,
#'                        to_scale = ~ .,
#'                        to_center = ~ .)
#' summary(lm_std)
#' plotmod(lm_std,
#'         x = emotional_stability,
#'         w = conscientiousness,
#'         x_label = "Emotional Stability",
#'         w_label = "Conscientiousness",
#'         y_label = "Sleep Duration")
#'
#' @export

plotmod <- function(output, x, w,
                            x_label,
                            w_label,
                            y_label,
                            title,
                            digits = 3,
                            x_from_mean_in_sd = 1,
                            w_from_mean_in_sd = 1,
                            w_method = c("sd", "percentile"),
                            w_percentiles = c(.16, .84),
                            x_method = c("sd", "percentile"),
                            x_percentiles = c(.16, .84),
                            w_sd_to_percentiles,
                            x_sd_to_percentiles,
                            note_standardized = TRUE,
                            no_title = FALSE,
                            line_width = 1,
                            point_size = 5
                    ) {
    w_method <- match.arg(w_method)
    x_method <- match.arg(x_method)
    if (x_method == "percentile") {
        if (!missing(x_sd_to_percentiles)) {
            x_percentiles <- c(stats::pnorm(-1 * abs(x_sd_to_percentiles)),
                               stats::pnorm(abs(x_sd_to_percentiles)))
          }
        if (x_percentiles[1] > x_percentiles[2]) {
            stop("In x_percentiles, the first percentile must be lower than the second percentile.")
          }
      }
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
    coef0 <- stats::coef(output)
    mf0 <- stats::model.frame(output)
    x_numeric <- is.numeric(mf0[, x])
    w_numeric <- is.numeric(mf0[, w])
    if (!x_numeric) {
        stop("x variable must be a numeric variable.")
      }
    if (w_method == "percentile") {
        if (!missing(w_sd_to_percentiles)) {
            w_percentiles <- c(stats::pnorm(-1 * abs(w_sd_to_percentiles)),
                               stats::pnorm(abs(w_sd_to_percentiles)))
          }
        if (w_percentiles[1] > w_percentiles[2]) {
            stop("In w_percentiles, the first percentile must be lower than the second percentile.")
          }
      }
    x_sd_raw <- stats::sd(mf0[, x])
    x_mean_raw <- mean(mf0[, x])
    x_sd <- x_sd_raw
    x_mean <- x_mean_raw
    if (!w_numeric) {
        w_sd_raw <- NA
        w_mean_raw <- NA
        w_levels <- levels(as.factor(mf0[, w]))
        w_nlevels <- nlevels(as.factor(mf0[, w]))
      } else {
        w_sd_raw <- stats::sd(mf0[, w])
        w_mean_raw <- mean(mf0[, w])
        w_levels <- NA
        w_nlevels <- NA
      }
    w_sd <- w_sd_raw
    w_mean <- w_mean_raw
    if (x_method == "sd") {
        x_lo <- x_mean - x_from_mean_in_sd * x_sd
        x_hi <- x_mean + x_from_mean_in_sd * x_sd
      } else {
        x_percs <- stats::quantile(mf0[, x], x_percentiles, na.rm = TRUE)
        x_lo <- x_percs[1]
        x_hi <- x_percs[2]
      }
    if (w_numeric) {
        if (w_method == "sd") {
            w_lo <- w_mean - w_from_mean_in_sd * w_sd
            w_hi <- w_mean + w_from_mean_in_sd * w_sd
          } else {
            w_percs <- stats::quantile(mf0[, w], w_percentiles, na.rm = TRUE)
            w_lo <- w_percs[1]
            w_hi <- w_percs[2]
          }
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
    if (w_numeric) {
        mf2 <- rbind(mf2, mf2, mf2, mf2)
        mf2 <- data.frame(x_level = c("Low", "Low", "High", "High"),
                          w_level = c("Low", "High", "Low", "High"),
                          mf2)
        mf2[, x] <- c(x_lo, x_lo, x_hi, x_hi)
        mf2[, w] <- c(w_lo, w_hi, w_lo, w_hi)
      } else {
        mf2 <- do.call(rbind, replicate(2 * w_nlevels, mf2, simplify = FALSE))
        mf2 <- data.frame(x_level = rep(c("Low", "High"), each = w_nlevels),
                          w_level = rep(levels(as.factor(mf0[, w])), 2),
                          mf2)
        mf2[, x] <- rep(c(x_lo, x_hi), each = w_nlevels)
        mf2[, w] <- rep(w_levels, 2)
      }

    mf2$predicted <- stats::predict(output, mf2)

    if (missing(x_label)) x_label <- x
    if (missing(w_label)) w_label <- w
    if (missing(y_label)) y_label <- y
    if (note_standardized) {
        if (isTRUE(all.equal(stats::sd(mf0[, x]), 1))) {
            x_label <- paste0(x_label, " (Standardized)")
          }
        if (w_numeric) {
            if (isTRUE(all.equal(stats::sd(mf0[, w]), 1))) {
                w_label <- paste0(w_label, " (Standardized)")
              }
          }
        if (isTRUE(all.equal(stats::sd(mf0[, y]), 1))) {
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
                              data = mf2,
                              size = point_size) +
          ggplot2::geom_segment(ggplot2::aes(
                x = mf2[mf2$x_level == "Low", x],
                xend = mf2[mf2$x_level == "High", x],
                y = mf2[mf2$x_level == "Low", "predicted"],
                yend = mf2[mf2$x_level == "High", "predicted"],
                colour = mf2[mf2$x_level == "Low", "w_level"]
              ), size = line_width)
    find_b <- function(w_i) {
        mf_i <- mf2[mf2[, w] == w_i, ]
        b_i <- (mf_i[mf_i$x_level == "High", "predicted"] -
                mf_i[mf_i$x_level == "Low", "predicted"]) /
              (mf_i[mf_i$x_level == "High", x] -
                mf_i[mf_i$x_level == "Low", x])
        b_i
      }
    if (w_numeric) {
        b_all <- sapply(c(w_lo, w_hi), find_b)
      } else {
        b_all <- sapply(w_levels, find_b)
      }
    b_format <- paste0("%.", digits, "f")
    if (w_numeric) {
        subtxt <- paste0(w_label, " low: ", x_label, " effect = ",
                         sprintf(b_format, b_all[1]),
                         "\n",
                         w_label, " high: ", x_label, " effect = ",
                         sprintf(b_format, b_all[2])
                        )
      } else {
        subtxt <- paste0(w_levels,
                         ": ",
                         x_label,
                         " effect = ",
                         sprintf(b_format, b_all),
                         collapse = "\n")
      }
    if (w_numeric) {
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
      } else {
        cap_txt <- NULL
      }
    out <- p +
      ggplot2::labs(title = title,
                    subtitle = subtxt,
                    caption = cap_txt) +
      ggplot2::theme(legend.position = "top",
                     plot.caption = ggplot2::element_text(hjust = .5,
                                                          size = 9),
                     plot.title = ggplot2::element_text(hjust = .5),
                     plot.subtitle = ggplot2::element_text(hjust = .5,
                                                           size = 9)) +
      ggplot2::xlab(x_label) +
      ggplot2::ylab(y_label) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = w_label))
    if (no_title) {
        out <- out + ggplot2::labs(title = NULL)
      }
    out
  }
