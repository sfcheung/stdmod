x_for_w <- function(w_i, mf, x, w, w_numeric, ...) {
    if (w_numeric) {
        return(x_for_w_numeric(w_i = w_i,
                               mf = mf,
                               x = x,
                               w = w,
                               ...))
      } else {
        return(x_for_w_categorical(w_i = w_i,
                               mf = mf,
                               x = x,
                               w = w,
                               ...))
      }
  }

x_for_w_numeric <- function(w_i, mf, x, w, band = .16) {
    w0 <- mf[, w]
    w_ecdf <- stats::ecdf(w0)
    w_i_p <- w_ecdf(w_i)
    w_i_r <- c(w_i_p - band, w_i_p + band)
    w_i_r[1] <- max(w_i_r[1], 0)
    w_i_r[2] <- min(w_i_r[2], 1)
    w_i_b <- stats::quantile(w0, w_i_r, na.rm = TRUE, names = FALSE)
    x1 <- mf[(w0 >= w_i_b[1]) & (w0 <= w_i_b[2]), x]
    x1
  }

x_for_w_categorical <- function(w_i, mf, x, w) {
    w0 <- mf[, w]
    x1 <- mf[w0 == w_i, x]
    x1
  }