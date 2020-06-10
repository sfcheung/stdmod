#'@title Compute the standardized moderation effect and betas for other predictors given the \code{lm} output
#'
#'@description Compute the standardized moderation effect and betas for other predictors given the \code{lm} output
#'
#'@details Compute the standardized moderation effect and betas for other predictors given the \code{lm} output
#'
#'@return
#' The updated \code{lm} output, with the class \code{stdmod} added. It will be 
#' treated as a usual \code{lm} object by most functions. It has these additional elements:
#'
#'  - \code{scaled}: The terms scaled.
#'
#'  - \code{centered}: The terms centered.
#'
#'  - \code{scaled_by}: The scaling factors. The value is 1 for terms not scaled.
#'
#'  - \code{centered}: The values used for centering. The value is 0 for terms not centered.
#'
#' 
#'@param lm_out The output from \code{lm}.
#'@param to_scale  Specify the terms to be rescaled by standard deviation, 
#'       using a formula as in \code{lm}. For example, if the terms to be scale
#'       is x1 and x3, use \code{~ x1 + x3}. No need to specify the interaction term.
#'       Specify only the original variables. If NULL, then all terms
#'       will be rescaled by standard deviation. Variables that are not numeric will
#'       will be ignored. Default is NULL.
#'@param to_center Specify the terms to be mean-centered, using a formula 
#'        as in \code{lm}. For example, if the terms to be scale
#'        is x1 and x3, use \code{~ x1 + x3}. No need to specify the interaction term.
#'        Specify only the original variables. If NULL, then all terms 
#'        will be centered. Default is NULL.
#'
#'@examples
#' # "To be prepared"
#' @export

stdmod_selected <- function(lm_out,
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

    # Collect the terms
    
    if (is.null(to_scale)) {
        scale_terms <- colnames(lm_out$model)
      } else {
        scale_f <- stats::as.formula(to_scale)
        scale_terms  <- attr(stats::terms(scale_f),  "term.labels")
      }
    
    if (is.null(to_center)) {
        center_terms <- colnames(lm_out$model)
      } else {
        center_f <- stats::as.formula(to_center)
        center_terms <- attr(stats::terms(center_f), "term.labels")
      }
    
    # Get the data frame

    dat <- lm_out$model
    k <- ncol(dat)

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
    var_sds <- suppressWarnings(sapply(dat, stats::sd))
    
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
    
    class(lm_out_mod) <- c("stdmod", class(lm_out))
    
    lm_out_mod$scaled   <- scale_terms
    lm_out_mod$centered <- center_terms
    lm_out_mod$scaled_by   <- var_b
    lm_out_mod$centered_by <- var_a    
    
    lm_out_mod
    
  }