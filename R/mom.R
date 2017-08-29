#' Method of moments (MoM)
#'
#' Mean, variance, skewness, and kurtosis of a vector (distribution)
#'
#' @param x  vector of values to be summarized
#'
#' @param na.rm  logical, defaults to TRUE
#'
#' @param digits numeric, number of digits for rounding
#'
#' @param type default=1, an integer between 1 and 3 selecting one of
#'   the algorithms for computing skewness detailed in
#'   \code{\link[e1071]{kurtosis}}
#'
#' @return
#' List with 4 moments: \itemize{
#'   \item mean = mean
#'   \item var  = variance
#'   \item skew = skewness
#'   \item kurt = kurtosis
#'  }
#'
#' @details
#' Method-of-moments summaries for first 4 moments. \itemize{
#'   \item NEG kurtosis = flat distribution (platykurtic)
#'   \item POS kurtosis = peaked distribution (leptokurtic)
#'   \item ZERO kurtosis ~ the normal distribution (mesokurtic)
#'   \item NEG skewness indicates mean < median (left-skewed)
#'   \item POS skewness indicates mean > median (right-skewed)
#'  }
#'
#' @examples
#' # require(e1071)
#' x <- rnorm(n, 0.1)
#' mom(x)
#'
#' @seealso
#' \code{\link[e1071]{kurtosis}},
#' \code{\link[e1071]{skewness}}
#'
#' @export
`mom` <- function(x, na.rm=TRUE, digits=2, type=1, ...) {
     out <- data.frame(mean = mean(x, na.rm=na.rm),
                       var  = var(x, na.rm=na.rm),
                       skew = skewness(x, na.rm=na.rm, type=type),
                       kurt = kurtosis(x, na.rm=na.rm, type=type))
     out <- round(out, digits=digits)
     mode(out) <- "numeric"
     return(out)
}