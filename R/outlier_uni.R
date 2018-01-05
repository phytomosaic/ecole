#' Univariate outliers
#'
#' Find extreme values in a vector
#'
#' @param x
#' vector of numeric values to query
#'
#' @param mult
#' IQR threshold for 'outliers'; default is 1.5 times the IQR
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' List of with 4 elements: threshold values (\code{lwrlim},
#'     \code{uprlim}) and indices of values beyond those
#'     thresholds (\code{lower}, \code{upper})
#'
#' @details
#' Given a vector, finds indices of values exceeding an IQR threshold.
#'
#' @examples
#' set.seed(19)
#' x <- data.frame(x=seq(1:10),y=c(rnorm(6,1),rnorm(2,-9),rnorm(2,9)))
#' (outs <- outlier_uni(x$yy))
#' plot(x)
#' points(x[outs$lower,], col=2, pch=19)
#' points(x[outs$upper,], col=4, pch=19)
#'
#' @family outlier functions
#' @seealso \code{\link{outlier_multi}} for multivariate analog.
#'
#' @references
#' davidrroberts.wordpress.com/2015/08/26/r-code-outlier-function/
#'
#' @export
`outlier_uni` <- function(x, mult=1.5, ...){
     lo <- as.numeric(quantile(x)[2] - IQR(x)*mult)
     hi <- as.numeric(IQR(x)*mult + quantile(x)[4])
     list(lwrlim=lo, uprlim=hi, lower=which(x<lo), upper=which(x>hi))
}
### end ###
