#' @title Summary statistics utilities
#'
#' @description
#' Vectorized summary statistics, including geometric mean, harmonic
#' mean, sample standard error (SE), coefficient of variation (CV),
#' and root mean square error (RMSE).
#'
#' @param x vector of values to evaluate
#'
#' @param xpred vector of 'predicted' values to compare against x
#'
#' @param na.rm logical indicating whether NA values in x should be
#' removed before proceeding
#'
#' @param zero.rm logical indicating whether zeros in x should be
#' removed before calculating harmonic or geometric means
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' Numeric value
#'
#' @details
#' Results of \code{geom_mean} and \code{harm_mean} are always 0 by
#' definition, unless \code{zero.rm=TRUE}.
#'
#' Like \code{\link[base]{sd}}, \code{sem} uses \emph{n-1} in
#' denominator to correct for small-sample bias.
#'
#' \code{rmse} is one way to assess prediction accuracy.
#'
#' These functions return NA when NAs present and \code{na.rm=TRUE}.
#'
#' @examples
#' # test data
#' xx <- c(-1, 0, 1, 4, 77, NA)
#'
#' # harmonic mean
#' harm_mean(xx, na.rm=T, zero.rm=F)  # 0 by definition
#' harm_mean(xx, na.rm=T, zero.rm=T)  # 15.20988
#'
#' # geometric mean
#' geom_mean(xx, na.rm=T, zero.rm=F)  # should fail for negative values
#' xx <- xx[-1]                       # remove negative values
#' geom_mean(xx, na.rm=T, zero.rm=F)  # 0 by definition
#' geom_mean(xx, na.rm=T, zero.rm=T)  # 6.753313
#'
#' # standard error of the mean
#' sem(xx)                            # 21.76899
#'
#' # coefficient of variation
#' cv(xx)                             # 183.9268
#'
#' # root mean squared error
#' set.seed(23)
#' rmse(xx, xx+rnorm(length(xx)), na.rm=T) # 1.033994
#'
#' @seealso \code{\link[base]{sd}}
#'
#' @export
#' @rdname utils_stats
`geom_mean` <- function(x, na.rm=TRUE, zero.rm=FALSE, ...){
     if(any(x < 0, na.rm = TRUE)) {
          stop('geometric mean not defined for negative values')
     }
     if(zero.rm) {
          x[x==0] <- NA
     }
     exp(mean(log(x), na.rm=na.rm))
}
#' @export
#' @rdname utils_stats
`harm_mean` <- function(x, na.rm=TRUE, zero.rm=FALSE, ...) {
     if(zero.rm) {
          x[x==0] <- NA
     }
     1/mean(1/x,na.rm=na.rm)
}
#' @export
#' @rdname utils_stats
`sem` <- function(x, na.rm=TRUE, ...) {
     sd(x, na.rm=na.rm) / sqrt(length(na.omit(x))-1)
}
#' @export
#' @rdname utils_stats
`cv` <- function(x, na.rm=TRUE, ...) {
     sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm) * 100
}
#' @export
#' @rdname utils_stats
`rmse` <- function(x, xpred, na.rm=TRUE, ...){
     sqrt(mean((x-xpred)^2, na.rm=na.rm))
}