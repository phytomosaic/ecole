#' @title Summary statistics utilities
#'
#' @description
#' Vectorized summary statistics, including geometric mean, harmonic
#' mean, sample standard error (SE), coefficient of variation (CV),
#' root mean square error (RMSE), mean absolute error (MAE),
#' sensitivity, and robust z-scores.
#'
#' @param x vector of values to evaluate
#'
#' @param y vector of 'predicted' values to compare against \code{x}
#'
#' @param na.rm logical, should NA values in \code{x} be removed
#' before calculating?
#'
#' @param zero.rm logical, should zeros in \code{x} be removed before
#' calculating harmonic or geometric means?
#'
#' @param stdz logical, standardize output by range of \code{x}?
#'
#' @param robust logical, should robust z-scores be calculated?
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' Numeric values.
#'
#' @details
#' For vectors including at least one zero, results of
#' \code{geom_mean} and \code{harm_mean} are always 0 by
#' definition, unless \code{zero.rm=TRUE}.
#'
#' Like \code{\link[stats]{sd}}, \code{sem} uses \emph{n-1} in
#' denominator to correct for small-sample bias.
#'
#' \code{rmse} is one way to assess prediction accuracy.
#'
#' \code{mae} gives a measure of sensitivity when \code{stdz=TRUE}.
#'
#' \code{zcsr} gives robust z-scores based on median (not mean) and
#' median absolute deviation (not standard deviation).
#'
#' These functions return NA when NAs present and \code{na.rm=TRUE}.
#'
#' @examples
#' # test data
#' xx <- c(-1, 0, 1, 4, 77, NA)
#'
#' # harmonic mean
#' harm_mean(xx, na.rm=TRUE, zero.rm=FALSE)     # 0 by definition
#' harm_mean(xx, na.rm=TRUE, zero.rm=TRUE)      # 15.20988
#'
#' # geometric mean
#' ### NOT RUN:
#' # geom_mean(xx, na.rm=TRUE, zero.rm=FALSE))  # fails for neg vals
#' ### END NOT RUN
#' xx <- xx[-1]                                 # remove negative values
#' geom_mean(xx, na.rm=TRUE, zero.rm=FALSE)     # 0 by definition
#' geom_mean(xx, na.rm=TRUE, zero.rm=TRUE)      # 6.753313
#'
#' # standard error of the mean
#' sem(xx)                       # 21.76899
#'
#' # coefficient of variation
#' cv(xx)                        # 183.9268
#'
#' # root mean squared error
#' set.seed(23)
#' xx <- c(-1, 0, 1, 4, 77, NA)
#' yy <- xx+rnorm(length(xx), 10)
#' rmse(xx, yy)                  # 10.71919
#' rmse(yy, xx)                  # same, order invariant
#'
#' # mean absolute error
#' mae(xx, yy, stdz=FALSE)       # 10.69236
#'
#' # range-standardized mean absolute error (aka sensitivity)
#' mae(xx, yy, stdz=TRUE)        # 0.1370815
#' mae(yy, xx, stdz=TRUE)        # 0.135684 -- order matters!
#'
#' # robust z-scores not so influenced by extreme values
#' x <- c(-99, -9, 0, 9, 99)
#' plot(zscr(x, robust=FALSE), zscr(x, robust=TRUE), asp=1)
#' abline(0,1)
#'
#' @seealso \code{\link[stats]{sd}}
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
`rmse` <- function(x, y, na.rm=TRUE, ...){
     sqrt(mean((x-y)^2, na.rm=na.rm))
}
#' @export
#' @rdname utils_stats
`mae` <- function(x, y, stdz=FALSE, na.rm=TRUE, ...){
     if(stdz) denom <- diff(range(x, na.rm = na.rm)) else denom <- 1
     mean(abs(x - y), na.rm = na.rm) / denom
}
#' @export
#' @rdname utils_stats
`zscr` <- function(x, robust = TRUE, na.rm = TRUE, ...) {
     if (robust) {
          center <- median(x, na.rm = na.rm)
          spread <- mad(x, center=center, na.rm = na.rm)
     } else {
          center <- mean(x, na.rm = na.rm)
          spread <- sd(x, na.rm = na.rm)
     }
     (x - center) / spread
}
