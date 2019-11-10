#' @title Transforms
#'
#' @description Transformations appropriate for ecological data.
#'
#' @param x  vector or array of values to transform
#'
#' @param from,to standardizes from low to high values; defaults from
#'      0 to 1.
#'
#' @param na.rm logical. Should missing values (including NaN) be
#'      removed?
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' Values on the modified scale.
#'
#' @details
#' \code{asin_sqrt} = Arc-sine square root transform:\cr
#' Appropriate for vegetation cover and other proportion data.  Forces
#' data to 0-1 scale if any original values were outside this range.
#' Follows pg. 69 of McCune & Grace (2002).
#'
#' \code{genlogtrans} = Generalized log-transform:\cr
#' Log transform that properly handles zeros and very small values.
#' Preserves original order of magnitudes while accounting for values
#' = 0 and << 1. Follows pg. 69 of McCune & Grace (2002).
#'
#' \code{standardize} = Range standardize:\cr
#' Standardizes data to 0-1 scale based on min/max values.
#'
#' @examples
#' set.seed(21)
#' x  <- rnorm(99, 50, 20)
#' x1 <- asin_sqrt(x)
#' x2 <- genlogtrans(x)
#' x3 <- standardize(x)
#' par(mfrow=c(1,3))
#' plot(x, x1)
#' plot(x, x2)
#' plot(x, x3)
#' par(mfrow=c(1,1))
#'
#' @family transformation functions
#'
#' @references
#' McCune, B., and J. B. Grace. 2002. Analysis of Ecological
#'     Communities. MjM Software, Gleneden Beach, Oregon, USA. 304 pp.
#'
#' @export
#' @rdname transforms
`asin_sqrt` <- function(x, ...) {
     wasVector <- is.vector(x)     # takes and returns a vector
     x <- as.matrix(x)             # convert to matrix
     if( any(x > 1) | any(x < 0) ) {
          cat('Range beyond 0-1, normalizing now')
          x <- (x-min(x, ...))/(max(x, ...)-min(x, ...)) # standardize
     }
     out <- (2/pi) * asin(sqrt(x)) # do the transform
     if (wasVector)                # return a vector
          out <- as.vector(out)
     return(out)
}
#' @export
#' @rdname transforms
`genlogtrans` <- function(x, ...) {
     wasVector <- is.vector(x)     # takes and returns a vector
     x <- as.matrix(x)               # convert to matrix
     minx <- min(x[which(x>0)], ...) # min of nonzero numbers
     const <- as.integer(log10(minx))# log10 of min is the constant
     d <- 10^const                   # 10 to the power of the constant
     x <- log10(x + d) - const       # produces NaNs
     if (wasVector)                # return a vector
          x <- as.vector(x)
     x
}
#' @export
#' @rdname transforms
`standardize` <- function (x, from = 0, to = 1, na.rm = TRUE, ...) {
        r <- range(x, na.rm = na.rm)
        x <- (x - r[1]) / diff(r)
        x <- x * diff(c(from, to)) + from
        x
}
