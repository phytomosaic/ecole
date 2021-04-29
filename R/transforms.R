#' @title Transforms
#'
#' @description Transformations appropriate for ecological data.
#'
#' @param x  vector or array of values to transform.
#'
#' @param from,to standardizes from low to high values; defaults from
#'      0 to 1.
#'
#' @param na.rm logical. Should missing values (including NaN) be
#'      removed?
#'
#' @param sigma numeric, curvature scale factor for pseudo-log transform.
#'
#' @param ... further arguments passed to other methods.
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
#' Log transform for non-negative values, preserves original order of magnitudes
#' while properly handling values = 0 and << 1.  Follows pg. 69 of McCune &
#' Grace (2002).
#'
#' \code{pseudo_log} = Pseudo-logarithmic transform:\cr
#' Log-like transformation for positive and negative values. Useful for color
#' scales.  Change `sigma` to vary curvature/linearity around zero.
#'
#' \code{standardize} = Range standardize:\cr
#' Standardizes data to 0-1 scale based on min/max values.
#'
#' @examples
#' set.seed(21)
#' x  <- (rbeta(99, 0.5, 1) - 0.15) * 100
#' x1 <- asin_sqrt(x)    # forces 0-1 scale
#' x2 <- genlogtrans(x)  # only for non-negative values, negative become NaN
#' x3 <- pseudo_log(x)   # handles positive and negative values
#' x4 <- standardize(x)  # forces 0-1 scale
#' par(mfrow=c(2,2))
#' plot(x, x1)
#' plot(x, x2)
#' plot(x, x3)
#' plot(x, x4)
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
        wasVector <- is.vector(x)       # takes and returns a vector
        x <- as.matrix(x)               # convert to matrix
        if( any(x > 1) | any(x < 0) ) {
                cat('Range beyond 0-1, normalizing now')
                x <- (x-min(x, ...))/(max(x, ...)-min(x, ...)) # standardize
        }
        out <- (2/pi) * asin(sqrt(x))   # do the transform
        if (wasVector)                  # return a vector
                out <- as.vector(out)
        return(out)
}
#' @export
#' @rdname transforms
`genlogtrans` <- function(x, ...) {
        wasVector <- is.vector(x)       # takes and returns a vector
        x <- as.matrix(x)               # convert to matrix
        minx <- min(x[which(x>0)], ...) # min of nonzero numbers
        const <- as.integer(log10(minx))# log10 of min is the constant
        d <- 10^const                   # 10 to the power of the constant
        x <- log10(x + d) - const       # produces NaN for negative inputs
        if (wasVector)                  # return a vector
                x <- as.vector(x)
        x
}
#' @export
#' @rdname transforms
`pseudo_log` <- function (x, sigma = 1) {
        wasVector <- is.vector(x)         # takes and returns a vector
        x <- as.matrix(x)                 # convert to matrix
        x <- asinh(x / (sigma * 2)) / log(10)  # transformation
        # 2 * sigma * sinh(x * log(10))   # back-transformation antidote
        if (wasVector)                    # return a vector
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
