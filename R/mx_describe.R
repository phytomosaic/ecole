#' @title Descriptive summary statistics
#'
#' @description Descriptive summary statistics for .
#'
#' @param x  vector of values to be summarized.
#'
#' @param na.rm  logical, defaults to TRUE.
#'
#' @param digits numeric, number of digits for rounding.
#'
#' @param type default=1, an integer between 1 and 3 selecting one of
#'   the algorithms for computing skewness detailed in
#'   \code{\link[e1071]{kurtosis}}.
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' For \code{describe}, a list with descriptive statistics: \itemize{
#'   \item mean = mean
#'   \item sd = standard deviation
#'   \item var  = variance
#'   \item sem = standard error of the mean
#'   \item cv = coefficient of variation
#'   \item n = count of real elements
#'   \item NAs = count of NA elements
#'   \item skw = skewness
#'   \item krt = kurtosis
#'   \item min = minimum value
#'   \item max = maximum value
#'   }\cr
#'
#' For \code{mx_describe}, a list of two items showing row and column
#'   summaries, each containing the above items.
#'
#' @details
#' Summary statistics applied to a vector or rows/columns of a matrix. For
#'   skewness and kurtosis: \itemize{
#'     \item NEG kurtosis = flat distribution (platykurtic)
#'     \item POS kurtosis = peaked distribution (leptokurtic)
#'     \item ZERO kurtosis ~ the normal distribution (mesokurtic)
#'     \item NEG skewness indicates mean < median (left-skewed)
#'     \item POS skewness indicates mean > median (right-skewed)
#'     }
#'
#' @examples
#' x <- c(rnorm(99, 0.1), NA)
#' describe(x)
#' y <- matrix(rnorm(1000), 20, 50)
#' mx_describe(y)
#'
#' @seealso
#' \code{\link[e1071]{kurtosis}},
#' \code{\link[e1071]{skewness}}
#'
#' @export
#' @rdname mx_describe
`describe` <- function(x, na.rm=TRUE, digits=2, type=1, ...) {
        if (!is.numeric(x)) return(NULL)
        m   <- mean(x, na.rm=na.rm)
        s   <- stats::sd(x, na.rm=na.rm)  # unbiased: sample sd divisor is n-1
        v   <- stats::var(x, na.rm=na.rm)
        na  <- sum(is.na(x))
        n   <- length(x)-na
        se  <- s/sqrt(n-1)
        cv  <- ifelse(m!=0, s/m*100, 0)
        skw <- e1071::skewness(x, na.rm=na.rm, type=type)
        krt <- e1071::kurtosis(x, na.rm=na.rm, type=type)
        mi  <- min(x, na.rm=na.rm)
        mx  <- max(x, na.rm=na.rm)
        out <- c(mean=m, sd=s, var=v, sem=se, cv=cv, n=n, NAs=na,
                 skw=skw, krt=krt, min=mi, max=mx)
        out <- round(out, digits=digits)
        return(out)
}
#' @export
#' @rdname mx_describe
`mx_describe` <- function(x, na.rm=TRUE, digits=2, type=1, ...) {
        list(Row_Summary = t(apply(x, 1, describe)),
             Col_Summary = t(apply(x, 2, describe)))
}
