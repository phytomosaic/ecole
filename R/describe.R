#' @title Describe
#'
#' @description Descriptive summary statistics, by method-of-moments.
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
#'   }\cr
#'
#' For \code{mom}, a list with 4 moments:\itemize{
#'   \item mean = mean
#'   \item var  = variance
#'   \item skew = skewness
#'   \item kurt = kurtosis
#'  }
#'
#' @details
#' Method-of-moments summaries for first 4 moments.  \itemize{
#'   \item NEG kurtosis = flat distribution (platykurtic)
#'   \item POS kurtosis = peaked distribution (leptokurtic)
#'   \item ZERO kurtosis ~ the normal distribution (mesokurtic)
#'   \item NEG skewness indicates mean < median (left-skewed)
#'   \item POS skewness indicates mean > median (right-skewed)
#'   }
#' @examples
#' x <- c(rnorm(99, 0.1), NA)
#' describe(x)
#' mom(x)
#'
#' @seealso
#' \code{\link[e1071]{kurtosis}},
#' \code{\link[e1071]{skewness}}
#'
#' @export
#' @rdname describe
`describe` <- function(x, na.rm=TRUE, digits=2, type=1, ...) {
     if (!is.numeric(x)){
          return(NULL)
     }
     m   <- mean(x, na.rm=na.rm)
     s   <- stats::sd(x, na.rm=na.rm) # unbiased: sample sd divisor is n-1
     v   <- stats::var(x, na.rm=na.rm)
     na  <- sum(is.na(x))
     n   <- length(x)-na
     se  <- s/sqrt(n-1)
     cv  <- ifelse(m!=0, s/m*100, 0)
     skw <- e1071::skewness(x, na.rm=na.rm, type=type)
     krt <- e1071::kurtosis(x, na.rm=na.rm, type=type)
     out <- data.frame(mean=m, sd=s, var=v, sem=se, cv=cv,
                       n=n, NAs=na, skw=skw, krt=krt)
     out <- round(out, digits=digits)
     mode(out) <- 'numeric'
     return(out)
}
#' @export
#' @rdname describe
`mom` <- function(x, na.rm=TRUE, digits=2, type=1, ...) {
     m   <- mean(x, na.rm=na.rm)
     v   <- stats::var(x, na.rm=na.rm)
     skw <- e1071::skewness(x, na.rm=na.rm, type=type)
     krt <- e1071::kurtosis(x, na.rm=na.rm, type=type)
     out <- data.frame(mean=m, var=v, skw=skw, krt=krt)
     out <- round(out, digits=digits)
     mode(out) <- 'numeric'
     return(out)
}
