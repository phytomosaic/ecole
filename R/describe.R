#' Describe
#'
#' Descriptive summary statistics
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
#'   \item sd = standard deviation
#'   \item var  = variance
#'   \item sem = standard error of the mean
#'   \item cv = coefficient of variation
#'   \item n = count of real elements
#'   \item NAs = count of NA elements
#'   \item skw = skewness
#'   \item krt = kurtosis
#'   }
#' @details
#' Method-of-moments summaries for first 4 moments.  \itemize{
#'   \item NEG kurtosis = flat distribution (platykurtic)
#'   \item POS kurtosis = peaked distribution (leptokurtic)
#'   \item ZERO kurtosis ~ the normal distribution (mesokurtic)
#'   \item NEG skewness indicates mean < median (left-skewed)
#'   \item POS skewness indicates mean > median (right-skewed)
#'   }
#' @examples
#' # require(e1071)
#' n <- 99
#' x <- c(rnorm(n, 0.1), NA)
#' describe(x)
#'
#' @seealso
#' \code{\link[ecole]{mom}},
#' \code{\link[e1071]{kurtosis}},
#' \code{\link[e1071]{skewness}}
#'
#' @export
`describe` <- function(x, na.rm=TRUE, digits=2, type=1, ...) {
     require(e1071)
     m   <- mean(x, na.rm=na.rm)
     s   <- sd(x, na.rm=na.rm) # unbiased: sample sd divisor is n-1
     v   <- var(x, na.rm=na.rm)
     na  <- sum(is.na(x))
     n   <- length(x)-na
     se  <- s/sqrt(n-1)
     cv  <- ifelse(m!=0, s/m*100, 0)
     skw <- skewness(x, na.rm=na.rm, type=type)
     krt <- kurtosis(x, na.rm=na.rm, type=type)
     out <- data.frame(mean=m, sd=s, var=v, sem=se, cv=cv,
                       n=n, NAs=na, skw=skw, krt=krt)
     out <- round(out, digits=digits)
     mode(out) <- "numeric"
     return(out)
}