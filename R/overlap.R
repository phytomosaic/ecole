#' Overlap of two probability density functions
#'
#' Calculate overlap of two probability density functions as their
#'     intersection.  Can compare two 'niche' distributions.
#'
#' @param a  first vector of values.
#'
#' @param b  second vector of values, potentially of different
#'     length than \code{a}.
#'
#' @param buff  multiplier for buffer at tail ends, expressed as
#'     proportion of total data range; default is 0.05.
#'
#' @param na.rm logical, remove NA values? Default is
#'     \code{na.rm = TRUE}.
#'
#' @param ... further arguments passed to additional methods.
#'
#' @return Numeric value for coefficient of overlap.
#'
#' @details Overlap is calculated as two times the area under the
#'     intersection, divided by total area under both curves.
#'
#' @examples
#' set.seed(122)
#' N  <- 999
#' x1 <- rnorm(N)
#' x2 <- rnorm(N+99, 2) # lengths allowed to differ
#' plot(density(x1, from=-5, to=5), xlab='x', main='', las=1, bty='l')
#' lines(density(x2, from=-5, to=5), lty=2)
#' overlap(x1,x2)
#' text(1, 0.1, round(overlap(x1,x2),2))
#'
#' @seealso
#' \url{https://stats.stackexchange.com/questions/97596/}
#'
#' @export
`overlap` <- function (a, b, buff = 0.05, na.rm = TRUE, ...) {
     if (buff > 0.05) {
          warning('buffer is >5% of data range, suggest decreasing')
     }
     rng  <- diff(range(c(a, b), na.rm=na.rm))
     fuzz <- rng * 0.005
     bf   <- rng * buff
     lwr  <- min(c(a, b), na.rm = na.rm) - bf
     upr  <- max(c(a, b), na.rm = na.rm) + bf
     if (length(unique(na.omit(a))) == 1) {
          a <- c(a - fuzz, a, a + fuzz)
     }
     if (length(unique(na.omit(b))) == 1) {
          b <- c(b - fuzz, b, b + fuzz)
     }
     da <- density(a, from = lwr, to = upr, na.rm = na.rm, ...)
     db <- density(b, from = lwr, to = upr, na.rm = na.rm, ...)
     d  <- data.frame(x = da$x, a = da$y, b = db$y)
     d$w <- pmin(d$a, d$b)
     ab <- sfsmisc::integrate.xy(d$x, d$a) + sfsmisc::integrate.xy(d$x,
                                                                   d$b)
     w <- sfsmisc::integrate.xy(d$x, d$w)
     out <- 2 * w/ab
     return(out)
}
