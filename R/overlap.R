#' Overlap of two probability density functions
#'
#' Calculate overlap of two probability density functions as their
#'     intersection.  Can compare two 'niche' distributions.
#'
#' @param a  first vector of values
#'
#' @param b  second vector of values, potentially of different
#'     length than \code{a}
#'
#' @param buff  multiplier for buffer at tail ends, expressed as
#'     proportion of total data range; default is 0.1
#'
#' @param ... further arguments passed to additional methods
#'
#' @return Numeric value for coefficient of overlap
#'
#' @details Overlap is calculated as two times the area under the
#'     intersection, divided by total area under both curves
#'
#' @examples
#' set.seed(122)
#' N <- 999
#' x1 <- rnorm(N)
#' x2 <- rnorm(N+99, 2) # lengths allowed to differ
#' plot(density(x1, from=-5, to=5), xlab='x', main='', las=1, bty='l')
#' lines(density(x2, from=-5, to=5), lty=2)
#' overlap(x1,x2)
#' text(1, .1, round(overlap(x1,x2),2))
#'
#' @seealso
#' \url{https://stats.stackexchange.com/questions/97596/}
#'
#' @export
`overlap` <- function(a, b, buff=0.1, ...){

     if(buff>0.1){
          warning('Buffer is >10% of data range, suggest decreasing')
     }

     # define limits of a common grid, w buffer so tails aren't cut
     bf <- diff(range(c(a, b), ...)) * buff
     lower <- min(c(a, b), ...) - bf
     upper <- max(c(a, b), ...) + bf

     # estimate kernel densities
     da <- density(a, from=lower, to=upper, ...)
     db <- density(b, from=lower, to=upper, ...)
     d  <- data.frame(x=da$x, a=da$y, b=db$y)

     # calculate intersection of densities
     d$w <- pmin(d$a, d$b)

     # integrate areas under curves
     total    <- sfsmisc::integrate.xy(d$x, d$a) +
                                   sfsmisc::integrate.xy(d$x, d$b)
     intersxn <- sfsmisc::integrate.xy(d$x, d$w)

     # calc overlap coefficient (is effectively Sorenson similarity)
     ovlp  <- 2 * intersxn / total
     return(ovlp)
}
