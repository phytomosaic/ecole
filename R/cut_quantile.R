#' @title Cut by quantile
#'
#' @description
#' Cut a vector into roughly equal-sized groups, based on quantile
#'     breaks.
#'
#' @param x vector of numeric values to cut.
#'
#' @param n number of groups desired.
#'
#' @param ordered logical: should the result be an ordered factor?
#'
#' @param labels if `labels = FALSE`, simple integer codes are returned
#'     instead of a factor; see \code{\link[base]{cut}}.
#'
#' @param ... additional arguments passed to \code{\link[base]{cut}}.
#'
#' @return
#' A factor is returned, unless `labels = FALSE` which results in an
#'     integer vector of level codes.
#'
#' @details
#' Useful to create balanced groups of _roughly_ equal size.
#'
#' @examples
#' set.seed(44)
#' x <- rnorm(99)   # vector of numeric values
#' (y <- cut_quantile(x, n = 3))
#' (z <- cut_quantile(x, n = 3, labels = FALSE))
#'
#' @export
#' @rdname cut_quantile
`cut_quantile` <- function (x, n=7, ordered=FALSE, labels = NULL,
                            ...){
     brk <- stats::quantile(x, probs=seq(0,1,len=n+1), na.rm=TRUE)
     cut(x, brk, include.lowest = TRUE, ordered=ordered,
         labels=labels, ...)
}
