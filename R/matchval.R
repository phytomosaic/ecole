#' @title Lookup a value
#
#' @description Lookup a value based on matching IDs among two
#'     data.frames.
#'
#' @param x data.frame containing values of interest.
#'
#' @param y data.frame to be matched.
#'
#' @param val character, name of column containing values in \code{x}.
#'
#' @param xid character, name of ID column in \code{x}.
#'
#' @param yid character, name of ID column in \code{y}.
#'
#' @return
#' Values from \code{x$val}, ordered by \code{y$yid}.
#'
#' @details
#' Similar to vlookup function found elsewhere.  Returns \code{NA} if
#'     no match is found.  If any IDs are duplicated, then only the
#'     first matching value is returned (just like
#'     \code{\link[base]{match}}).
#'
#' @examples
#' x <- data.frame(ident = letters[1:7], foo = rnorm(7))
#' y <- data.frame(ident = letters[5:9], bar = rnorm(5))
#' y$foo <- matchval(x, y, 'foo')
#' x
#' y
#'
#' # be careful you use the intended column name!
#' colnames(y)[1] <- 'identifier'
#' matchval(x, y, 'foo')  # wrongly matches on existing 'foo' column
#' matchval(x, y, 'foo', 'ident', 'identifier')  # correct
#'
#' @seealso \code{\link[base]{match}}.
#'
#' @export
#' @rdname matchval
`matchval` <- function(x, y, val = NULL, xid = NULL, yid = NULL){
     if(!is.data.frame(x) | !is.data.frame(y)){
          stop('both `x` and `y` must be of class `data.frame`')
     }
     xnm <- dimnames(x)[[2]]
     ynm <- dimnames(y)[[2]]
     if(!val %in% xnm){
          stop(paste0('`', val, '` is not a column name in `x`'))
     }
     qnm <- intersect(xnm, ynm)
     if(length(qnm) == 0 & (is.null(xid) | is.null(yid))) {
          stop('please select common identifiers `xid` and `yid`')
     }
     if(is.null(xid) & length(qnm) == 1) {
          xid <- qnm
          message(paste0('Matching on column name `', xid, '`'))
     }
     if(is.null(yid)) yid <- xid
     x[match(y[,yid], x[,xid]), val]
}
