#' @title
#' Winnow matrix
#'
#' @description
#' Winnow out \code{NA} values from a matrix or data.frame, based on
#'     row and column totals.
#'
#' @param x
#' matrix or data.frame.
#'
#' @param tol_row,tol_col
#' numeric, giving the number of \code{NA}s that will be tolerated in
#'     rows or columns. Default \code{0} will return an array totally
#'     free of \code{NA} values.
#'
#' @param order
#' logical, should matrix rows and columns be re-ordered by number of
#'     \code{NA}s upon exiting? Default = \code{FALSE}.
#'
#' @return
#' Modified dataframe or matrix, with added attributes for original
#'     and new dimensions, and number of \code{NA} that were tolerated
#'     in rows and columns.
#'
#' @details
#' The algorithm iteratively removes the most \code{NA}-heavy columns
#'     and/or rows, up to a user-specified tolerance.  Useful, for
#'     example, to reduce a huge traits matrix to something that is
#'     \code{NA}-free or can have just a few \code{NA} values to be
#'     later imputed or handled.
#'
#' @examples
#' ### hypothetical data
#' set.seed(888)
#' nr <- 10
#' nc <- 16
#' n  <- 27  # number of NA to randomly add
#' x  <- matrix(1, nrow=nr, ncol=nc)
#' na <- cbind(sample(1:nr,n,replace=TRUE),
#'             sample(1:nc,n,replace=TRUE))
#' x[na] <- NA
#' x  <- data.frame(x)
#' x
#' ### winnow
#' (w <- mx_winnow(x, 2, 5)) # tolerate some NA
#' attributes(w)
#' (w <- mx_winnow(x, 0, 0)) # tolerate no NA
#' attributes(w)
#'
#' @export
#' @rdname mx_winnow
`mx_winnow` <- function(x, tol_row=0, tol_col=0, order=FALSE) {
        dimx <- dim(x)
        rsum <- rowSums(is.na(x))
        csum <- colSums(is.na(x))
        while (any(rsum > tol_row) | any(csum > tol_col)) {
                rsum <- rowSums(is.na(x))
                if (any(rsum > tol_row)) {
                        x <- x[rsum != max(rsum),]  # drop rows
                }
                csum <- colSums(is.na(x))
                if (any(csum > tol_col)) {
                        x <- x[,csum != max(csum)]  # drop cols
                }
        }
        if(order) {
                x <- x[,order(colSums(is.na(x)))]
                x <- x[order(rowSums(is.na(x))),]
        }
        attr(x, 'orig_dim') <- dimx
        attr(x, 'new_dim')  <- dim(x)
        attr(x, 'NA_tol_row_col') <- c(tol_row, tol_col)
        return(x)
}
