#' @title Ordination correlations
#'
#' @description
#' Correlations of ordination scores versus the original dissimilarity matrix,
#' the main species matrix, or a secondary matrix.
#'
#' @param object  ordination object.
#'
#' @param diss  matrix of original dissimilarities (should exactly match those
#'     used in the ordination).
#'
#' @param x  matrix or data.frame of variables to correlate with the ordination.
#'
#' @param method  a character string indicating which correlation coefficient
#'      to compute, one of "pearson" (default), "kendall", or "spearman".
#'
#' @param ... further arguments passed to \code{stats::cor()}.
#'
#' @return
#' Matrix of correlations or variance explained, per ordination axis.
#'
#' @details
#' For \code{ord_varexpl}, the correlation is calculated between each ordination
#' axis versus the original dissimilarity matrix \code{diss}.  This can be
#' interpreted as the percentage of variance (in the original dissimilarity
#' matrix) explained.
#'
#' For \code{ord_corr}, the correlation is calculated between each ordination
#' axis versus each column of the supplied matrix \code{x}.  This can be
#' interpreted as the strength of association between ordination scores and each
#' individual variable.
#'
#' @examples
#' data(smoky, package='ecole')
#' spe <- smoky$spe
#' env <- smoky$env
#' D   <- vegan::vegdist(spe, method='bray') # original dissimilarities
#' ord <- stats::cmdscale(D, k=5)            # ordination
#' ord_varexpl(ord, D)
#' ord_corr(ord, spe)
#' ord_corr(ord, env)
#'
#' @export
#' @rdname ord_corr
`ord_varexpl` <- function(object, diss, ...) {
    scr   <- vegan::scores(object, display='si')
    cumul <- sapply(1:NCOL(scr), function(j) cor(dist(scr[,1:j]), diss, ...) ^ 2)
    incrm <- c(cumul[1], diff(cumul))
    t(matrix(c(incrm, cumul), ncol=2,
             dimnames=list(colnames(scr), c('increment','cumulative'))))
}
#' @export
#' @rdname ord_corr
`ord_corr` <- function(object, x, method = c('pearson', 'kendall', 'spearman'),
                       ...) {
    scr   <- vegan::scores(object, display='si')
    x     <- as.data.frame(x)
    nm    <- colnames(x)
    isnum <- sapply(x, is.numeric)
    x     <- as.matrix(x[,isnum])
    colnames(x) <- nm[isnum]
    cor(x, scr, method=method, ...)
}
