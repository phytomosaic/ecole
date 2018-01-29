#' Drop zero-sum rows or columns
#'
#' Drop zero-sum rows or columns from a species abundance matrix,
#'     often needed before calculating dissimilarities.
#'
#' @param x array of species data, where rows = SUs and cols = species
#'
#' @param method either one of \code{'col'} or \code{'row'}
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' Modified dataframe or matrix, with fewer rows or columns than the
#'     original.
#'
#' @details
#' Often useful before calculating certain dissimilarity measures.
#'
#' @examples
#' # species abundance data
#' set.seed(1917)
#' spe <- data.frame(matrix(rnorm(30, 10, 50), 10, 3))
#' spe[spe < 0] <- 0
#' colnames(spe) <- c('Acer rubrum','Acer saccharum','Acer negundo')
#' spe
#' mx_dropzero(spe) # returns unchanged abundance matrix or data.frame
#' spe[,2] <- 0
#' spe[4,] <- 0
#' mx_dropzero(spe, 'col') # second column removed
#' mx_dropzero(spe, 'row') # fourth row removed
#'
#' @export
#' @rdname mx_dropzero
### drop zero-sum rows or zero-sum columns in matrix data
`mx_dropzero` <- function(x, method='col', ...){
     method <- match.arg(method, c('col','row'))
     `f` <- function(x) sum(x, na.rm=TRUE)
     switch(method,
            row = {
                 out <- x[apply(x, MARGIN=1, f)>0, ]
                 margin <- 1
            },
            col = {
                 out <- x[ ,apply(x, MARGIN=2, f)>0]
                 margin <- 2
            }
     )
     delta <- (dim(x) - dim(out))[[margin]]
     cat(delta, paste0(method,'s removed\n'))
     return(out)
}
