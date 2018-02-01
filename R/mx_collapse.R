#' @title
#' Collapse columns in a species matrix
#'
#' @description
#' Collapse two matrix or data.frame columns into one, where new
#' values are the min, max, or mean.  Appropriate for synonymizing
#' species (columns) in a species abundance matrix.
#'
#' @param x
#' array of species data, where rows = SUs and cols = species,
#' containing the two columns to be collapsed and synonymized
#'
#' @param c1
#' column name (to be retained)
#'
#' @param c2
#' column name (to be omitted)
#'
#' @param method
#' method to retain values, one of 'min', 'max' or 'mean'
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' Modified dataframe or matrix, with one fewer column than the
#' original.
#'
#' @details
#' Appropriate for synonymizing species (columns) in a species
#' abundance matrix. For example, if a species occurs twice in a plot,
#' simply take the highest, lowest, or mean of observed values.  The
#' name of the first column is retained, the second column is removed.
#'
#' @examples
#' # species abundance data
#' set.seed(1917)
#' spe <- data.frame(matrix(rnorm(30, 10, 50), 10, 3))
#' spe[spe < 0] <- 0
#' colnames(spe) <- c('Acer rubrum','Acer saccharum','Acer negundo')
#' spe
#' mx_collapse(spe, c1='Acer rubrum', c2='Acer negundo')
#' mx_collapse(spe, c1='Acer rubrum', c2='Acer negundo', method='mean')
#'
#' @export
#' @rdname mx_collapse
`mx_collapse` <- function(x, c1, c2, method='max', ...){
     if(!is.character(c(c1, c2))) stop('check column names')
     if( !all( c(c1, c2) %in% dimnames(x)[[2]] )  ){
          warning('\nnames not present, returning original matrix\n')
          return(x)
     }
     method <- match.arg(method, c('max','min','mean'))
     if(method=='max'){
          x[,c1] <- pmax(x[,c1], x[,c2], na.rm = TRUE)
     }
     if(method=='min'){
          x[,c1] <- pmin(x[,c1], x[,c2], na.rm = TRUE)
     }
     if(method=='mean'){
          x[,c1] <- rowMeans(cbind(x[,c1], x[,c2]), na.rm=TRUE)
     }
     x[,!(dimnames(x)[[2]] %in% c2)]
}
