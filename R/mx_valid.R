#' Test matrix validity
#'
#' Affirm whether a species abundance matrix is valid (no NAs, no
#'     zero-sum rows, no zero-sum columns, all data 'connected').
#'
#' @param x array of species data, where rows = SUs and cols = species
#'
#' @param checkconnect logical, check whether data are connected,
#'     using \code{\link[vegan]{distconnected}}?  Default \code{FALSE}
#'     saves time and memory.
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' Logical value, \code{TRUE} == valid, \code{FALSE} == not valid.
#'
#' @details
#' Often useful before further analysis such as calculating certain
#' dissimilarity measures or during rejection sampling of simulated
#' data.  Data are strictly `connected` when all sample units share
#' the same species pool (no disconnected species pools), checked
#' using \code{\link[vegan]{distconnected}} and Bray-Curtis
#' dissimilarities.
#'
#' @examples
#' # species abundance data
#' set.seed(1917)
#' spe <- data.frame(matrix(rnorm(30, 10, 50), 10, 3))
#' spe[spe < 0] <- 0
#' colnames(spe) <- c('Acer rubrum','Acer saccharum','Acer negundo')
#' spe
#' mx_valid(spe) # expect TRUE
#' spe[,2] <- 0
#' spe[4,] <- 0
#' mx_valid(spe) # expect FALSE
#'
#' @export
#' @rdname mx_valid
`mx_valid` <- function(x, checkconnect=FALSE, ...){
     lacksna <- !anyNA(x)
     allrow  <- all(rowSums(x, na.rm=T)!=0)
     allcol  <- all(colSums(x, na.rm=T)!=0)
     if (checkconnect){
          isconnected <- sum(unique(vegan::distconnected(
               suppressWarnings(vegan::vegdist(x)),1,F)))==1
     } else {
          isconnected <- TRUE
     }
     return(lacksna & allrow & allcol & isconnected)
}
