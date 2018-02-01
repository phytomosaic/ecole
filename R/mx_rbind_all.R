#' Combine two species abundance matrices
#'
#' Cleanly combine two species abundance matrices by binding rows and
#'     filling zeros in empty cells.
#'
#' @param xx
#' first array of species data, where rows = SUs and cols = species
#'
#' @param yy
#' second array of species data
#'
#' @return
#' Matrix or data.frame containing all combined species and SUs.
#'
#' @details
#' Top rows are from first matrix \code{xx}, bottom rows are from
#' second matrix \code{yy}.  All names of original sample units and
#' species are retained.  Zeros fill empty cells.
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
#' @seealso
#' \code{rbind.fill} from package \code{plyr}, and
#' \code{rbind_all} from package \code{dplyr}.
#'
#' @export
#' @rdname mx_rbind_all
`mx_rbind_all` <- function(xx, yy) {
     xrn   <- dimnames(xx)[[1]]
     yrn   <- dimnames(yy)[[1]]
     xn    <- dimnames(xx)[[2]]
     yn    <- dimnames(yy)[[2]]
     if(any(sapply(list(xrn,yrn,xn,yn),is.null)))stop('need dimnames')
     un    <- union(xn, yn)   # union == all species names
     xdiff <- setdiff(xn, yn) # species in x not in y
     ydiff <- setdiff(yn, xn) # species in y not in x
     tmp   <- matrix(0L, nrow=length(yrn), ncol=length(xdiff),
                     dimnames=list(yrn, xdiff))
     yy    <- cbind(yy, tmp)[,un]
     tmp   <- matrix(0L, nrow=length(xrn), ncol=length(ydiff),
                     dimnames=list(xrn, ydiff))
     xx    <- cbind(xx, tmp)[,un]
     rbind(xx, yy)
}
