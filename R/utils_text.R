#' @title
#' Clean character vector
#'
#' @description
#' Clean a character vector (text string) by replacing punctuation and
#' whitespaces with underscores.
#'
#' @param x
#' character vector
#'
#' @param lower
#' logical, convert all characters to lowercase?
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' Cleaned-up character vector of the same length and with the same
#' attributes as \code{x}.
#'
#' @details
#' Useful to clean a vector of species names, perhaps obtained from a
#' biodiversity database, to a machine-readable form.
#'
#' @examples
#' # sloppy
#' x <- c('Alnus___incana', 'Alnus incana ssp.tenuifolia',
#'        'Alnus rubra  ', '  Alnus viridis',
#'        'Alnus viridis ssp. crispa', 'Alnus viridis ssp.. sinuata')
#' # cleaned
#' clean_text(x)
#' clean_text(x, lower=TRUE)
#' clean_text('Alnus viridis ssp.. sinuata ')
#'
#' @export
#' @rdname utils_text
`clean_text` <- function(x, lower=F, ...){
     if(!is.vector(x)) stop('`x` must be vector')
     x <- trimws(x, 'both')        # trim trailing and leading spaces
     x <- gsub('[[:punct:]]{1,}',' ', x) # sub space for punctuation
     x <- gsub('[[:space:]]{1,}','_', x) # sub underscore for space
     x <- gsub('_{1,}','_', x)     # sub single for double underscore
     x <- gsub('_$','', x)         # strip flanking underscores
     if(lower) x <- tolower(x)
     x
}
