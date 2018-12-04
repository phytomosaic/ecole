#' Prepare data for  PC-ORD
#'
#' Prepare data for export to PC-ORD (>= 5.0), with required formats
#'     and column headers.
#'
#' @param x array of species or environmental data, where rows = SUs
#'     and row.names = SU identifiers.
#'
#' @param filename string, name for new file; if NULL then returns an
#'     R object; if file already exists then a timestamp is added.
#'
#' @param outdir string, directory to which file will be written.
#'
#' @param header logical, add a PC-ORD header?
#'
#' @param ... further arguments passed to other methods.
#'
#' @return
#' .xls file (optionally an R object) with automatic PC-ORD header.
#'
#' @details
#' Writes a file compatible with PC-ORD (McCune and Mefford 2011), and
#'     automates the addition of a PC-ORD header. Empty cells not
#'     allowed.  Column names longer than 10 characters are
#'     automatically abbreviated to \emph{unique} 10-char names;
#'     PC-ORD abbreviates but doesn't check for duplicates.
#'
#' In R first call \code{prep_pcord(header = TRUE)}, then in PC-ORD
#'     do: \code{File > Import > Main matrix > Excel spreadsheet (
#'     with PC-ORD header)}.
#'
#' @examples
#' # species abundance data
#' spe <- data.frame(matrix(rnorm(9, 10), 3, 3))
#' colnames(spe) <- c("Acer rubrum",
#'                    "Acer saccharum",
#'                    "Acer saccharinum")
#' lookin <- getwd()
#' prep_pcord(spe, filename="spe_example", outdir=lookin)
#'
#' # environmental data including some factors
#' env <- data.frame(soil = c("A","B","D"),
#'                   aspect = c("N","S","SE"),
#'                   slope = c(1,17,9))
#' prep_pcord(env, filename="env_example", outdir=lookin)
#'
#' # header not strictly required
#' prep_pcord(env, filename="env_example", outdir=lookin, header=FALSE)
#'
#' # allows no element to be empty or NA
#' env[2,3] <- NA
#' ### NOT RUN:
#' # prep_pcord(env, filename="env_example", outdir=lookin)
#' ### END NOT RUN
#'
#' # allows only xls output
#' ### NOT RUN:
#' # prep_pcord(env, filename="env_example.csv", outdir=lookin)
#' ### END NOT RUN
#'
#' @references
#' McCune, B., and M. J. Mefford. 2011. PC-ORD. Multivariate Analysis
#'     of Ecological Data. Version 6. MjM Software Design, Gleneden
#'     Beach, OR.
#'
#' @export
#' @rdname prep_pcord
`prep_pcord` <- function(x, filename=NULL, outdir=getwd(),
                      header=TRUE, ... ){
     if( is.null(filename) ){
          stop("filename required")
     }
     if( any(grepl("[.]", filename)) ){
          stop("no periods allowed in filename")
     }
     if(!is.data.frame(x)){
          x <- data.frame(x, stringsAsFactors=F)
     }
     if( any(is.na(x) | x=="") ) {
          stop("must fill empty cells")
     }
     i   <- sapply(x, is.factor)             # index factor columns
     x[i]<- lapply(x[i], as.character)       # force factor to char
     if( any(sapply(x, class) != "numeric") ){
          message("data included some non-numeric values")
     }
     if( header ){                           # assemble PCORD header
          c1 <- c(as.numeric(nrow(x)),       # column 1
                  as.numeric(ncol(x)), NA, NA,
                  as.character(row.names(x)))
          row.names(x) <- NULL
          r1 <- c("SUs", rep(NA, ncol(x)-1)) # header row 1
          r2 <- c("spp", rep(NA, ncol(x)-1)) # header row 2
          r3 <- ifelse(i, "C", "Q")          # header row 3
          r4 <- colnames(x)                  # header row 4
          if( any(nchar(r4) >10 ) ){
               r4 <- abbreviate(r4, minlength=10)
               message("column names abbreviated to 10 characters")
          }
          x  <- rbind(r1, r2, r3, r4, x)
          x  <- cbind(c1, x)
          colnames(x) <- NULL
     } else {
          SUs <- as.character(row.names(x))
          x   <- cbind(SUs, x) # add colnames
     }
     outdir <- ifelse(grepl("[/]", outdir),
                      outdir <- paste0(outdir, "/"), outdir)
     full <- file.path(outdir, filename, ".xls", fsep="")
     if( file.exists(full)){
          full <- file.path(outdir, filename,
                            format(Sys.time(),"%H-%M-%S"),
                            ".xls", fsep="")
          message("file already exists, renaming now")
     }
     write.table(x, file=full, row.names=F, quote=F, na="", sep="\t")
     message("file written to:\n\t", full)
}
