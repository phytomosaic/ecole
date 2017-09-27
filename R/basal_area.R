#' @title Tree basal area
#'
#' @description
#' Total tree basal area via simple geometric area of a circle.
#'
#' @param x vector of tree DBHs in cm (metric)
#'
#' @param total logical, indicating whether to sum the
#'
#' @param ... additional arguments passed to \code{\link[base]{sum}}
#'
#' @return
#' Numeric total basal area per unit:
#'
#' @details
#' Uses simple geometric area of a circle, summed across the vector.
#'
#' @examples
#' dbhs <- c(40, 23, 32.5, 80)   # vector of tree DBH values (cm)
#' ba <- basal_area(dbhs)         # m^2 per unit
#' ba
#'
#' @export
#' @rdname basal_area
`basal_area` <- function(x, total=FALSE, ...){ # x units are cm
     ba_vec <- pi / 4 / 10000 * (x^2) # 10000 cm^2 per m^2
     if(total){
          sum(ba_vec, ...)    # TOTAL basal area (m^2) in x vector
     }else{
          ba_vec              # INDIVIDUAL basal area (m^2)
     }
}




























