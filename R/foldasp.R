#' Folded aspect
#'
#' Aspect of a slope after folding along a SW-NE axis
#'
#' @param asp  vector of aspect, in degrees 0-360
#'
#' @return
#' Vector of folded aspect, in degrees.
#'
#' @details
#' Heatload is unitless. Follows pg. 605 of McCune and Keon (2002).
#'
#' @examples
#' # simple example:
#' aspects <- c(12, 146, 240)
#' foldasp(aspects)
#'
#' @family topography functions
#' @seealso \code{\link{pdir}} for potential direct incident radiation
#'     , and \code{\link{htld}} for heatload.
#'
#' @references
#' McCune, B., and D. Keon. 2002. Equations for potential
#'     annual direct incident radiation and heat load. Journal of
#'     Vegetation Science 13:603-606.\cr
#' McCune, B. 2007. Improved estimates of incident radiation and heat
#'     load using nonparametric regression against topographic
#'     variables. Journal of Vegetation Science 18:751-754.
#' @export
`foldasp` <- function(asp=0) {
     abs(180-abs(asp-225))
}