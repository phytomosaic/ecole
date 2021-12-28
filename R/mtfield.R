#' @name mtfield
#' @title Vegetation and site data from Mount Field, Tasmania
#' @aliases mtfield
#' @docType data
#' @description
#' Vegetation and site data from Mount Field, Tasmania, collected by Peter
#'     Minchin. Taxonomic treatment as recorded in 1983. This is a subset of the
#'     original 438 sample units and 221 species.
#'
#' @format
#' A list of 2 data.frames:\cr
#'     - \code{spe} Species abundance matrix: 424 observations of 209 vascular
#'     plant species.  Values are cover classes (midpoints = 0.05, 0.55, 5.5,
#'     20, 50, 85 percent).\cr
#'     - \code{env} Environmental matrix: 424 observations of 4 environmental
#'     variables describing topography (altitude, drainage, radiation, slope).
#'
#' @source
#' Minchin (1983), as communicated by Roberts (2020).
#'
#' @references
#' Minchin, P. R. 1983. A comparative evaluation of techniques for ecological
#'     ordination using simulated vegetation data and an integrated
#'     ordination-classification analysis of the alpine and subalpine plant
#'     communities of the Mt. Field plateau, Tasmania. Doctoral dissertation,
#'     University of Tasmania, Hobart, Australia.\cr
#'
#' Roberts, D.W. 2020. Comparison of distance‐based and model‐based ordinations.
#'     Ecology 101(1):e02908. doi:10.1002/ecy.2908.
#'
#' @seealso https://doi.org/10.6084/m9.figshare.9912551.v1 and
#'     https://parks.tas.gov.au/explore-our-parks/mount-field-national-park
#'
#' @examples
#' # split into two data.frames
#' data(mtfield)
#' spe <- mtfield$spe
#' env <- mtfield$env
#' @keywords datasets
"mtfield"
