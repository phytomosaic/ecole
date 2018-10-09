#' @name smoky
#' @title Whittaker's Great Smoky Mountains Vegetation Data (USA)
#' @aliases smoky
#' @docType data
#' @description
#' Trees and environment, showing vegetation changes along a moisture
#'     gradient in Great Smoky Mountains National Park, USA.  This is
#'     Whittaker's (1956) Table 3.
#'
#' @format A list of 2 data.frames:\cr
#'     - \code{spe} 12 observations of 41 woody plant species,\cr
#'     - \code{env} 12 observations of 7 environmental variables.
#'
#' @details Species matrix values are abundance of tree species in 12
#'     stations, (percentages of total woody plant stems per station
#'     > or = 1-inch diameter). Each station is an aggregate of 1 to 7
#'     plots of variable size and sampling intensity (!). Presences
#'     < 0.5 percent are coded as 0.1 here.\cr
#'
#'     Environmental matrix values are:\cr
#'     - \code{mesic} mesic indicator value,\cr
#'     - \code{submesic} submesic indicator value,\cr
#'     - \code{subxeric} subxeric indicator value,\cr
#'     - \code{xeric} xeric indicator value,\cr
#'     - \code{treect} count of trees per station,\cr
#'     - \code{nsites} count of sites per station,\cr
#'     - \code{moisture} position on a putative moisture gradient,
#'         ranging from 1 = mesic to 12 = xeric.\cr
#'     The first four variables are species weighted averages as
#'     moisture indicator values.\cr
#'
#'     Whittaker's caption verbatim:\cr
#'     \dQuote{Table 3. Composite transect of moisture gradient
#'     between 3500 and 4500 ft, distribution of trees along gradient.
#'     Transect along the moisture gradient from mesic valley sites
#'     (Sta. 1) to xeric southwest slope sites (Sta. 12), based on 46
#'     site counts including 4906 stems from elevations between 3500
#'     ft and 4500 ft. All figures are percentages of total stems in
#'     station from 1-in. diameter class up.
#'     }\cr
#'
#' @source Table 3 in Whittaker (1956).
#'
#' @references
#' Whittaker, R. H. 1956. Vegetation of the Great Smoky Mountains.
#'     Ecological Monographs 26:2–80.
#'
#' @examples
#' # split into two data.frames
#' data(smoky)
#' spe <- smoky$spe
#' env <- smoky$env
#'
#' # describe the species abundance matrix
#' mx_diversity(spe)
#' mx_valid(spe)
#'
#' # visualize the species abundance matrix
#' plot_heatmap(spe, xord='wa', yord='wa', logbase=10, yexp=1.7,
#'     asp=1)
#'
#' # roughly following Whittaker's Fig. 4, top:
#' e <- cbind(sapply(env[,1:4], standardize), moisture=env$moisture)
#' plot(1:12, ylim=c(0,1), type='n', las=1, bty='l', ylab='')
#' for(i in 1:4){
#'      points(e[,i], pch=16, col=i)
#'      lines(loess(e[,i] ~ e[,'moisture']), col=i)
#' }
#'
#' @keywords datasets
"smoky"
