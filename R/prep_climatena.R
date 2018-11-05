#' Prepare data for ClimateNA
#'
#' Prepare data to query climate values using ClimateNA (>= 5.0),
#'     with required formats and column names.
#'
#' @param ID1 vector of primary identifier values (required)
#'
#' @param ID2 vector of secondary identifier values (optional)
#'
#' @param lat latitude values, in decimal degrees (required)
#'
#' @param long longitude values, in decimal degrees (required)
#'
#' @param el vector of elevation values, in meters a.s.l. (optional)
#'
#' @param file filename for .csv output
#'
#' @param ... further arguments currently ignored
#'
#' @return
#' Writes a csv file ready for ClimateNA querying.
#'
#' @details
#' Works with ClimateNA (Wang et al. 2016) versions >= 5.0.
#'
#' @examples
#' data(braun)
#' env <- braun$env
#' tmp <- tempfile(fileext = '.csv')
#' prep_climatena(ID1=rownames(env), ID2=NA, lat=env$lat,
#'                long=env$lon, el=NA, file=tmp)
#' head(read.csv(tmp), 10)
#' unlink(tmp)
#'
#' @references
#' Wang, T., Hamann, A., Spittlehouse, D.L., Carroll, C. 2016. Locally
#'     downscaled and spatially customizable climate data for
#'     historical and future periods for North America. PLoS ONE
#'     11(6).
#'
#' @export
#' @rdname prep_climatena
`prep_climatena` <- function(ID1, ID2=NA, lat, long, el=NA,
                             file='./to_climatena.csv', ...){
     allvec <- all(sapply(list(ID1,ID2,lat,long,el), is.vector))
     if (!allvec) stop('all arguments must be vectors')
     d <- data.frame(ID1=ID1, ID2=ID2, lat=lat, long=long, el=el)
     write.csv(d, file=file, row.names = FALSE)
}
