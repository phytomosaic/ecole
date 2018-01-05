#' Topographic measures
#'
#' Calculate folded aspect, heatload, and potential direct incident
#' radiation (PDIR) of a slope based on topographic measures.
#'
#' @param lat  vector of latitude, in degrees 0-90
#'
#' @param slo  vector of slope, in degrees 0-60
#'
#' @param asp  vector of aspect, in degrees 0-360
#'
#' @param lognat logical, should result be on natural log scale?
#'
#' @param digits default=3, integer number of digits for rounding
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#'  Vector of values based on topographic attributes.
#'
#' @details
#' Aspect units are degrees. Follows pg. 605 of McCune and Keon
#' (2002).
#'
#' Heatload is unitless. Follows Equation 2 of McCune and Keon (2002).
#'
#' PDIR units are MJ cm-2 yr-1. Follows Equation 2 of McCune and Keon
#' (2002).
#'
#' @examples
#' # simple example:
#' lats    <- c(41.2, 44.7, 45.8)
#' slopes  <- c(10, 0.1, 34)
#' aspects <- c(12, 146, 240)
#'
#' foldasp(aspects)
#' htld(lats, slopes, aspects, lognat=FALSE)
#' pdir(lats, slopes, aspects, lognat=FALSE)
#'
#'
#' # from Table 2 in McCune & Keon (2002):
#' htld(lat=40, slo=30, asp=0,   lognat=TRUE)
#' htld(lat=40, slo=30, asp=180, lognat=TRUE)
#' htld(lat=40, slo=0,  asp=0,   lognat=TRUE)
#'
#' # from Table 2 in McCune & Keon (2002):
#' pdir(lat=40, slo=30, asp=0,   lognat=TRUE)     # expect -0.889
#' pdir(lat=40, slo=30, asp=180, lognat=TRUE)     # expect -0.005
#' pdir(lat=40, slo=0,  asp=0,   lognat=TRUE)     # expect -0.202
#'
#' @family topographic
#'
#' @references
#' McCune, B., and D. Keon. 2002. Equations for potential
#'     annual direct incident radiation and heat load. Journal of
#'     Vegetation Science 13:603-606.
#'
#' McCune, B. 2007. Improved estimates of incident radiation and heat
#'     load using nonparametric regression against topographic
#'     variables. Journal of Vegetation Science 18:751-754.
#'
#' @export
#' @rdname topographic
`foldasp` <- function(asp=0, ...) {
     abs(180-abs(asp-225))
}
#' @export
#' @rdname topographic
`htld`  <- function(lat=0, slo=0, asp=0, lognat=FALSE, digits=3, ...){
     asp <- abs(180-abs(asp-225)) # 1st fold aspect along SW-NE axis
     asp <- asp*pi/180     # convert to radians
     slo <- slo*pi/180
     lat <- lat*pi/180
     out <- exp(
          (-1.236) +     # differs by 'exp()' from McCune & Keon
               ( 1.350) * cos(lat) * cos(slo) +
               (-1.376) * cos(asp) * sin(slo) * sin(lat) +
               (-0.331) * sin(lat) * sin(slo) +
               ( 0.375) * sin(asp) * sin(slo)
     )
     if(lognat){         # same ln scale as McCune & Keon
          out <- log(out, base = exp(1))
     }
     out <- round(out, digits=digits)
     mode(out) <- 'numeric'
     return(out)
}
#' @export
#' @rdname topographic
`pdir`  <- function(lat=0, slo=0, asp=0, lognat=FALSE, digits=3, ...){
     asp <- asp*pi/180     # convert to radians
     slo <- slo*pi/180
     lat <- lat*pi/180
     out <- exp(
          (-1.236) +     # differs by 'exp()' from McCune & Keon
               ( 1.350) * cos(lat) * cos(slo) +
               (-1.376) * cos(asp) * sin(slo) * sin(lat) +
               (-0.331) * sin(lat) * sin(slo) +
               ( 0.375) * sin(asp) * sin(slo)
     )
     if(lognat){         # same ln scale as McCune & Keon
          out <- log(out, base = exp(1))
     }
     out <- round(out, digits=digits)
     mode(out) <- 'numeric'
     return(out)
}
