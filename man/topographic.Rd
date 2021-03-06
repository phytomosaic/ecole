% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topographic.R
\name{foldasp}
\alias{foldasp}
\alias{htld}
\alias{pdir}
\title{Topographic measures}
\usage{
foldasp(asp = 0, ...)

htld(lat = 0, slo = 0, asp = 0, lognat = FALSE, digits = 3, ...)

pdir(lat = 0, slo = 0, asp = 0, lognat = FALSE, digits = 3, ...)
}
\arguments{
\item{asp}{vector of aspect, in degrees 0-360}

\item{...}{further arguments passed to other methods}

\item{lat}{vector of latitude, in degrees 0-90}

\item{slo}{vector of slope, in degrees 0-60}

\item{lognat}{logical, should result be on natural log scale?}

\item{digits}{default=3, integer number of digits for rounding}
}
\value{
Vector of values based on topographic attributes.
}
\description{
Calculate folded aspect, heatload, and potential direct incident
radiation (PDIR) of a slope based on topographic measures.
}
\details{
Aspect units are degrees. Follows pg. 605 of McCune and Keon
(2002).

Heatload is unitless. Follows Equation 2 of McCune and Keon (2002).

PDIR units are MJ cm-2 yr-1. Follows Equation 2 of McCune and Keon
(2002).
}
\examples{
# simple example:
lats    <- c(41.2, 44.7, 45.8)
slopes  <- c(10, 0.1, 34)
aspects <- c(12, 146, 240)

foldasp(aspects)
htld(lats, slopes, aspects, lognat=FALSE)
pdir(lats, slopes, aspects, lognat=FALSE)


# from Table 2 in McCune & Keon (2002):
htld(lat=40, slo=30, asp=0,   lognat=TRUE)
htld(lat=40, slo=30, asp=180, lognat=TRUE)
htld(lat=40, slo=0,  asp=0,   lognat=TRUE)

# from Table 2 in McCune & Keon (2002):
pdir(lat=40, slo=30, asp=0,   lognat=TRUE)     # expect -0.889
pdir(lat=40, slo=30, asp=180, lognat=TRUE)     # expect -0.005
pdir(lat=40, slo=0,  asp=0,   lognat=TRUE)     # expect -0.202

}
\references{
McCune, B., and D. Keon. 2002. Equations for potential
    annual direct incident radiation and heat load. Journal of
    Vegetation Science 13:603-606.

McCune, B. 2007. Improved estimates of incident radiation and heat
    load using nonparametric regression against topographic
    variables. Journal of Vegetation Science 18:751-754.
}
\concept{topographic}
