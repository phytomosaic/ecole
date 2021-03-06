% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlap.R
\name{overlap}
\alias{overlap}
\title{Overlap of two probability density functions}
\usage{
overlap(a, b, buff = 0.05, na.rm = TRUE, ...)
}
\arguments{
\item{a}{first vector of values.}

\item{b}{second vector of values, potentially of different
length than \code{a}.}

\item{buff}{multiplier for buffer at tail ends, expressed as
proportion of total data range; default is 0.05.}

\item{na.rm}{logical, remove NA values? Default is
\code{na.rm = TRUE}.}

\item{...}{further arguments passed to additional methods.}
}
\value{
Numeric value for coefficient of overlap.
}
\description{
Calculate overlap of two probability density functions as their
    intersection.  Can compare two 'niche' distributions.
}
\details{
Overlap is calculated as two times the area under the
    intersection, divided by total area under both curves.
}
\examples{
set.seed(122)
N  <- 999
x1 <- rnorm(N)
x2 <- rnorm(N+99, 2) # lengths allowed to differ
plot(density(x1, from=-5, to=5), xlab='x', main='', las=1, bty='l')
lines(density(x2, from=-5, to=5), lty=2)
overlap(x1,x2)
text(1, 0.1, round(overlap(x1,x2),2))

}
\seealso{
\url{https://stats.stackexchange.com/questions/97596/}
}
