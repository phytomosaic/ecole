% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mx_describe.R
\name{describe}
\alias{describe}
\alias{mx_describe}
\title{Descriptive summary statistics}
\usage{
describe(x, na.rm = TRUE, digits = 2, type = 1, ...)

mx_describe(x, na.rm = TRUE, digits = 2, type = 1, ...)
}
\arguments{
\item{x}{vector of values to be summarized.}

\item{na.rm}{logical, defaults to TRUE.}

\item{digits}{numeric, number of digits for rounding.}

\item{type}{default=1, an integer between 1 and 3 selecting one of
the algorithms for computing skewness detailed in
\code{\link[e1071]{kurtosis}}.}

\item{...}{further arguments passed to other methods}
}
\value{
For \code{describe}, a list with descriptive statistics: \itemize{
  \item mean = mean
  \item sd = standard deviation
  \item var  = variance
  \item sem = standard error of the mean
  \item cv = coefficient of variation
  \item n = count of real elements
  \item NAs = count of NA elements
  \item skw = skewness
  \item krt = kurtosis
  \item min = minimum value
  \item max = maximum value
  }\cr

For \code{mx_describe}, a list of two items showing row and column
  summaries, each containing the above items.
}
\description{
Descriptive summary statistics for .
}
\details{
Summary statistics applied to a vector or rows/columns of a matrix. For
  skewness and kurtosis: \itemize{
    \item NEG kurtosis = flat distribution (platykurtic)
    \item POS kurtosis = peaked distribution (leptokurtic)
    \item ZERO kurtosis ~ the normal distribution (mesokurtic)
    \item NEG skewness indicates mean < median (left-skewed)
    \item POS skewness indicates mean > median (right-skewed)
    }
}
\examples{
x <- c(rnorm(99, 0.1), NA)
describe(x)
y <- matrix(rnorm(1000), 20, 50)
mx_describe(y)

}
\seealso{
\code{\link[e1071]{kurtosis}},
\code{\link[e1071]{skewness}}
}
