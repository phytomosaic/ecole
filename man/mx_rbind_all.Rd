% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mx_rbind_all.R
\name{mx_rbind_all}
\alias{mx_rbind_all}
\title{Combine two species abundance matrices}
\usage{
mx_rbind_all(xx, yy)
}
\arguments{
\item{xx}{first array of species data, where rows = SUs and cols = species}

\item{yy}{second array of species data}
}
\value{
Matrix or data.frame containing all combined species and SUs.
}
\description{
Cleanly combine two species abundance matrices by binding rows and
    filling zeros in empty cells.
}
\details{
Top rows are from first matrix \code{xx}, bottom rows are from
second matrix \code{yy}.  All names of original sample units and
species are retained.  Zeros fill empty cells.
}
\examples{
# species abundance data
set.seed(1917)
spe <- data.frame(matrix(rnorm(30, 10, 50), 10, 3))
spe[spe < 0] <- 0
colnames(spe) <- c('Acer rubrum','Acer saccharum','Acer negundo')
spe
mx_valid(spe) # expect TRUE
spe[,2] <- 0
spe[4,] <- 0
mx_valid(spe) # expect FALSE

}
\seealso{
\code{rbind.fill} from package \code{plyr}, and
\code{rbind_all} from package \code{dplyr}.
}
