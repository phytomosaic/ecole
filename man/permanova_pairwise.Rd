% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permanova_pairwise.R
\name{permanova_pairwise}
\alias{permanova_pairwise}
\title{PERMANOVA multiple comparisons}
\usage{
permanova_pairwise(
  x,
  grp,
  permutations = 999,
  method = "bray",
  padj = "bonferroni",
  ...
)
}
\arguments{
\item{x}{Community `data.frame` or `dist` object.}

\item{grp}{Vector of groups: factor levels to be compared.}

\item{permutations}{Number of permutations, or else the permutation structure
from `permute::how()`.}

\item{method}{Dissimilarity method from `vegdist` if `x` is not a distance
matrix, default is `bray`.}

\item{padj}{The p-value correction method, one of the methods
supported by `p.adjust()`; default is `bonferroni`.}

\item{...}{Other arguments passed to `adonis2`.}
}
\value{
Table with pairwise factors, SS, pseudo-F, R^2^, p-value and
    adjusted p-value.
}
\description{
Wrapper function for pairwise multiple comparisons using
    `adonis2` from package `vegan`, and adjusted p-values using `p.adjust()`.
}
\examples{
# typical usage:
data(oakwoods, package='ecole')
spe <- oakwoods$spe
env <- oakwoods$env
D   <- vegan::vegdist(spe, 'bray')
table(env$thiltype)
permanova_pairwise(x = D, grp = env$thiltype)
# warning when any factor level contains singletons:
table(env$aspclass)
permanova_pairwise(x = D, grp = env$aspclass)

}
\seealso{
https://github.com/pmartinezarbizu/pairwiseAdonis
}
\author{
Rob Smith, inspired by Pedro Martinez Arbizu and Sylvain Monteux.
}
