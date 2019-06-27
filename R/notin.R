#' @title Values NOT matching
#'
#' @description
#' Returns a logical vector indicating if there is NOT a match for its
#'      left operand.
#'
#' @param x
#' vector or NULL: the values to be matched.
#'
#' @param y
#' vector or NULL: the values to be matched against.
#'
#' @return
#' A logical vector of the same length as \code{x}, indicating if a
#'      match was NOT located for each element of \code{x}: thus the
#'      values are TRUE or FALSE and never NA.
#'
#' @details
#' Inverse of the commonly used \code{\link[base]{match}}.
#'
#' @examples
#' a <- 1:5
#' b <- 4:9
#' a %notin% b
#'
#' @seealso \code{\link[base]{match}}.
#'
#' @export
`%notin%` <- function(x, y) { !(x %in% y) }
