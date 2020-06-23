#' Distance
#'
#' @description calculation of geometric distance for three pairs of values describing two points
#'
#' @param x a value on axis 1, pairs with \code(k)
#' @param y a value on axis 2, pairs with \code(l)
#' @param z a value on axis 3, pairs with \code(m)
#' @param l a value on axis 1, pairs with \code(x)
#' @param k a value on axis 2, pairs with \code(y)
#' @param m a value on axis 3, pairs with \code(z)
#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @export


Distance <- function(x, y, z, k, l, m) {
  S <- sqrt((x-k)^2 + (y-l)^2 + (z-m)^2)
  return(S)
}
