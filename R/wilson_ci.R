#' Calculate lower bound of Wilson CI for binomial proportion
#'
#' @param num Scalar number of events.
#' @param denom
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' wilson_lower(15,80)
wilson_lower <- function(num, denom) {
  return(mapply(function(num, denom) epitools::binom.wilson(num, denom)["lower"][1,1], num, denom))
}

#' Calculate upper bound of Wilson CI for binomial proportion
#'
#' @param num Scalar number of events.
#' @param denom Scalar size of population.
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' wilson_upper(15,80)
wilson_upper <- function(num, denom) {
  return(mapply(function(num, denom) epitools::binom.wilson(num, denom)["upper"][1,1], num, denom))
}
