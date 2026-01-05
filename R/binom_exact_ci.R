#' Calculate lower bound of exact CI for binomial proportion
#'
#' @param num Vector number of events.
#' @param denom Vector size of population.
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' binom_exact_lower(15,80)
binom_exact_lower <- function(num, denom) {
  return(mapply(function(num, denom) epitools::binom.exact(num, denom)["lower"][1,1], num, denom))
}

#' Calculate upper bound of exact CI for binomial proportion
#'
#' @param num Vector number of events.
#' @param denom Vector size of population.
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' binom_exact_upper(15,80)
binom_exact_upper <- function(num, denom) {
  return(mapply(function(num, denom) epitools::binom.exact(num, denom)["upper"][1,1], num, denom))
}


#' Calculate string confidence interval for binom exact CI
#'
#' @param num Vector number of events.
#' @param denom Vector size of population.
#' @param accuracy Accuracy of rounding
#' @param ci_only Boolean of whether to report CI only without mean
#' @param per counts per x
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' binom_exact_ci(15,80, accuracy=0.01)
binom_exact_ci <- function(num, denom, accuracy=0.1, ci_only=FALSE, per=1) {
  estimated_mean <- (num/denom) * per
  estimated_lb <- binom_exact_lower(num, denom) * per
  estimated_ub <- binom_exact_upper(num, denom) * per
  if (ci_only) {
    ci_string <- paste0(scales::number(estimated_lb, accuracy=accuracy), "-",
                        scales::number(estimated_ub, accuracy=accuracy))
  } else {
    ci_string <- paste0(scales::number(estimated_mean, accuracy=accuracy)," (",
                        scales::number(estimated_lb, accuracy=accuracy), "-",
                        scales::number(estimated_ub, accuracy=accuracy),")")
  }
  return(ci_string)
}
