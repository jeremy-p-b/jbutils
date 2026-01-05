#' Calculate lower bound of Wilson CI for binomial proportion
#'
#' @param num Vector number of events.
#' @param denom Vector size of population.
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' wilson_lower(15,80)
wilson_lower <- function(num, denom) {
  return(mapply(function(num, denom) epitools::binom.wilson(num, denom)[1,"lower"], as.numeric(num), as.numeric(denom)))
}


#' Calculate upper bound of Wilson CI for binomial proportion
#'
#' @param num Vector number of events.
#' @param denom Vector size of population.
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' wilson_upper(15,80)
wilson_upper <- function(num, denom) {
  return(mapply(function(num, denom) epitools::binom.wilson(num, denom)["upper"][1,1], as.numeric(num), as.numeric(denom)))
}

#' Calculate string confidence interval for wilson CI
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
#' wilson_ci(15,80, accuracy=0.01)
wilson_ci <- function(num, denom, accuracy=0.1, ci_only=FALSE, per=1) {
  estimated_mean <- (num/denom) * per
  estimated_lb <- wilson_lower(num, denom) * per
  estimated_ub <- wilson_upper(num, denom) * per
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




