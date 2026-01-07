#' Calculate fixed effects pooled risk ratio (following Borenstein 2009)
#'
#' @param s1x1 number of exposed cases in study 1
#' @param s1n1 number exposed in study 1
#' @param s1x2 number of unexposed cases in study 1
#' @param s1n2 number unexposed in study 1
#' @param s2x1 number of exposed cases in study 2
#' @param s2n1 number exposed in study 2
#' @param s2x2 number of unexposed cases in study 2
#' @param s2n2 number unexposed in study 2
#' @param pretty logical indicator of whether to output a formatted string or numerical vector (default=TRUE)
#'
#' @return A numerical vector with estimate, lower bound, and upper bound or a character representation of the vector
#' @export
#'
#' @examples
#' calculate_fe_pooled_rr(38,398,37,514,29,314,18,215)
calculate_fe_pooled_rr <- function(s1x1, s1n1, s1x2, s1n2, s2x1, s2n1, s2x2, s2n2, pretty=TRUE) {
  var1 <- var_log_rr(s1x1, s1n1, s1x2, s1n1)
  var2 <- var_log_rr(s2x1, s2n1, s2x2, s2n2)
  log_rr1 <- ((s1x1/s1n1)/(s1x2/s1n2)) %>% log()
  log_rr2 <- ((s2x1/s2n1)/(s2x2/s2n2)) %>% log()
  sum_weights = (1/var1 + 1/var2)
  pooled_var <- 1/sum_weights
  pooled_log_rr <- (log_rr1/var1 + log_rr2/var2)/sum_weights
  lb <- pooled_log_rr - 1.96*sqrt(pooled_var)
  ub <- pooled_log_rr + 1.96*sqrt(pooled_var)
  if (pretty) {
    return(glue::glue("{scales::number(exp(pooled_log_rr), accuracy=0.01)} ({scales::number(exp(lb), accuracy=0.01)}-{scales::number(exp(ub), accuracy=0.01)})"))
  } else {
    return(c(pooled_log_rr, lb, ub))
  }
}


