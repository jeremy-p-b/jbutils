#' Get formatted current date
#'
#'
#' @return A scalar string with currrent date (e.g., 20241231)
#' @export
#'
#' @examples
#' get_formatted_date()
get_formatted_date <- function() {
  formatted_date <- format(Sys.Date(), "%Y%m%d")
  return(formatted_date)
}
