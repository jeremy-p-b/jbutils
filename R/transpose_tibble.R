#' Transpose a tibble
#'
#' @param tibble_data Tibble or data frame with data
#' @param new_colname String new column name
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' animals <- data.frame(animal=c("horse", "cheetah"), max_speed=c(44, 75), max_height=c(7, 4))
#' transpose_tibble(animals, "Characteristic")
transpose_tibble <- function(tibble_data, new_colname="name") {
  colnames_tibble_data <- colnames(tibble_data)
  tibble_data <- tibble_data %>% tidyr::pivot_longer(colnames_tibble_data[2:length(colnames_tibble_data)], names_to=new_colname) %>%
    tidyr::pivot_wider(id_cols=dplyr::all_of(new_colname), names_from=colnames_tibble_data[1])
  return(tibble_data)
}
