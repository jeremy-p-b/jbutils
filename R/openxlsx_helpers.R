#' Set options for workbook
#'
#' @param wb Workbook object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_wb_options(wb)
#' }
set_wb_options <- function(wb) {
  options("openxlsx.borderColour" = "#4F80BD")
  options("openxlsx.borderStyle" = "thin")
  openxlsx::modifyBaseFont(wb, fontSize = 12, fontName = "Arial")
}


#' Insert Image with a description
#'
#' @param ... Arguments to insertImage function
#' @param description A string with the image description
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' \dontrun{
#' InsertDescriptiveImage(wb, 1, "output/myimage.jpeg")
#' }
insertDescriptiveImage <- function(..., description) {
  boldStyle <- openxlsx::createStyle(textDecoration = "bold")
  params <- list(...)
  row_no <- 1

  openxlsx::addStyle(params[[1]], sheet=params$sheet, style = boldStyle, rows = row_no, cols=1)
  openxlsx::writeData(params[[1]], sheet=params$sheet, description, startRow=row_no)
  row_no <- row_no + 1

  openxlsx::insertImage(..., startRow = row_no)
}


#' Write formattedDataTable with a description
#'
#' @param ... Arguments to WriteDataTable function
#' @param description A string with the table description
#'
#' @return A numerical vector.
#' @export
#'
#' @examples
#' \dontrun{
#' writeDescriptiveDataTableAuto(wb, 1, df)
#' }
writeDescriptiveDataTableAuto <- function(..., description) {
  boldStyle <- openxlsx::createStyle(textDecoration = "bold")
  params <- list(...)
  row_no <- 1

  openxlsx::addStyle(params[[1]], sheet=params$sheet, style = boldStyle, rows = row_no, cols=1)
  openxlsx::writeData(params[[1]], sheet=params$sheet, description, startRow=row_no)
  row_no <- row_no + 1

  openxlsx::writeDataTable(..., tableStyle = "TableStyleLight9", startRow=row_no)

  # Calculate column widths excluding the first row
  # Get the data
  data <- params$x

  # Function to calculate width for a vector, excluding first element
  calculateWidth <- function(colname, x) {
    # Convert all values to character
    x_char <- as.character(x)  # Exclude first element
    # Calculate max width, with a minimum of 8 characters
    max(8, max(c(nchar(x_char), nchar(colname)), na.rm = TRUE) + 2)
  }

  # Calculate widths for each column
  widths <- mapply(calculateWidth, colnames(data), data)

  # Apply the calculated widths
  openxlsx::setColWidths(params[[1]], params$sheet, 1:ncol(params$x), widths=widths)
}

#' Write formatted DataTable
#'
#' @param ... Arguments to WriteDataTable function
#'
#' @export
#'
#' @examples
#' \dontrun{
#' writeDataTableAuto(wb, 1, df)
#' }
writeDataTableAuto <- function(...) {
  openxlsx::writeDataTable(..., tableStyle = "TableStyleLight9")
  params <- list(...)
  openxlsx::setColWidths(params[[1]], params$sheet, 1:ncol(params$x), widths="auto")
}


#'  Convert vector to logical if a binary number (useful for preparing for tbl_summary)
#'
#' @param x A vector
#'
#' @export
#'
#' @examples
#' \dontrun{
#' convert_numeric_to_binary(c(1,2,3))
#' }
convert_numeric_to_binary <- function(x) {
  if (inherits(x, c("integer", "numeric")) &&
      all(unique(stats::na.omit(x)) %in% c(0, 1))) {
    y <- as.logical(x)
  } else {
    y <- x
  }
  Hmisc::label(y) <- Hmisc::label(x)
  return(y)
}
