#' Create an empty matrix
#'
#' This function initializes a sparse and empty matrix
#'
#' @param number_of_rows    Number of rows
#' @param number_of_columns Optional number of columns. If omitted, this is assumed to equal to the number of rows.
#' @param fill              Optional fill of the empty matrix. The standard is zeros, because this creates a sparse matrix.
#' @return A sparse and empty matrix
#' @export
#'
#'


create_empty_matrix <- function(number_of_rows, number_of_columns = NA, fill = 0) {
  # If number of columns is not provided, the output is a square matrix
  if (is.na(number_of_columns)) number_of_columns <- number_of_rows

  # Create the output matrix
  Matrix::Matrix(data = fill, nrow = number_of_rows, ncol = number_of_columns) %>%
    return()
}
