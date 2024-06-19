#' Create an identity matrix
#'
#' This function initializes an identity matrix
#'
#' @param number_of_rows    Number of rows
#' @return A sparse identity matrix
#' @export

create_identity_matrix <- function(number_of_rows) {
  # Create the output matrix
  temporary_matrix <- Matrix::Matrix(0, nrow = number_of_rows, ncol = number_of_rows)
  diag(temporary_matrix) <- 1
  temporary_matrix %>%
    return()
}
