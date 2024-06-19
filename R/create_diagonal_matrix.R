#' Create diagnoal matrix from vector
#'
#' This function creates a sparse diagonal matrix from a vector input.
#'
#' @param diagonal Diagonal elements of resulting matrix
#' @param number_of_rows Optional input that denotes the number of rows and columns.
#' @return A sparse diagonal matrix.
#' @export

create_diagonal_matrix <- function(diagonal, number_of_rows = NA) {
  # If dimension was provided, we check that dimension match
  if (!is.na(number_of_rows) & (length(diagonal) != number_of_rows & length(diagonal) != 1)) {
    stop("Some dimensions are off.")
  }

  # If no dimension was provided, the dimension is read from the input for diagonal
  if (is.na(number_of_rows)) number_of_rows <- length(diagonal)

  # Stop if data is not numerical
  if (any(!is.numeric(diagonal))) stop("The input vector is not numeric.")

  # Create the output matrix
  temporary_matrix <- Matrix::Matrix(0, nrow = number_of_rows, ncol = number_of_rows)
  diag(temporary_matrix) <- diagonal
  temporary_matrix %>%
    return()
}
