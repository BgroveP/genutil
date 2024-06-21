#' Create diagnoal matrix from vector
#'
#' This function creates a sparse diagonal matrix from a vector input.
#'
#' @param diagonal Diagonal elements of resulting matrix
#' @param number_of_rows Optional input that denotes the number of rows and columns.
#' @return A sparse diagonal matrix.
#' @export

create_diagonal_matrix <- function(diagonal) {
  # The dimension is read from the input for diagonal
  number_of_rows <- length(diagonal)

  # Stop if data is not numerical
  if (any(!is.numeric(diagonal))) stop("The input vector is not numeric.")

  # Stop if the vector was empty
  if (!number_of_rows >= 1) stop("The input vector is empty.")

  # Create the output matrix
  temporary_matrix <- Matrix::Matrix(0, nrow = number_of_rows, ncol = number_of_rows)
  diag(temporary_matrix) <- diagonal
  temporary_matrix %>%
    return()
}
