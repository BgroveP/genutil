#' Create a square matrix from vector input
#'
#' This function initializes a square matrix from vector input.
#' The vector input is assumed to contain triangular data.
#'
#' @param x    The vector data
#' @return A square matrix
#' @export
create_square_matrix_from_vector <- function(x) {
  length_of_vector <- length(x)
  number_of_rows <- floor(sqrt(2 * length_of_vector))


  # Stop if length of vector does not match a square matrix size
  estimated_vector_length <- number_of_rows * (1 + number_of_rows) / 2
  if (length_of_vector != estimated_vector_length) stop("The length of the vector does not match a square matrix.")
  out <- matrix(
    NA,
    number_of_rows,
    number_of_rows
  )

  iterator <- 1

  for (i in 1:number_of_rows) {
    for (j in 1:i) {
      out[i, j] <- out[j, i] <- x[iterator]
      iterator <- iterator + 1
    }
  }
  return(out)
}
