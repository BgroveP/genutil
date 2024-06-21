#' Sample a correlation matrix
#'
#' This function samples a correlation matrix of a given size.
#' The function progressively, samples increasingly big partial correlation matrices,
#' and only continues when the partial correlation matrix is positive-definite.
#'
#' @param size Integer that denotes the size of the correlation matrix.
#' @return A standard matrix object.
#' @export
#'

sample_correlation_matrix <- function(size = 1, density_function = "uniform") {
  if (abs(size %% 1) >= 0.0000001) stop("Size is not an integer")

  this_matrix <- matrix(1, size, size)

  if (size > 1) {
    for (this_size in 2:size) {
      this_matrix_has_full_rank <- F

      while (!this_matrix_has_full_rank) {
        this_matrix[1:(this_size - 1), this_size] <-
          this_matrix[this_size, 1:(this_size - 1)] <-
          ifelse(density_function == "uniform",
            runif(this_size - 1, min = -1, max = 1),
            tanh(rnorm(1, 0, 1))
          )
        if (det(this_matrix[1:this_size, 1:this_size]) > 0) this_matrix_has_full_rank <- T
      }
    }
  }

  this_matrix %>%
    return()
}
