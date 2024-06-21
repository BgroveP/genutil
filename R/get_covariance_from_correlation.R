#' Calculate covariance matrix
#'
#' This function initializes a sparse and empty matrix
#'
#' @param correlation_matrix The correlation matrix
#' @param variances Vector of diagonal elements of the resulting covariance matrix
#' @return A covariance matrix
#' @export
get_covariance_from_correlation <- function(correlation_matrix, variances) {
  create_diagonal_matrix(sqrt(variances)) %*% correlation_matrix %*% create_diagonal_matrix(sqrt(variances)) %>% return()
}
