#' Get Xth Legendre polynomial
#'
#' This function calculates the Xth Legendre polynomial
#'
#' @param x Covariate that should be transformed to a Legendre polynomial.
#' @param polynomial The Xth order of the polynomial.
#' @param normalized Don't really know what this means. Just be consistent.
#' @return A vector of real values
#' @export
#'
get_legendre_polynomial <- function(x, polynomial = 1, normalized = T) {
  x_max <- max(x, na.rm = T)
  x_min <- min(x, na.rm = T)
  x_scaled <- 2 * (x - x_min) / (x_max - x_min) - 1

  select_column <- paste("V", polynomial + 1, sep = "")

  orthopolynom::legendre.polynomials(n = polynomial, normalized = normalized) %>%
    polynomial.values(x = x_scaled) %>%
    as.data.frame(col.names = paste("V", 1:(polynomial + 1), sep = "")) %>%
    select(all_of(select_column)) %>%
    unlist() %>%
    unname() %>%
    return()
}
