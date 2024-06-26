#' Select the xth element in a list
#'
#' Selects the xth element in list.
#'
#' @param this_list The list with elements.
#' @param x Integer for element number
#' @return A vector of data.
#' @export
#'
get_xth_elements_from_list_of_vectors <- function(this_list, x) {
  length_of_list <- length(this_list)
  out <- c(rep(NA, length_of_list))

  for (i in 1:length_of_list) {
    out[i] <- this_list[[i]][x]
  }

  return(out)
}
