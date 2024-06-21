#' Get last position of pattern in string
#'
#' This function can take a vector of strings and return the position of last match for a pattern.
#'
#' @param string String, or vector of strings
#' @return A vector of integers
#' @export

str_locate_last <- function(string, pattern) {
  tmp_string <- str_locate_all(string, pattern) %>%
    lapply(tail, n = 1) %>%
    unlist()

  tmp_string[seq(1, length(tmp_string), 2)] %>%
    return()
}