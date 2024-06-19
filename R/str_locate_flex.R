#' Get x'th position of pattern in string from start or end
#'
#' This function can take a vector of strings and return the position of xth match for a pattern.
#'
#' @param string String, or vector of strings
#' @return A vector of integers
#' @export

str_locate_flex <- function(string, pattern, x = 1, reference_point = "start") {
  legal_reference_points <- c("start", "end")
  if (!reference_point %in% legal_reference_points) stop("Not valid reference point.")
  if (!x %% 1 == 0 & x > 0) stop("Not valid x.")

  if (reference_point == "start") {
    tmp_string <- str_locate_all(string, pattern) %>%
      lapply(head, n = x) %>%
      lapply(tail, n = 1) %>%
      unlist()
  }
  if (reference_point == "end") {
    tmp_string <- str_locate_all(string, pattern) %>%
      lapply(tail, n = x) %>%
      lapply(head, n = 1) %>%
      unlist()
  }

  tmp_string[seq(1, length(tmp_string), 2)] %>%
    return()
}
