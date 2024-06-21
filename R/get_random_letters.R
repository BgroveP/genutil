get_random_letters <- function(
    number_of_letters = 10,
    with_date = FALSE) {
    # number must be integer between 0 and 50
    if (!is.numeric(number_of_letters)) stop("Number of letters is not numeric.")
    if (number_of_letters < 1) stop("Number of letters is too small. Minimum value is 1.")
    if (number_of_letters >= 50) stop("Number of letters is too large. Maximum value is 50.")
    if (!is.logical(with_date)) stop("'with date' variable is not logical.")

    letters %>%
        sample(number_of_letters, replace = T) %>%
        paste(collapse = "") %>%
        paste(
            ifelse(with_date,
                paste("_", gsub("-", "", Sys.Date()), sep = ""),
                ""
            ),
            sep = ""
        ) %>%
        return()
}
