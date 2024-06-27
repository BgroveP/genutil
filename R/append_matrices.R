#' Appends two matrices
#'
#' This function initializes a sparse and empty matrix
#'
#' @param ... Any number of matrices or scalars with numeric values
#' @return A matrix
#' @export
#'
#'


test <- function(...) {
    list_view <- list(...)
    list_view %<>% lapply(as.matrix)

    number_of_rows <- lapply(list_view, nrow) %>% unlist()
    number_of_columns <- lapply(list_view, ncol) %>% unlist()

    out <- Matrix(0, sum(number_of_rows), sum(number_of_columns))
    row_iterator <- 0
    column_iterator <- 0

    for (i in 1:length(list_view)) {
        out[
            1:number_of_rows[i] + row_iterator,
            1:number_of_columns[i] + column_iterator
        ] <- list_view[[i]]
        row_iterator <- row_iterator + number_of_rows[i]
        column_iterator <- column_iterator + number_of_columns[i]
    }
    return(out)
}
