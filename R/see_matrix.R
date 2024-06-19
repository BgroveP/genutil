#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
#'
see_matrix <- function(matrix, where = c("start"), number_of_rows = 10) {
    number_of_rows_in_matrix <- nrow(matrix)
    number_of_columns_in_matrix <- ncol(matrix)
    is_square <- number_of_rows_in_matrix == number_of_columns_in_matrix

    if (!(where %in% c("start", "middle", "end") | 1 <= where & where <= number_of_rows_in_matrix)) {
        stop("where has to be 'start', 'middle', or 'end'")
    }

    if (!is_square) {
        stop("The provided matrix is not square. This function only works for square matrices!")
    }

    if (number_of_rows_in_matrix <= number_of_rows) {
        matrix %>%
            return()
    }

    if (where == "start") first_row <- 1
    if (where == "middle") first_row <- round(number_of_rows_in_matrix / 2) 
    if (where == "end") first_row <- number_of_rows_in_matrix - (number_of_rows - 1)
    if (is.numeric(where)){
      first_row <- round(where- number_of_rows/2)
      if(first_row < 1) first_row <- 1
      if(first_row > (number_of_rows_in_matrix-number_of_rows+1)) first_row <- number_of_rows_in_matrix-number_of_rows+1
  
    } 
    last_row <- first_row + (number_of_rows - 1)
    matrix[first_row:last_row, first_row:last_row] %>%
        return()
}
