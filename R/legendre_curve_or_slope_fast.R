#' Calculate curve or slope for legendre polynomials of any order
#'
#'
#' @param ... Numeric vectors of the linear covariate and the regression coefficients
#' @param output A variable that denotes whether the output should be the curve of the slope of the curve.
#' @param debug Denotes whether additional output should be printed.
#' @return A numeric vector of curves or slopes.
#' @export
#'

legendre_curve_or_slope_fast <- function(
    ...,
    output = "curve",
    debug = FALSE) {
    # Derive information from input
    list_of_input <- list(...)
    length_of_input <- list_of_input %>%
        length()
    lengths_of_inputs <- lapply(list_of_input, length) %>%
        unique() %>%
        unlist()
    covariate_vector_indice <- 1
    coefficient_vector_indices <- 2:length_of_input
    number_of_orders <- length_of_input - 2

    # Vectors must be provided
    if (length_of_input == 0) stop("No input.")

    # All list elements must be a numeric vector
    for (i in 1:length_of_input) {
        if (!is.vector(list_of_input[[i]])) stop(paste("Input", i, "is not a vector."))
        if (any(!is.numeric(list_of_input[[i]]))) stop(paste("Input", i, "is not numeric."))
    }
    if (!(length(lengths_of_inputs) == 1 | length(lengths_of_inputs) > 1 & min(lengths_of_inputs) == 1)) {
        stop("The input vectors are not the same size")
    }

    # Debug information
    if (debug) {
        paste("Length of input list:", length_of_input) %>% print()
        paste("Coefficient indices:", coefficient_vector_indices) %>% print()
        paste("number of orders:", number_of_orders) %>% print()
    }

    # Initialize the output
    return_object <- list_of_input[[covariate_vector_indice]] %>%
        length() %>%
        numeric()
    temporary_input <- data.frame(
        x = list_of_input[[covariate_vector_indice]],
        out = 0
    ) %>%
        distinct(x, out) %>%
        mutate(x_encapsulated = paste("(", x, ")", sep = ""))

    for (this_order in 0:number_of_orders) {
        this_equation <- lapply(
            legendre.polynomials(this_order, normalized = FALSE),
            as.character
        )[this_order + 1] %>%
            unlist()

        if (output == "slope" & this_order == 0) this_equation <- "0*x"
        if (output == "slope" & this_order > 0) {
            this_equation <- this_equation %>%
                Deriv(cache.exp = F)
        }
        if (debug) print(this_equation)

        # output the return object
        temporary_input %<>%
            mutate(out = str_replace_all(
                this_equation,
                "x",
                x_encapsulated
            ) %>% evaluate_string())

        return_object <- return_object +
            list_of_input[[2 + this_order]] * temporary_input$out[match(
                list_of_input[[covariate_vector_indice]],
                temporary_input$x
            )]
    }
    return_object %>%
        return()
}




list_of_input <- data.frame(x = sample(0:314, 10000000, TRUE)) %>%
    mutate(
        x = (x - 314 / 2) / (314 / 2),
        b0 = rnorm(n()),
        b1 = rnorm(n()),
        b2 = rnorm(n()),
        b3 = rnorm(n()),
        b4 = rnorm(n())
    )

time1 <- Sys.time()
tmp <- list_of_input %>%
    mutate(
        method1 = legendre_curve_or_slope(x, b0, b1, b2, b3, b4)
    )
time2 <- Sys.time()
tmp <- list_of_input %>%
    mutate(
        method1 = legendre_curve_or_slope_fast(x, b0, b1, b2, b3, b4)
    )
time3 <- Sys.time()


time2 - time1
time3 - time2
