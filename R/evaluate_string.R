evaluate_string <- function(x) {
    parse(text = paste(
        paste(
            "c(",
            collapse = ""
        ),
        paste(paste(x, c(rep(",", length(x) - 1), ")"), sep = ""), collapse = " "),
        collapse = " "
    )) %>%
        eval() %>%
        return()
}
