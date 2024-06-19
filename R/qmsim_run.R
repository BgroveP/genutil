get_random_string_of_letters <- function(
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

qmsim_simulate <- function(
    heritability = 0.2,
    heritability_from_qtl = 0.2,
    phenotypic_variance = 1,
    historical_population_generations = 3000,
    historical_population_sizes = 1000,
    number_of_replicates = 2,
    number_of_fathers_per_generation = 50,
    number_of_mothers_per_generation = 50,
    littersize = 4,
    number_of_generations = 4,
    number_of_chromosomes = 5,
    number_of_markers_per_chromosome = 100,
    number_of_qtl_per_chromosome = 10,
    qmsim_path = "/usr/home/qgg/bgrovep/Software/QMSim/QMSim",
    output_path = "") {

}
