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

sort_pedigree <- function(pedigree, maximum_number_of_generations = 30, add_generation_variable = F) {
  # Check that all animal have their own line
  new_animals <- pedigree %>%
    mutate(
      father_not_in_animal = !father %in% animal,
      mother_not_in_animal = !mother %in% animal
    ) %>%
    filter(father_not_in_animal | mother_not_in_animal) %>%
    select(father, mother, father_not_in_animal, mother_not_in_animal) %>%
    gather("role", "animal", father:mother) %>%
    filter(father_not_in_animal & role == "father" | mother_not_in_animal & role == "mother") %>%
    distinct(role, animal) %>%
    mutate(
      father = 0,
      mother = 0,
      sex = ifelse(role == "father", "male", "female")
    ) %>%
    select(-role)

  # Insert animals that do not

  all_animals <- pedigree %>%
    add_row(new_animals) %>%
    filter(animal != 0)

  # Sort the pedigree
  current_generation <- 1
  animals <- all_animals %$% animal
  fathers <- all_animals %$% father
  mothers <- all_animals %$% mother
  generations <- rep(1, length(animals))
  not_sorted <- rep(TRUE, length(animals))

  while (
    !(all(!not_sorted) |
      current_generation == maximum_number_of_generations)
  ) {
    not_sorted[not_sorted] <- ifelse(!(animals[not_sorted] %in% fathers[not_sorted] |
      animals[not_sorted] %in% mothers[not_sorted]), FALSE, TRUE)

    generations[not_sorted] <- generations[not_sorted] + 1

    current_generation <- current_generation + 1
  }

  last_generation <- max(generations)
  # Return animals56t
  if (add_generation_variable) {
    all_animals %>%
      mutate(generation = (last_generation:1)[generations]) %>%
      arrange(generation) %>%
      return()
  } else {
    all_animals %>%
      mutate(generation_placeholder = generations) %>%
      arrange(-generation_placeholder) %>%
      select(-generation_placeholder) %>%
      return()
  }
}
