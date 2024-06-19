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

write_dmu_phenotypes <- function(data, file_path = "phenotypes", class_variables, phenotypes, continuous_variables) {

  all_variables <- c(class_variables, phenotypes, continuous_variables)
  information_path <- paste(file_path, "_information.csv", sep = "")
  dictionary_path <- paste(file_path, "_dictionary.csv", sep = "")

  file_path <- paste(file_path, ".csv", sep = "")

  translate_missing_class <- function(x) {
    ifelse(is.na(x), 0, x) %>%
      return()
  }
  translate_missing_continuous <- function(x) {
    ifelse(is.na(x), -9999, x) %>%
      return()
  }

  find_class_column_name_from_values <- function(x){
    for(column in class_variables){
      if(all(x == data %>% select(all_of(column)) %>% unlist)){
        return(column)
      }
    }
  }

  translate_class_strings <- function(x){

    append_file <- file.exists(dictionary_path) & difftime(Sys.time(), file.info("phenotypes_dictionary.csv")$atime, units = "secs") %>% as.numeric <= 10

    all_digits <- all(!grepl("\\D", x))
    has_too_large_variables <- any(nchar(x) >= 11)

    if(!all_digits | has_too_large_variables){

      column_name <- find_class_column_name_from_values(x)
      dictionary <- data.frame(variable = column_name, 
      old_classifier = x) %>% 
      distinct(variable, old_classifier) %>% 
      mutate(new_classifier = 1:n()) %T>% 
      fwrite(dictionary_path, append = append_file)

      data.frame(x) %>% 
      mutate(new = dictionary$new_classifier[match(x, dictionary$old_classifier)]) %$% 
      return(new)



    }else{
      return(x)
    }
  }
  
  data.frame(
    variable = c(class_variables, phenotypes, continuous_variables),
    type = c(
      rep("class", length(class_variables)),
      rep("continuous", length(c(phenotypes, continuous_variables)))
    )
  ) %>% 
  fwrite(information_path)

data %>%
    select(all_of(all_variables)) %>%
    mutate_at(class_variables, translate_class_strings) %>%
    mutate_at(class_variables, translate_missing_class) %>%
    mutate_at(c(phenotypes, continuous_variables), translate_missing_continuous)  %>% 
    fwrite(file_path, col.names = F)

cat("\n\n\n")
cat("Summary statistics for class variables:\n")
data %>%
    select(all_of(class_variables)) %>%
    gather("variable", "input") %>% 
    group_by(variable, input) %>%
    summarize(N = sum(!is.na(input)),
          .groups = "drop") %>% 
    group_by(variable) %>%  
    summarize(levels = length(unique(input)),
          fewest_records = min(N),
          most_records = max(N),
          .groups = "drop") %>% 
    print()
cat("\n\n\n")

cat("Summary statistics for continuous variables:\n")
data %>%
    select(all_of(c(phenotypes, continuous_variables))) %>%
    gather("variable", "input") %>% 
    group_by(variable) %>%  
    summarize(number_of_records = sum(!is.na(input)),
    average_records = mean(input, na.rm = T),
    smallest_record = min(input, na.rm = T),
          largest_record= max(input, na.rm = T),
          standard_deviation = sd(input, na.rm = T),
          .groups = "drop") %>% 
    print()
cat("\n\n\n")



# udskriv fil til DMU-DIR-fil
	number_of_class_variables <- c(class_variables) 
	number_of_continous_variables <- c(phenotypes,continuous_variables)

  faq_output <- c(
	paste("$DATA ASCII (",length(number_of_class_variables),",",length(number_of_continous_variables),",-9999",") ",file_path,"\n \n",sep=""),
	"$VARIABLE \n",
	format (c("#1",2:length(number_of_class_variables)),width=max(nchar(all_variables))+5),
  "\n", 
	format (c(number_of_class_variables),width=max(nchar(all_variables))+5),
  "\n", 
	format (c("#1",2:length(number_of_continous_variables)),width=max(nchar(all_variables))+5),
  "\n", 
	format (c(number_of_continous_variables),width=max(nchar(all_variables))+5),
  "\n"
  ) %>% 
  paste(collapse = "")

  data.frame(x = faq_output) %>% 
  fwrite(information_path, col.names = F, quote = F)

}
