#' Get random-regression output from DMU analysis
#'
#' This function calculates prediction (incl. breeding values) and variances per covariate level in the phenotype file.
#'
#' @param dmu_analysis  Relative path to the DMU analysis.
#' @return A list of output.
#' @export


# Suggestion: Say actual trait instead of trait number.

dmu_calculate_breeding_values_with_random_regression <- function(
    dmu_analysis,
    covariance_matrix_names = NA) {
  dir_path <- paste(dmu_analysis, ".DIR", sep = "")
  parout_path <- paste(dmu_analysis, ".PAROUT", sep = "")
  solution_path <- paste(dmu_analysis, ".SOL", sep = "")
  output <- list()
  output[["Transformation matrix"]] <- list()
  output[["Covariance matrix"]] <- list()
  output[["Predicted effects"]] <- list()

  # Check that the files exist
  if (!file.exists(dir_path)) stop(paste("Cant find the .DIR file:", dir_path))
  # if (!file.exists(solution_path)) stop(paste("Cant find the .SOL file:", solution_path))
  if (!file.exists(solution_path)) message(paste("Cant find the .SOL file:", solution_path))

  # Get .DIR information
  dir <- data.frame(line = scan(dir_path, what = character(), sep = "\n", quiet = T)) %>%
    filter(!substr(line, 1, 1) == "#") %>%
    mutate(line_no_ws = gsub("[[:space:]]", "", line))

  # Get analysis type
  analysis_line <- dir %>%
    filter(grepl("\\$ANALYSE", line)) %>%
    mutate(line = gsub("[[:space:]]", " ", line)) %$%
    strsplit(line, split = " ") %>%
    unlist()

  if (analysis_line[2] == "1") analysis_type <- "estimation of variance components"
  if (analysis_line[2] %in% c("11", "12")) analysis_type <- "prediction of breeding values"
  if (!analysis_line[2] %in% c("1", "11", "12")) analysis_type <- stop("Weird analysis type? Did this job finish with DMU?")

  #
  if (analysis_type == "prediction of breeding values") {
    parout_relative_path <- (analysis_line <- dir %>%
      filter(grepl("\\$PRIOR", line)) %>%
      mutate(line = gsub("[[:space:]]", " ", line)) %$%
      strsplit(line, split = " ") %>%
      unlist())[2]

    if (grepl("\\/", parout_relative_path) & !substr(parout_relative_path, 1, 1) == "/") {
      full_dmu_path <- paste(getwd(), dmu_analysis, sep = "/")
      last_dash_position <- str_locate_all(full_dmu_path, "\\/") %>%
        as.data.frame() %>%
        select(start) %>%
        unlist() %>%
        max()
      string_to_replace <- substr(full_dmu_path, last_dash_position + 1, nchar(full_dmu_path))
      parout_path <- gsub(string_to_replace, parout_relative_path, dmu_analysis)
    } else {
      parout_path <- parout_relative_path
    }
  }
  if (!file.exists(parout_path)) stop(paste("Cant find the .PAROUT file:", parout_path))
  # Get data information
  data_line <- dir %>%
    filter(grepl("\\$DATA", line)) %>%
    mutate(line = gsub("[[:space:]]", " ", line)) %$%
    strsplit(line, split = " ") %>%
    unlist()

  number_of_variable_types <- gsub("\\)", "", gsub("\\(", "", data_line[3])) %>%
    str_split(pattern = ",") %>%
    unlist() %>%
    as.numeric()
  number_of_class_variables_in_phenotype_file <- number_of_variable_types[1]
  number_of_continuous_variables_in_phenotype_file <- number_of_variable_types[2]
  #
  dir_number_of_submodels_line <- which("$MODEL" == dir$line_no_ws) + 1
  number_of_submodels <- dir$line[dir_number_of_submodels_line] %>% as.numeric()
  dir_phenotype_line <- which(grepl("\\$DATAASCII\\(", dir$line_no_ws))

  dir_class_variable_line <- which(dir$line_no_ws == "$VARIABLE") + 1
  dir_continuous_variable_line <- dir_class_variable_line + 1


  dir_model_class_lines <- dir_number_of_submodels_line + number_of_submodels + 1:number_of_submodels
  dir_model_random_lines <- max(dir_model_class_lines) + 1:number_of_submodels
  dir_model_continuous_lines <- max(dir_model_random_lines) + 1:number_of_submodels

  number_of_class_effects <- dir %$%
    strsplit(line[dir_model_class_lines], split = " ") %>%
    get_xth_elements_from_list_of_vectors(3) %>%
    as.numeric()

  number_of_random_effects <- dir %$%
    strsplit(line[dir_model_random_lines], split = " ") %>%
    get_xth_elements_from_list_of_vectors(1) %>%
    as.numeric()

  number_of_continuos_effects <- dir %$%
    strsplit(line[dir_model_continuous_lines], split = " ") %>%
    get_xth_elements_from_list_of_vectors(1) %>%
    as.numeric()
  # Interpret .DIR information
  ## Phenotype information
  phenotype_header <- gsub(
    "^ ", "",
    gsub(
      " $", "",
      gsub(
        "[[:space:]]+", " ",
        dir$line[dir_class_variable_line:(dir_number_of_submodels_line - 2)]
      )
    )
  ) %>%
    paste(collapse = " ") %>%
    str_split(patter = " ") %>%
    unlist()
  phenotype_header <- phenotype_header[which(nchar(phenotype_header) > 0)]
  phenotype_header_class <- phenotype_header[1:number_of_class_variables_in_phenotype_file]
  phenotype_header_continuous <- phenotype_header[number_of_class_variables_in_phenotype_file + 1:number_of_continuous_variables_in_phenotype_file]

  phenotype_relative_path <- gsub(".*\\)", "", dir$line_no_ws[dir_phenotype_line])
  if (grepl("\\/", dmu_analysis) & !substr(phenotype_relative_path, 1, 1) == "/") {
    last_dash_position <- str_locate_all(dmu_analysis, "\\/") %>%
      as.data.frame() %>%
      select(start) %>%
      unlist() %>%
      max()
    string_to_replace <- substr(dmu_analysis, last_dash_position + 1, nchar(dmu_analysis))
    phenotype_path <- gsub(string_to_replace, phenotype_relative_path, dmu_analysis)
  } else {
    phenotype_path <- phenotype_relative_path
  }

  covariance_components_threecolumn <- fread(parout_path, fill = T) %>%
    rename_at(
      paste("V", 1:4, sep = ""),
      ~ c("covariance_matrix", "row", "column", "estimate")
    ) %>%
    group_by(covariance_matrix, row, column) %>%
    summarize(estimate = mean(estimate, na.rm = T), .groups = "drop") %>%
    ungroup()

  number_of_covariance_matrices <- covariance_components_threecolumn %>%
    select(covariance_matrix) %>%
    unlist() %>%
    max()

  if (any(!is.na(covariance_matrix_names))) {
    covariance_matrix_names <- paste("Number:", 1:number_of_covariance_matrices)
  } else if (number_of_covariance_matrices == length(unique(covariance_matrix_names))) {

  } else {
    covariance_matrix_names <- paste("Number:", 1:number_of_covariance_matrices)
  }

  covariance_matrix_sizes <- covariance_components_threecolumn %>%
    group_by(covariance_matrix) %>%
    summarize(size = max(row)) %>%
    select(size) %>%
    unlist()

  model_random <- data.frame(x = dir$line[dir_model_random_lines]) %>%
    mutate(
      first_blank = str_locate(x, "[:blank:]+")[, 2],
      newline = substr(x, first_blank + 1, nchar(x))
    )

  # For each covariance matrix...
  this_covariance_matrix <- 1
  this_submodel <- 1
  for (this_covariance_matrix in 1:number_of_covariance_matrices) {
    this_transformation_names <- character()
    this_transformation_traits <- numeric()

    for (this_submodel in 1:number_of_submodels) {
      ## Present in random_vector
      covariance_matrix_in_random_line <- FALSE
      potential_covariance_matrix_instances <- c(
        paste(" ", this_covariance_matrix, " ", sep = ""),
        paste("^", this_covariance_matrix, " ", sep = ""),
        paste(" ", this_covariance_matrix, "$", sep = ""),
        paste("^", this_covariance_matrix, "$", sep = "")
      )
      for (i in 1:length(potential_covariance_matrix_instances)) {
        if (

          grepl(
            potential_covariance_matrix_instances[i],
            model_random$newline[this_submodel]
          )
        ) {
          covariance_matrix_in_random_line <- TRUE
        }
      }

      if (covariance_matrix_in_random_line) {
        this_transformation_names <- c(this_transformation_names, "internal_intercept")
        this_transformation_traits <- c(this_transformation_traits, this_submodel)

        # Check if also nested regression
        this_continuous_line <- dir$line[dir_model_continuous_lines][this_submodel]

        this_random_effect_number <- which(strsplit(gsub("[ ]+", " ", model_random$newline[this_submodel]), split = " ") %>% unlist() == as.character(this_covariance_matrix))
        this_class_effect_number <- number_of_class_effects[this_submodel] - number_of_random_effects[this_submodel] + this_random_effect_number
        potential_class_effect_references <- c(
          paste(" ", this_class_effect_number, " ", sep = ""),
          paste("^", this_class_effect_number, " ", sep = ""),
          paste(" ", this_class_effect_number, "$", sep = "")
        )
        this_continuous_vector <- data.frame(
          regressor_start = str_locate_all(this_continuous_line, "[:blank:][0-9]*\\(")[[1]][, 1] + 1,
          regressor_end = str_locate_all(this_continuous_line, "[:blank:][0-9]*\\(")[[1]][, 2] - 1,
          insides_start = str_locate_all(this_continuous_line, "\\([0-9 ]+\\)")[[1]][, 1] + 1,
          insides_end = str_locate_all(this_continuous_line, "\\([0-9 ]+\\)")[[1]][, 2] - 1
        ) %>%
          group_by(regressor_start) %>%
          mutate(
            regressor = substr(this_continuous_line, regressor_start, regressor_end),
            inside_parentheses = substr(this_continuous_line, insides_start, insides_end)
          )

        for (i in 1:length(potential_class_effect_references)) {
          nesting_is_present <- grepl(potential_class_effect_references[i], this_continuous_vector$inside_parentheses)

          if (any(nesting_is_present)) {
            which_nesting_is_present <- phenotype_header_continuous[this_continuous_vector$regressor[nesting_is_present] %>% as.numeric()]
            this_transformation_names <- c(this_transformation_names, which_nesting_is_present)
            this_transformation_traits <- c(this_transformation_traits, rep(this_submodel, length(which_nesting_is_present)))
          }
        }
      }
    }
    if (this_covariance_matrix == number_of_covariance_matrices) {
      this_class_effect_number <- 0
      this_transformation_names <- rep(c("internal_intercept"), covariance_matrix_sizes[this_covariance_matrix])
      this_transformation_traits <- 1:number_of_submodels
    }

    # Read phenotypes to get transformation matrix
    covariates_from_phenotypes <- fread(phenotype_path) %>%
      rename_all(function(x) {
        phenotype_header
      }) %>%
      mutate(internal_intercept = 1) %>%
      select(all_of(this_transformation_names)) %>%
      distinct() %>%
      ungroup() %>%
      arrange(across(everything())) %>%
      data.matrix()

    this_transformation_matrix_sum <- covariates_from_phenotypes[, this_transformation_names]
    if (is.null(nrow(this_transformation_matrix_sum))) {
      this_transformation_matrix_sum <- this_transformation_matrix_sum %>%
        as.matrix() %>%
        t()
    }

    # Add rows that are missing
    this_observed_number_of_covariances <- covariance_components_threecolumn %>%
      filter(covariance_matrix == this_covariance_matrix) %>%
      nrow()

    this_number_of_variances <- covariance_components_threecolumn %>%
      filter(covariance_matrix == this_covariance_matrix) %>%
      filter(row == column & row == max(row)) %$%
      row
    expected_number_of_covariances <- this_number_of_variances * (this_number_of_variances + 1) / 2

    if (this_observed_number_of_covariances != expected_number_of_covariances) {
      missing_covariances <- data.frame(
        covariance_matrix = rep(this_covariance_matrix, this_number_of_variances^2),
        row = rep(1:this_number_of_variances, this_number_of_variances),
        column = sort(rep(1:this_number_of_variances, this_number_of_variances)),
        estimate = 0
      ) %>%
        filter(row >= column) %>%
        arrange(row, column) %>%
        filter(!paste(row, column) %in% (covariance_components_threecolumn %>% filter(covariance_matrix == this_covariance_matrix) %$% paste(row, column)))

      covariance_components_threecolumn <- covariance_components_threecolumn %>%
        add_row(missing_covariances) %>%
        arrange(row, column)
    }

    this_covariance_matrix_square <- covariance_components_threecolumn %>%
      filter(covariance_matrix == this_covariance_matrix) %$%
      create_square_matrix_from_vector(estimate)

    this_transformation_matrix <- matrix(0, nrow(this_transformation_matrix_sum) * number_of_submodels, ncol(this_transformation_matrix_sum))

    for (i in 1:number_of_submodels) {
      this_transformation_matrix[(i - 1) * nrow(this_transformation_matrix_sum) + 1:nrow(this_transformation_matrix_sum), which(this_transformation_traits == i)] <-
        this_transformation_matrix_sum[, this_transformation_names[which(this_transformation_traits == i)]]
    }
    row_names_transformation <- paste("trait",
      sort(rep(1:number_of_submodels, nrow(this_transformation_matrix_sum))),
      ",",
      "point",
      1:nrow(this_transformation_matrix_sum),
      sep = ""
    )

    # Covariance matrix
    output[["Covariance matrix"]][[covariance_matrix_names[this_covariance_matrix]]] <- this_transformation_matrix %*% this_covariance_matrix_square %*% t(this_transformation_matrix)
    colnames(output[["Covariance matrix"]][[covariance_matrix_names[this_covariance_matrix]]]) <- row_names_transformation
    rownames(output[["Covariance matrix"]][[covariance_matrix_names[this_covariance_matrix]]]) <- row_names_transformation
    # Transformation_matrix
    output[["Transformation matrix"]][[covariance_matrix_names[this_covariance_matrix]]] <- this_transformation_matrix

    colnames(output[["Transformation matrix"]][[covariance_matrix_names[this_covariance_matrix]]]) <- this_transformation_names
    rownames(output[["Transformation matrix"]][[covariance_matrix_names[this_covariance_matrix]]]) <- row_names_transformation
    #
    if (this_class_effect_number > 0 & file.exists(solution_path)) {
      this_solutions <- fread(solution_path) %>%
        rename_all(
          function(x) {
            strsplit("effect_type submodel effect_number class_variable_number class_code something3 something4 prediction standard_error", split = " ") %>% unlist()
          }
        ) %>%
        filter(class_variable_number == this_random_effect_number & effect_type %in% 3:4) %>%
        select(all_of(c("class_code", "effect_number", "prediction"))) %>%
        arrange(class_code, effect_number) %>%
        pivot_wider(names_from = "effect_number", values_from = "prediction") %>%
        data.matrix()

      output[["Predicted effects"]][[covariance_matrix_names[this_covariance_matrix]]] <- cbind(
        this_solutions[, 1],
        this_solutions[, -1] %*% t(this_transformation_matrix)
      )
    }
  }
  return(output)
}
