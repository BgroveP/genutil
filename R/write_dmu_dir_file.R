write_dmu_dir_file <- function(information_path = NA,
                               analysis_type,
                               dense_analysis = F,
                               phenotypes,
                               fixed_effects = c(),
                               random_effect_structure = c(),
                               covariates = c(),
                               comment = " ",
                               covariance_structures = " ",
                               additional_options = " ",
                               social_interaction_model = 0,
                               traits_without_residual_covariance = c(""),
                               reduce_statement = c()) {
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(stringr)


  information_path <- "phenotypes_information.csv"
  analysis_type <- "average information ReML"
  dense_analysis <- F
  phenotypes <- c("energycorrected_milk_yield", "drymatter_intake")
  fixed_effects <- c(" herd      animal", "herd animal   ")
  random_effect_structure <- c("1", "1")
  covariates <- c(
    "days_in_milk_legendre1 days_in_milk_legendre2 days_in_milk_legendre3",
    "days_in_milk_legendre1 days_in_milk_legendre2 days_in_milk_legendre3"
  )
  comment <- "test"
  covariance_structures <- "$VAR_STR 1 PED 1 ASCII ../../pedigree.csv"
  additional_options <- c("$RESIDUALS ASCII", "$SOLUTION")
  SocialInteractionsModels <- 0
  traits_without_residual_covariance <- c("")
  reduce_statement <- c()

  # Read the input file
  input <- fread(information_path) %>%
    group_by(type) %>%
    mutate(variable_number = 1:n()) %>%
    ungroup() %>%
    add_row(data.frame(variable = "mu", type = "class", variable_number = 0)) %>%
    arrange(type, variable_number)

  # Start formulating model
  number_of_submodels <- length(phenotypes)

  ## read phenotypes
  submodel_phenotype <- input$variable_number[match(phenotypes, input$variable)]

  ## read class variables
  submodel_class_variables <- c()
  if (length(fixed_effects) >= 0) {
    for (j in 1:number_of_submodels) {
      class_variables_temporary <- gsub("^[[:space:]]+", "", gsub("[[:space:]]+", " ", fixed_effects[j])) %>%
        strsplit(split = " ") %>%
        unlist()

      submodel_class_variables[j] <- input$variable_number[match(class_variables_temporary, input$variable)] %>%
        paste(collapse = " ")
    }
  }

  x <- read.table(FAQfile, sep = "!", comment.char = "#") %>%
    mutate_all("as.character")

  VarPos <- grep("VARIABLE", x$V1)

  Fvars <- data.frame(V1 = NA) %>%
    mutate(V1 = x$V1[VarPos + 1]) %$%
    c(unlist(strsplit(V1, "\\s+")), "MU")

  Rvars <- data.frame(V1 = NA) %>%
    mutate(V1 = x$V1[VarPos + 2]) %$%
    c(unlist(strsplit(V1, "\\s+")))

  Fdic <- data.frame(VARIABLE = Fvars) %>%
    mutate(N = seq(1, n())) %>%
    spread("VARIABLE", "N") %$%
    cbind(., MU = c(0))

  Rdic <- data.frame(VARIABLE = Rvars) %>%
    mutate(N = seq(1, n())) %>%
    spread("VARIABLE", "N")

  Nvars <- length(phenotypes)

  # Response
  for (j in 1:Nvars) {
    for (i in 1:length(Rvars)) {
      rep <- i
      if (Rvars[i] == tail(strsplit(phenotypes[j], " ")[[1]], n = 1)) {
        SearchString <- Rvars[i]
      } else {
        SearchString <- paste(Rvars[i], " ", sep = "")
      }
      if (grepl(SearchString, phenotypes[j])) {
        phenotypes[j] <- gsub(SearchString, rep, phenotypes[j])
      }
    }
  }

  # Class
  if (length(fixed_effects) > 0) {
    for (j in 1:Nvars) {
      for (i in 1:length(Fvars)) {
        if (Fvars[i] == "MU") {
          rep <- 0
        } else {
          rep <- i
        }
        if (Fvars[i] == tail(strsplit(fixed_effects[j], " ")[[1]], n = 1)) {
          SearchString <- Fvars[i]
        } else {
          SearchString <- paste(Fvars[i], " ", sep = "")
        }
        if (grepl(SearchString, fixed_effects[j])) {
          fixed_effects[j] <- gsub(SearchString, paste(rep, " ", sep = ""), fixed_effects[j])
        }
      }
    }
    for (j in 1:Nvars) {
      for (i in 1:length(Fvars)) {
        if (Fvars[i] == "MU") {
          rep <- 0
        } else {
          rep <- i
        }
        SearchString <- Fvars[i]
        if (grepl(SearchString, fixed_effects[j])) {
          fixed_effects[j] <- gsub(SearchString, rep, fixed_effects[j])
        }
      }
    }

    for (j in 1:Nvars) {
      if (tail(c(unlist(strsplit(fixed_effects[j], ""))), n = 1) == " ") {
        fixed_effects[j] <- substr(fixed_effects[j], 1, (nchar(fixed_effects[j]) - 1))
      }
    }
  }
  # Real
  if (length(random_effect_structure) > 0) {
    for (j in 1:Nvars) {
      for (i in 1:length(Rvars)) {
        rep <- i
        if (Rvars[i] == tail(strsplit(covariates[j], " ")[[1]], n = 1)) {
          lastR <- T
          SearchString <- paste(" ", Rvars[i], sep = "")
        } else {
          lastR <- F
          SearchString <- paste(Rvars[i], " ", sep = "")
        }
        if (grepl(SearchString, covariates[j]) & !lastR) {
          covariates[j] <- gsub(SearchString, paste(rep, " ", sep = ""), covariates[j])
        }
        if (grepl(SearchString, covariates[j]) & lastR) {
          covariates[j] <- gsub(SearchString, paste(" ", rep, sep = ""), covariates[j])
        }
      }
    }
    for (j in 1:Nvars) {
      for (i in 1:length(Rvars)) {
        rep <- i
        SearchString <- Rvars[i]
        if (grepl(SearchString, covariates[j])) {
          covariates[j] <- gsub(SearchString, as.character(rep), covariates[j])
        }
      }
    }

    if (social_interaction_model == 1) {
      for (j in 1:Nvars) {
        for (i in 1:length(Fvars)) {
          rep <- which(c(strsplit(gsub("(0)", "", fixed_effects[j], fixed = T), " ")[[1]]) %in% i)
          SearchString <- paste(Fvars[i], Fvars[i], sep = " ")
          if (grepl(SearchString, covariates[j])) {
            covariates[j] <- gsub(SearchString, paste(rep, collapse = " "), covariates[j])
          }
        }
      }
    }

    for (j in 1:Nvars) {
      for (i in 1:length(Fvars)) {
        rep <- which(c(strsplit(gsub("(0)", "", fixed_effects[j], fixed = T), " ")[[1]]) %in% i)
        SearchString <- Fvars[i]
        if (grepl(SearchString, covariates[j])) {
          covariates[j] <- gsub(SearchString, rep, covariates[j])
        }
      }
    }
  }

  # Number of variables
  if (length(fixed_effects) > 0) {
    temp <- gsub("[[:space:]]", "a", fixed_effects)
    temp2 <- gsub("\\d", "", temp)
    if (grepl("[[:punct:]]", temp2)[1]) {
      NfixEff <- nchar(gsub("[[:punct:]]", "", temp2)) + 1
    } else {
      NfixEff <- nchar(temp2) + 1
    }
  } else {
    NfixEff <- 0
  }

  if (length(random_effect_structure) > 0) {
    temp <- gsub("[[:space:]]", "", random_effect_structure)
    temp2 <- gsub("\\([[:digit:]]+\\.[[:digit:]]+\\)", "", temp)
    temp3 <- gsub("[[:punct:]]", "", temp2)
    Nrandom <- nchar(temp3)
  } else {
    Nrandom <- 0
  }

  if (length(covariates) > 0) {
    temp <- gsub("[[:space:]]", "a", covariates)
    temp2 <- gsub("\\d", "", temp)
    Ncovar <- nchar(gsub("[[:punct:]]", "", temp2)) + 1
  } else {
    Ncovar <- 0
  }

  Nvars <- length(c(unlist(strsplit(phenotypes, " "))))
  ModelClass <- matrix(NA, Nvars, 1)
  ModelRandom <- matrix(NA, Nvars, 1)
  ModelCovar <- matrix(NA, Nvars, 1)

  for (i in 1:length(phenotypes)) {
    ModelClass[i, ] <- paste(phenotypes[i],
      "0",
      NfixEff[i],
      paste(c(unlist(strsplit(fixed_effects[i], " "))), collapse = " "),
      sep = " "
    )

    ModelRandom[i, ] <- paste(Nrandom[i],
      random_effect_structure[i],
      collapse = " "
    )

    if (Ncovar[i] != 0) {
      ModelCovar[i, ] <- paste(Ncovar[i],
        paste(c(unlist(strsplit(covariates[i], " "))), collapse = " "),
        sep = " "
      )
    } else {
      ModelCovar[i, ] <- Ncovar[i]
    }
  }

  DirOut <- rbind(
    "$COMMENT",
    Comment,
    " ",
    if (analysis_type == "AI" & dense_analysis) {
      "$ANALYSE 1 31 0 0"
    },
    if (analysis_type == "AI" & !dense_analysis) {
      "$ANALYSE 1 1 0 0"
    },
    if (analysis_type == "DMU4" & dense_analysis) {
      "$ANALYSE 11 30 0 0"
    },
    if (analysis_type == "DMU4" & !dense_analysis) {
      "$ANALYSE 11 9 0 0"
    },
    if (analysis_type == "DMU5") {
      "$ANALYSE 12 2 0 0"
    },
    "",
    x,
    " ",
    "$MODEL",
    if (social_interaction_model == 1) {
      paste(Nvars, "1 0 0 0 ")
    } else {
      Nvars
    },
    matrix(0, Nvars, 1),
    ModelClass,
    ModelRandom,
    ModelCovar,
    ifelse(nchar(traits_without_residual_covariance) == 0, paste("0", "0", sep = "\n"), paste(length(traits_without_residual_covariance), paste(traits_without_residual_covariance, collapse = "\n"), sep = "\n")),
    " ",
    if (length(reduce_statement) > 0) {
      rbind(
        "$REDUCE",
        matrix(reduce_statement, length(reduce_statement), 1)
      )
    },
    paste(covariance_structures, collapse = "\n"),
    matrix(additional_options, length(additional_options), 1)
  )
}
