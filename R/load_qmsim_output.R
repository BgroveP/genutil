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
load_QMsim_output <- function(path, replicate = 1) {
    phenotype_data_path <- paste(path, "/temporary_phenotypes.csv", sep = "")
    genotype_data_path <- paste(path, "/temporary_genotypes.csv", sep = "")

    files <- data.frame(
        full_path = list.files(path,
            include.dirs = T,
            full.names = T
        ),
        file = list.files(path)
    ) %>%
        mutate(
            filetype = substr(file, nchar(file) - 3, nchar(file)),
            file_replicate = as.numeric(ifelse(!substr(file, nchar(file) - 6, nchar(file) - 6) %in% as.character(0:9), "0", substr(file, nchar(file) - 6, nchar(file) - 4))),
            is_linkage_map = substr(file, 1, 3) == "lm_",
            is_qtl_file = grepl("_qtl_", file)
        )

    data_files <- files %>%
        filter(file_replicate == replicate & filetype == ".txt" & !is_linkage_map & !is_qtl_file)

    linkage_files <- files %>%
        filter(file_replicate == replicate & is_linkage_map)

    # Read the linkage map
    linkage_map <- fread(linkage_files$full_path[1]) %>%
        add_row(fread(linkage_files$full_path[2])) %>%
        rename_all(function(x) {
            return(unlist(strsplit("locus chromosome position", split = " ")))
        }) %>%
        arrange(chromosome, position) %>%
        mutate(locus = ifelse(substr(locus, 1, 1) == "M", gsub("M", "marker", locus), gsub("Q", "qtl", locus)))

    number_of_data_files <- nrow(data_files)
    for (file_number in 1:number_of_data_files) {
        if (grepl("_data_", data_files$full_path[file_number])) {
            fread(data_files$full_path[file_number]) %>%
                rename_all(function(x) {
                    return(unlist(strsplit("animal father mother sex generation number_of_male_progeny number_of_female_progeny inbreeding_coefficient homozygosity phenotype residual polygenic_effect qtl_effect", split = " ")))
                }) %>%
                mutate(
                    population = unlist(strsplit(data_files$file[file_number], split = "_"))[1],
                    sex = ifelse(sex == "M", "male", "female")
                ) %>%
                fwrite(phenotype_data_path, append = data_files$file[file_number] != data_files$file[min(which(grepl("_data_", data_files$full_path)))])
        }


        if (grepl("_mrk_", data_files$full_path[file_number])) {

            # Read the marker matrix
            intermediate_marker_matrix <- fread(data_files$full_path[file_number], header = F) %>%
                data.matrix()

            number_of_markers <- (ncol(intermediate_marker_matrix) - 1) / 2
            marker_genotype_matrix <- (intermediate_marker_matrix[, 1 + seq(1, 2 * number_of_markers, 2)] + intermediate_marker_matrix[, 1 + seq(2, 2 * number_of_markers, 2)] - 2)
            colnames(marker_genotype_matrix) <- paste("marker", 1:number_of_markers, sep = "")


            # Read the qtl matrix
            intermediate_qtl_matrix <- fread(gsub("_mrk_", "_qtl_", data_files$full_path[file_number]), header = F) %>%
                data.matrix()

            number_of_qtl <- (ncol(intermediate_qtl_matrix) - 1) / 2
            qtl_genotype_matrix <- (intermediate_qtl_matrix[, 1 + seq(1, 2 * number_of_qtl, 2)] + intermediate_qtl_matrix[, 1 + seq(2, 2 * number_of_qtl, 2)] - 2)
            colnames(qtl_genotype_matrix) <- paste("qtl", 1:number_of_qtl, sep = "")

            # Combine information
            cbind(marker_genotype_matrix, qtl_genotype_matrix)[, linkage_map$locus] %>%
                as.data.frame() %>%
                mutate(animal = intermediate_qtl_matrix[, 1]) %>%
                relocate(animal) %>%
                fwrite(genotype_data_path, append = data_files$file[file_number] != data_files$file[min(which(grepl("_mrk_", data_files$full_path)))])
        }
    }

    phenotypes <- fread(phenotype_data_path)
    genotypes <- fread(genotype_data_path)

    list("phenotypes" = phenotypes, "genotypes" = genotypes, "linkage_map" = linkage_map) %>%
        return()
}
