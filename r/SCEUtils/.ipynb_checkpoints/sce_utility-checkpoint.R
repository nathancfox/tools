# Getters for metadata dataframes

get_cell_data <- function(sce_obj) {
    return(data.frame(SummarizedExperiment::colData(sce_obj)))
}

get_gene_data <- function(sce_obj) {
    return(data.frame(SummarizedExperiment::rowData(sce_obj)))
}