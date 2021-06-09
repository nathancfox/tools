#' Prints the str() of the colData from the SCE object.
#'
#' Gets the colData from the SCE object and transforms it from
#' an S4Vectors dataframe to a base dataframe. It is then fed
#' to \code{str()} to get a summary of the fields.
#'
#' @param sce_obj The SingleCellExperiment object.
#' @export
summary_cell <- function(sce_obj) {
    str(data.frame(SummarizedExperiment::colData(sce_obj)))
}

#' Prints the str() of the rowData from the SCE object.
#'
#' Gets the rowData from the SCE object and transforms it from
#' an S4Vectors dataframe to a base dataframe. It is then fed
#' to \code{str()} to get a summary of the fields.
#'
#' @param sce_obj The SingleCellExperiment object.
#' @export
summary_gene <- function(sce_obj) {
    str(data.frame(SummarizedExperiment::rowData(sce_obj)))
}

#' Returns the scran-chosen highly variable genes from the SCE object.
#'
#' Uses \code{scran::modelGeneVar} to select highly variable
#' genes from the SCE object. Then returns them as a vector.
#'
#' @param sce_obj The SingleCellExperiment object.
#' @param assay The name of the assay to be used.
#' @param alpha The FDR cutoff. Any gene with an FDR <= to this
#'   value will be labeled highly variable.
#' @export
scran_hvg <- function(sce_obj, assay = "logcounts", alpha = 0.5) {
    # fit <- scran::trendVar(sce_obj, use.spikes=F)
    # decomp <- scran::decomposeVar(sce_obj, fit)
    # hvg <- (decomp$FDR <= 0.05)
    model <- scran::modelGeneVar(sce_obj, assay.type = assay)
    hvg <- rownames(sce_obj)[(model$FDR <= alpha)]
    hvg <- hvg[!is.na(hvg)]
    return(hvg)
}
