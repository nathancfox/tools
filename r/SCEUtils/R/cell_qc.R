#' Plot rank-ordered library size.
#'
#' Plots the library size of each cell, rank-ordered by decreasing
#' library size. Also adds the knee and inflection points.
#' @param sce_obj The SingleCellExperiment object.
#' @param assay The SCE assay to use.
#' @return Nothing. Just plots the plot.
#' @export
plot_read_count_cell_ranks <- function(sce_obj, assay = "counts") {
    br_out <- DropletUtils::barcodeRanks(SummarizedExperiment::assay(sce_obj, i = assay))
    plot(br_out$rank, br_out$total, log = "xy",
         xlab = "Rank", ylab = "Total")
    o <- order(br_out$rank)
    lines(br_out$rank[o], br_out$fitted[o], col = "red")
    
    abline(h = br_out@metadata$knee, col = "dodgerblue", lty = 2)
    abline(h = br_out@metadata$inflection, col = "forestgreen", lty = 2)
    legend("bottomleft", lty = 2, col = c("dodgerblue", "forestgreen"),
           legend = c(sprintf("knee       = %.0f", br_out@metadata$knee), 
                      sprintf("inflection = %.0f", br_out@metadata$inflection)))
}

#' Filters empty droplets from droplet-based scRNA-seq data.
#'
#' Uses the \code{DropletUtils::emptyDrops} function to filter
#' out empty droplets. Can just mark the empty drops in the 
#' \code{colData}, or can actually remove them.
#'
#' @param sce_obj The SingleCellExperiment object.
#' @param min_read_count All cells with a library size lower than this
#'   will be automatically filtered without calculation.
#' @param assay The SCE assay to use.
#' @param alpha The cutoff value for significance testing
#' @param keep If TRUE, the empty drops will be marked and kept instead
#'   of being filtered out.
#' @param marker_column If keep == TRUE, the empty drops will be marked
#'   in this column of \code{colData}.
#' @param verbose If TRUE, the statistics will be reported via stdout.
#' @return The edited SCE object.
#' @export
filter_empty_drops <- function(sce_obj, min_read_count,
                               assay = "counts", alpha = 0.05,
                               keep = FALSE, marker_column = "non_empty",
                               verbose = TRUE) { 
    ed_results <- DropletUtils::emptyDrops(SummarizedExperiment::assay(sce_obj, i = assay),
                                           lower = min_read_count,
                                           test.ambient = FALSE)
    if (verbose) {
        n_cells_below_min <- sum(is.na(ed_results$FDR)) / length(is.na(ed_results$FDR)) * 100
        cat(sprintf("%.2f%% of the droplets have UMI counts of fewer than lower option = %d.\n\n",
                    n_cells_below_min, min_read_count))    
    }
    filter <- ed_results$FDR < alpha
    filter[is.na(filter)] <- FALSE
    if (verbose) {
        if (keep) {
            cat(sprintf("%.2f%% of the cells were marked as empty droplets.\n", sum(!filter) / length(filter) * 100, sum(filter)))
        } else {
            cat(sprintf("%.2f%% of the cells were removed as empty droplets.\n%d cells remain.", sum(!filter) / length(filter) * 100, sum(filter)))
        }
    }
    if (keep) {
        col_names <- colnames(SummarizedExperiment::colData(sce_obj))
        if (marker_column %in% col_names) {
            loop <- TRUE
            counter <- 1
            while (loop) {
                if (!(paste(marker_column, counter, sep = "_") %in% col_names)) {
                    loop = FALSE
                    marker_column <- paste(marker_column, counter, sep = "_")
                }
            }
            warning(sprintf("marker_column exists! Using %s", marker_column))
        }
        SummarizedExperiment::colData(sce_obj)[, marker_column] <- filter
    } else {
        sce_obj <- sce_obj[, filter]
    }
    return(sce_obj)
}

#' Calculate Library Size
#'
#' Calculate a vector of number of counts for each cell.
#'
#' @param sce_obj The SingleCellExperiment object.
#' @param assay The named assay to use.
#' @return A numeric vector the same length as the number
#'   of cells in sce_obj. The number of counts for each cell.
#' @export
calc_n_counts <- function(sce_obj, assay = "counts") {
    return(colSums(as.matrix(SummarizedExperiment::assay(sce_obj, i = assay))))
}

#' Calculate Number of Genes.
#'
#' Calculate a vector of number of non-zero genes for each cell.
#'
#' @param sce_obj The SingleCellExperiment object.
#' @param assay The named assay to use.
#' @return A numeric vector the same length as the number
#'   of cells in sce_obj. The number of expressed genes for each cell.
#' @export
calc_n_genes <- function(sce_obj, assay = "counts") {
    return(colSums(as.matrix((SummarizedExperiment::assay(sce_obj, i = assay)) > 0)))
}
