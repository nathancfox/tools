# Collection of functions that take (and generally return) a
# SingleCellExperiment object as an argument.

plot_read_count_cell_ranks <- function(sce_obj, assay = "counts") {
    br_out <- DropletUtils::barcodeRanks(SummarizedExperiment::assay(sce_obj, i = assay))
    plot(br_out$rank, br_out$total, log = "xy",
         xlab = "Rank", ylab = "Total")
    o <- order(br_out$rank)
    lines(br_out$rank[o], br_out$fitted[o], col = "red")
    
    abline(h = br_out@metadata$knee, col = "dodgerblue", lty = 2)
    abline(h = br_out@metadata$inflection, col = "forestgreen", lty = 2)
    legend("bottomleft", lty = 2, col = c("dodgerblue", "forestgreen"),
           legend = c(sprintf("knee       = %.0f", new_br_out@metadata$knee), 
                      sprintf("inflection = %.0f", new_br_out@metadata$inflection)))
}

filter_empty_drops <- function(sce_obj, min_read_count,
                               assay = "counts", alpha = 0.05,
                               keep = FALSE, marker_column = "non_empty",
                               verbose = FALSE) { 
    ed_results <- DropletUtils::emptyDrops(SummarizedExperiment::assay(sce_obj, i = assay),
                                           lower = min_read_count,
                                           test.ambient = FALSE)
    if (verbose) {
        n_cells_below_min <- sum(is.na(ed_results$FDR)) / length(is.na(ed_results$FDR)) * 100
        cat(sprintf("\n%.2f%% of the droplets have UMI counts of fewer than lower option = %d.\n\n", temp, lower_option))    
    }
    filter <- ed_results$FDR < alpha
    filter[is.na(filter)] <- FALSE
    if (verbose) {
        if (keep) {
            cat(sprintf("%.2f%% of the cells were marked as empty droplets.\n%d cells remain.", sum(!filter) / length(filter) * 100, sum(filter)))
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

get_cell_data(sce_obj) {
    return(data.frame(SummarizedExperiment::colData(sce_obj)))
}

get_gene_data(sce_obj) {
    return(data.frame(SummarizedExperiment::rowData(sce_obj)))
}
















