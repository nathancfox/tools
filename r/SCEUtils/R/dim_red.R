#' Runs PCA.
#'
#' Runs the \code{rsvd::rpca} function on a subset of the
#' expression data in an SingleCellExperiment object.
#' 
#' @param sce_obj The SingleCellExperiment object.
#' @param hvg The highly variable genes to include.
#' @param n_pcs The number of PCs to calculate.
#' @param assay The named assay to use from the SCE.
#' @param full_results If TRUE, the full results from the
#'   \code{rpca} function will be returned. If FALSE, only the
#'   cells x PCA matrix will be returned.
#' @return Either the full results list or the PCA matrix.
#' @export
run_pca <- function(sce_obj, hvg, n_pcs = 100, assay = "logcounts",
                    full_results = TRUE) {
    mat <- SummarizedExperiment::assay(sce_obj, i = assay)[hvg, ]
    pca_results <- rsvd::rpca(t(as.matrix(mat)), k = n_pcs)
    if (full_results) {
        return(pca_results)
    } else {
        return(pca_results$x)
    }
}

#' Plot a PCA Screeplot
#'
#' Plots the principal components against their variances
#' to create an "elbow plot" or a screeplot. This plot is typically
#' used to estimate the number of PCs to include.
#'
#' @param pca_results Anything with an sdev vector in it. Typically
#'   from \code{prcomp}, \code{princomp}, or \code{rsvd:rpca}.
#' @param npcs The number of PCs to plot.
#' @return Nothing. It just plots the plot.
#' @export
pca_elbow_plot <- function(pca_results, npcs = 50) {
    screeplot(pca_results, npcs = 50, type = "line",
              main = "Principal Components Variance")
}

#' Runs FIt-SNE on an scRNA-seq PCA matrix.
#'
#' Runs the FIt-SNE version of t-SNE on a cells x PCs matrix
#' from a scRNA-seq dataset. Implements the recommendations made
#' by Kobak & Berens (10.1038/s41467-019-13056-x) for t-SNE on
#' scRNA-seq data.
#'
#' @param mat A cells x PCs matrix.
#' @param n_pcs The number of PCs to include.
#' @param fast_tsne_path The path to the fast_tsne executable.
#'   Not technically necessary as long as the copy of fast_tsne.R
#'   is appropriately altered for this package.
#' @return A cells x 2 matrix holding the t-SNE coordinates.
#' @export
run_fast_tsne <- function(mat, n_pcs = NULL,
                          fast_tsne_path = NULL) {
    if (!is.null(n_pcs)) {
        mat <- mat[, 1:n_pcs]
    }
    n_cells <- dim(mat)[1]
    if (n_cells > 100000) {
        perplexity_list <- NULL
        perplexity <- 30
    } else if ((n_cells / 100) > 70) {
        perplexity_list <- c(30, (n_cells / 100))
        perplexity <- 0
    } else {
        perplexity_list <- NULL
        perplexity <- (n_cells / 100)
    }
    initialization <- mat[, 1:2]
    if ((n_cells / 12) > 200) {
        learning_rate <- n_cells / 12
    } else {
        learning_rate <- 200
    }
    fast_tsne_out <- fftRtsne(mat, perplexity = perplexity,
                              perplexity_list = perplexity_list,
                              initialization = initialization,
                              learning_rate = learning_rate,
                              fast_tsne_path = fast_tsne_path)
    return(fast_tsne_out)
}

#' Runs UMAP on an scRNA-seq PCA matrix.
#'
#' Runs the umap version of UMAP on a cells x PCs matrix
#' from an scRNA-seq dataset.
#'
#' @param mat A cells x PCs matrix.
#' @param n_pcs The number of PCs to include.
#' @return A cells x 2 matrix holding the UMAP coordinates.
#' @export
run_umap <- function(mat, n_pcs = NULL) {
    if (!is.null(n_pcs)) {
        mat <- mat[, 1:n_pcs]
    }
    umap_res <- umap::umap(mat)
    return(umap_res$layout)
}
