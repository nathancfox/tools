#' @export
calc_cluster_de_1v1 <- function(sce_obj, clust_ids, fc_thresh=0.5, p_thresh=1E-4,
                                method=c("mw", "ttest"), missing_clusts = FALSE,
                                full=FALSE) {
    # I ADDED >>>
    if (is.character(clust_ids) && length(clust_ids == 1) &&
            clust_ids %in% colnames(SingleCellExperiment::colData(sce_obj))) {
        clust_ids <- sce_obj[[clust_ids]]
    }
    if (missing_clusts) {
        l <- length(levels(clust_ids))
    } else {
        l <- length(unique(clust_ids))
    }
    # <<<
    # l <- length(unique(clust_ids))
    # clust.mean <- t(apply(2^logcounts(sce_obj), 1,
    #                       function(z) {sapply(split(z, clust_ids), mean)}))
    clust.mean <- t(apply(2^SingleCellExperiment::logcounts(sce_obj)-1, 1,
                          function(z) {sapply(split(z, clust_ids), mean)}))

    logFC <- list()
    for (i in 1:l){
        fc <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
        for(j in 1:l) {
            fc[, j] <- log2(clust.mean[, i] + 1) - log2(clust.mean[, j] + 1)
        }
        logFC[[i]] <- fc
    }

    min.fc <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
    for (i in 1:l) {
        min.fc[,i]<- apply(logFC[[i]][, -i], 1 ,min)
    }

    combos <- combn(unique(clust_ids), 2)

    if (method == "mw") {
        pvals <- vector("list", length=dim(combos)[2])
        names(pvals) <- apply(combos, 2, function(x) {paste(x[1], "vs", x[2], sep="_")})
        for (i in 1:dim(combos)[2]) {
            f1 <- (clust_ids == combos[1, i])
            f2 <- (clust_ids == combos[2, i])
            pvals[[i]] <- apply(SingleCellExperiment::logcounts(sce_obj), 1,
                                function(x) {wilcox.test(x[f1], x[f2])$p.value})
        }
    }
    else {
        pvals <- vector("list", length=dim(combos)[2])
        names(pvals) <- apply(combos, 2, function(x) {paste(x[1], "vs", x[2], sep="_")})
        for (i in 1:dim(combos)[2]) {
            f1 <- (clust_ids == combos[1, i])
            f2 <- (clust_ids == combos[2, i])
            temp <- genefilter::rowttests(as.matrix(SingleCellExperiment::logcounts(sce_obj)[, (f1 | f2)]), as.factor(f1[f1 | f2]))
            pvals[[i]] <- temp[, 3]
        }
    }

    maxp <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
    for (i in 1:l) {
        a <- paste("^", i, "_vs", sep="")
        b <- paste("[[:digit:]]+_vs_", i, "$", sep="")
        ind <- c(grep(a, names(pvals)), grep(b, names(pvals)))
        temp <- as.data.frame(pvals[ind])
        maxp[, i] <- apply(temp, 1, function(x) suppressWarnings(max(x, na.rm=TRUE)))
    }

    gene.mat <- matrix(0, ncol=l, nrow=dim(maxp)[1])
    rownames(gene.mat) <- rownames(clust.mean)
    for (i in 1:l) {
        f <- (min.fc[, i] > fc_thresh) & (maxp[, i] < p_thresh)
        gene.mat[f, i] <- 1
    }

    if (full) {
        res <- list(clust_exp=clust.mean, deg=gene.mat,
                    max_pval=maxp, min_fc=min.fc,
                    log_fc=logFC, pvals=pvals)
    } else {
        res <- list(clust_exp=clust.mean, deg=gene.mat,
                    max_pval=maxp, min_fc=min.fc)
    }
    return(res)
}

#' @export
calc_cluster_de_vs_all_cells <- function(sce_obj, clust_ids, fc_thresh=0.5,
                                         p_thresh=1E-4, method=c("mw", "ttest"),
                                         numeric_clusts = FALSE, depth = NULL,
                                         log_mean_exp = FALSE) {
    # I ADDED >>>
    if (is.character(clust_ids) && length(clust_ids == 1) &&
            clust_ids %in% colnames(SingleCellExperiment::colData(sce_obj))) {
        clust_ids <- sce_obj[[clust_ids]]
    }
    clusts <- unique(as.character(clust_ids))
    l <- length(clusts)
    if (numeric_clusts) {
        clusts <- sort(as.numeric(clusts))
    } else {
        # clusts <- sort(as.numeric(unique(clust_ids)))
        clusts <- sort(clusts)
    }
    if (! is.null(depth)) {
        if (! (depth %in% colnames(SingleCellExperiment::colData(sce_obj)))) {
            stop("depth must be the name of a column in colData!")
        }
    }
    # <<<
    if (is.null(depth)) {
        clust.mean <- t(apply(((2^SingleCellExperiment::logcounts(sce_obj))-1),
                              1, function(z) {sapply(split(z, clust_ids), mean)}))
#         clust.mean <- t(apply(2^SingleCellExperiment::logcounts(sce_obj),
#                           1, function(z) {sapply(split(z, clust_ids, drop = TRUE), mean)}))
    } else {
        clust.mean <- t(apply(matrix(sce_obj[[depth]], nrow=1), 1,
                              function(z) {sapply(split(z, clust_ids), sum)}))
    }
        
    logFC <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
    rownames(logFC) <- rownames(clust.mean)
    colnames(logFC) <- clusts
    for (i in 1:l) {
        if (is.null(depth)) {
#             logFC[, i] <- log2(clust.mean[, i] + 1) - log2(rowMeans(clust.mean[, -i]) + 1)
            logFC[, i] <- log2(clust.mean[, i] + 1) - log2(base::rowMeans(clust.mean[, -i, drop = FALSE], na.rm = TRUE) + 1)
        } else {
            logFC[, i] <- log2(clust.mean[, i] + 1) - log2(base::rowMeans(matrix(clust.mean[, -i], nrow = 1), na.rm = TRUE) + 1)
        }
    }
    
    if (method == "mw") {
        pvals <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
        rownames(pvals) <- rownames(clust.mean)
        colnames(pvals) <- clusts
        for (i in 1:l) {
            f <- (clust_ids == clusts[i])
            if (is.null(depth)) {
                pvals[, i] <- matrixTests::row_wilcoxon_twosample(as.matrix(SingleCellExperiment::logcounts(sce_obj)[, f]),
                                                                  as.matrix(SingleCellExperiment::logcounts(sce_obj)[, !f]))$pvalue
            } else {
                pvals[, i] <- matrixTests::row_wilcoxon_twosample(matrix(sce_obj[[depth]][f], nrow = 1),
                                                                  matrix(sce_obj[[depth]][!f], nrow = 1))$pvalue
            }
            # if (is.null(depth)) {
            #     pvals[,i] <- apply(as.matrix(SingleCellExperiment::logcounts(sce_obj)),
            #                        1, function(x) {wilcox.test(x[f], x[!f], alternative="g")$p.value})
            # } else {
            #     pvals[,i] <- apply(matrix(sce_obj[[depth]], nrow = 1),
            #                        1, function(x) {wilcox.test(x[f], x[!f], alternative="g")$p.value})
            # }
        }
    }
    else {
        pvals <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
        rownames(pvals) <- rownames(clust.mean)
        colnames(pvals) <- clusts
        for (i in 1:l) {
            f <- (clust_ids == i)
            if (is.null(depth)) {
                temp <- genefilter::rowttests(as.matrix(SingleCellExperiment::logcounts(sce_obj)), as.factor(f))
            } else {
                temp <- genefilter::rowttests(matrix(sce_obj[[depth]], nrow=1), as.factor(f))
            }
            pvals[, i] <- temp[, 3]
        }
    }
    
    gene.mat <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
    rownames(gene.mat) <- rownames(clust.mean)
    colnames(gene.mat) <- clusts
    for (i in 1:l) {
        if (is.null(depth)) {
            f <- (logFC[, i] > fc_thresh) & (pvals[, i] < p_thresh)
        } else {
            f <- (abs(logFC[, i]) > fc_thresh) & (pvals[, i] < p_thresh)
        }
        gene.mat[f, i] <- 1
    }

    if (is.null(depth)) {
        if (log_mean_exp) {
            clust.mean <- log2(clust.mean + 1)
        }
    }
    
    res <- list(clust_exp = clust.mean, deg = gene.mat,
                pval = pvals, logfc = logFC)
    return(res)
}
                           
#' @export
calc_ROC_de <- function(sce_obj, clusters) {
    # I ADDED >>>
    if (is.character(clusters) && length(clusters == 1) &&
            clusters %in% colnames(SingleCellExperiment::colData(sce_obj))) {
        clusters <- sce_obj[[clusters]]
    }
    # <<<
    filt <- !is.na(clusters)
#     require(EGAD)
#     gf <- rowSums(SingleCellExperiment::counts(sce_obj[, filt])) != 0
#     gf <- (Matrix::rowSums(SingleCellExperiment::counts(sce_obj[, filt])) != 0)
    dmat <- model.matrix(~as.factor(clusters[filt]) + 0)
#     rank_dat <- t(apply(SingleCellExperiment::logcounts(sce_obj[gf, filt]),
#                         1, function(x) {rank(x, ties.method="average")}))
    rank_dat <- t(apply(SingleCellExperiment::logcounts(sce_obj[, filt]),
                        1, function(x) {rank(x, ties.method="average")}))
    de_roc <- t(apply(rank_dat, 1, function(x) {EGAD::auc_multifunc(dmat, x)}))
    colnames(de_roc) <- levels(as.factor(clusters[filt]))
    return(de_roc)
}
                           
#' @export
percent_in_out <- function(sce_obj, clusters, missing_clusts = FALSE) {
    # I ADDED >>>
    if (is.character(clusters) && length(clusters == 1) &&
            clusters %in% colnames(SingleCellExperiment::colData(sce_obj))) {
        clusters <- sce_obj[[clusters]]
    }
    # <<<
    filt <- !is.na(clusters)
#     gf <- (rowSums(SingleCellExperiment::counts(sce_obj[, filt])) != 0)
    gf <- (rowSums(as.matrix(SingleCellExperiment::counts(sce_obj[, filt]))) != 0)
    # I ADDED >>>
    if (missing_clusts) {
        clusts <- sort(as.numeric(levels(clusters[filt])))
    } else {
        clusts <- sort(as.numeric(unique(clusters[filt])))
    }
    # <<<
    pct_in_out <- vector('list', length=length(clusts))
    # I ADDED >>>
    clusters <- as.character(clusters)
    # <<<
    for(i in 1:length(clusts)) {
        f <- (clusters[filt] == clusts[i])
#         pct_in_out[[i]]$p_in <- (rowSums(SingleCellExperiment::counts(sce_obj[, filt][, f]) > 0) / sum(f))
#         pct_in_out[[i]]$p_out <- (rowSums(SingleCellExperiment::counts(sce_obj[, filt][, !f]) > 0) / sum(!f))
        pct_in_out[[i]]$p_in <- (Matrix::rowSums(SingleCellExperiment::counts(sce_obj[, filt][, f]) > 0) / sum(f))
        pct_in_out[[i]]$p_out <- (Matrix::rowSums(SingleCellExperiment::counts(sce_obj[, filt][, !f]) > 0) / sum(!f))
    }
    names(pct_in_out) <- clusts
    return(pct_in_out)
}
                           
#' @export
write_de_full <- function(pct_list, roc_list, de_res,
                     path_prefix = "", missing_clusters = FALSE) {
    for (j in 1:length(pct_list)) {
        m <- match(rownames(roc_list[[j]]), names(pct_list[[j]][[1]][[1]]))
        f.a = !is.na(m)
        f.b = m[f.a]
        for (i in 1:dim(roc_list[[j]])[2]) {
            clust_exp <- de_res[[j]]$clust_exp[, i]
            log_fc <- log2(de_res[[j]]$clust_exp[, i] + 1) - log2(base::rowMeans(de_res[[j]]$clust_exp[, -i], na.rm = TRUE) + 1)
            de_pval <- de_res[[j]]$pval[, i]
            de_roc <- roc_list[[j]][, i]
            pct_in <- pct_list[[j]][[i]][[1]]
            pct_out <- pct_list[[j]][[i]][[2]]
            de_temp <- data.frame(mean_exp=clust_exp, log2fc=log_fc,
                                  de_pval=de_pval, de_roc=de_roc,
                                  pct_in=pct_in, pct_out=pct_out)
            # batch names depend on the names in pct_list
            # and they must be numbers. Fragile point.
            if (is.null(names(pct_list))) {
                batch <- j
            } else if (is.na(names(pct_list)[j])) {
                batch <- j
            } else {
                batch <- names(pct_list)[j]
            }
            if (is.null(colnames(roc_list[[j]]))) {
                cluster <- i
            } else if (is.na(colnames(roc_list[[j]])[i])) {
                cluster <- i
            } else {
                cluster <- colnames(roc_list[[j]])[i]
            }
            file <- file.path(path_prefix,
                              paste("batch", batch, "cluster", cluster,
                                    "de.csv", sep = "_"))
            if (!(all(is.na(de_temp$mean_exp)))) {
                write.table(de_temp, file = file,
                            quote = FALSE, sep = ",")
            }
        }
    }
}

#' @export
write_de <- function(de_list, path_prefix = "", missing_clusters = FALSE) {
    for (j in 1:length(de_list)) {
        for (i in 1:dim(de_list[[j]][[1]])[2]) {
            clust_exp <- de_list[[j]]$clust_exp[, i]
            log_fc <- de_list[[j]]$logfc[, i]
            de_pval <- de_list[[j]]$pval[, i]
            de_temp <- data.frame(mean_exp=clust_exp,
                                  log2fc=log_fc,
                                  de_pval=de_pval)
            # batch names depend on the names in pct_list
            # and they must be numbers. Fragile point.
            if (is.null(names(de_list))) {
                batch <- as.character(j)
            } else if (is.na(names(de_list)[j])) {
                batch <- as.character(j)
            } else {
                batch <- names(de_list)[j]
            }
            if (is.null(colnames(de_list[[j]][[1]]))) {
                cluster <- as.character(i)
            } else if (is.na(colnames(de_list[[j]][[1]])[i])) {
                cluster <- as.character(i)
            } else {
                cluster <- colnames(de_list[[j]][[1]])[i]
            }
            file <- file.path(path_prefix,
                              paste("batch", batch, "cluster", cluster,
                                    "de.csv", sep = "_"))
            if (!(all(is.na(de_temp$mean_exp)))) {
                write.table(de_temp, file = file,
                            quote = FALSE, sep = ",")
            }
        }
    }
}
                           
#' Read a folder of csv files generated by write.de()
#'
#' Reads a folder of csv files generated by write.de()
#' into a list of lists of dataframes containing the
#' DE marker analysis results. The top list is sorted
#' by cluster. Then each sub list is sorted by batch.
#' Each sub list contains the DE analysis results
#' for that cluster in that batch.
#'
#' @param folder The name of the folder holdin the csv
#'   files. It must not contain anything else. Additionally
#'   the files must be named in the style that write.de()
#'   writes them, i.e. "batch_B_cluster_C_de.csv" where
#'   B and C are the batch and cluster number.
#' @param batch_prefix A character prefix to prepend to the
#'   batch number when the names are being generated for
#'   the elements of the sub lists.
#' @param cluster_prefix A character prefix to prepend to the
#'   cluster number when the names are being generated for the
#'   top list.
#' @param batch_index The index of the batch number in the file
#'   name, when the file name is split by "_".
#' @param cluster_index The index of the cluster number in the
#'   file name, when the file name is split by "_".
#' @return A list of lists of dataframes. The top list has
#'   C elements where C is the number of metaclusters. Each
#'   element is a sub list. Each sub list has B elements where
#'   B is the number of batches with cells in that cluster. Each
#'   sub list's element is a data frame containing the DE
#'   analysis results for the cells in that cluster in that
#'   batch. The results are mean_exp, log2fc, de_pval, de_roc,
#'   pct_in, and pct_out for each gene.
#' @export
read_de_results <- function(folder, batch_prefix = "batch_",
                            cluster_prefix = "cluster_",
                            batch_index = 2, cluster_index = 4) {
    tables <- list.files(folder)
    tables <- tables[grepl("de\\.csv$", tables)]
    split <- strsplit(tables, "_")
    batches <- sapply(split, "[", batch_index)
    clusters <- sapply(split, "[", cluster_index)
    combos <- data.frame(batches = batches,
                         clusters = clusters,
                         filenames = tables,
                         stringsAsFactors = FALSE)
    combos <- combos[order(combos$clusters, combos$batches), ]
    batches <- unique(combos$batches)
    clusters <- unique(combos$clusters)
    de_lists <- vector("list", length = length(clusters))
    names(de_lists) <- paste0(cluster_prefix, clusters)
    names(clusters) <- names(de_lists)
    for (i in names(de_lists)) {
        de_lists[[i]] <- vector("list", length = dim(combos[combos$clusters == clusters[i], ])[1])
        names(de_lists[[i]]) <- paste0(batch_prefix, combos[combos$clusters == clusters[i], "batches"])
        files <- combos[combos$clusters == clusters[i], "filenames"]
        names(files) <- names(de_lists[[i]])
        for (j in names(de_lists[[i]])) {
            de_lists[[i]][[j]] <- read.table(file.path(folder, files[j]),
                                             header = TRUE, sep = ",",
                                             row.names = 1, stringsAsFactors = FALSE)
            if (all(is.na(de_lists[[i]][[j]]$mean_exp))) {
                de_lists[[i]][[j]] <- NULL
            }
        }
    }
    return(de_lists)
}
                           
#' Build a DE binary matrix from de_lists
#'
#' Converts the \code{de_lists} object from Maggie's DE pipeline
#' to a genes x metaclusters binary matrix, where 1 indicates that
#' a gene is DE in that metacluster.
#'
#' @param de_lists The \code{de_lists} object.
#' @param min_fc The minimum fold change threshold
#' @param max_pval The maximum p-value threshold
#' @param min_roc The minimum ROC threshold
#' @param max_bad_batches The maximum number of batches that a gene
#'   may not meet thresholds and still be called DE. Example: if 
#'   max_bad_batches is 0, then the gene must meet these thresholds
#'   in all batches to be called DE. If max_bad_batches is 1, then the
#'   gene may fail the thresholds in 1 out of N batches and still be
#'   called DE. Can also be a vector the same length as de_lists.
#' @return The genes x metacluster binary matrix
#' @export
build_binary_mat <- function(de_lists, min_fc = NULL, max_pval = NULL, min_roc = NULL, max_bad_batches = 0) {
    if (is.null(min_roc) && (is.null(min_fc) || is.null(max_pval))) {
        stop("You must use min_fc&max_pval or min_roc or both!")
    }
    if (!is.null(min_roc) && (xor(is.null(min_fc), is.null(max_pval)))) {
        warning("min_fc and max_pval are being ignored because one of them is NULL and min_roc is available!")
        min_fc <- NULL
        max_pval <- NULL
    }
    cluster_names <- names(de_lists)
    # Pre-allocate Matrix
    bin_mat <- matrix(0, nrow = dim(de_lists[[1]][[1]])[1], ncol = length(cluster_names))
    batch_n <- sapply(de_lists, length)
    # Fill matrix 1 metacluster at a time
    for (i in seq_along(de_lists)) {
        mc <- de_lists[[i]]
        # Check the thresholds for all genes in each batch in one metacluster
        if (is.null(min_roc)) {
            batch_status <- sapply(mc, function(batch) {
                (
                    (batch$log2fc >= min_fc)
                  & (batch$de_pval <= max_pval)
                )
            })
        } else if (is.null(min_fc)) {
            # I don't need to check max_pval because of the input validation
            batch_status <- sapply(mc, function(batch) {
                (
                  (batch$de_roc >= min_roc)
                )
            })
        } else { # Both sets of args are passed
            batch_status <- sapply(mc, function(batch) {
                (
                    (batch$log2fc >= min_fc)
                  & (batch$de_pval <= max_pval)
                  & (batch$de_roc >= min_roc)
                )
            })
        }
            
        # Count the number of batches each gene passed thresholds in
        batch_status <- apply(batch_status, 1, function(gene) {
            sum(gene, na.rm = TRUE)
        })
        # Report whether or not each gene passed thresholds in enough batches
        bin_mat[, i] <- ((batch_n[i] - batch_status) <= max_bad_batches)
    }
    rownames(bin_mat) <- rownames(de_lists[[1]][[1]])
    colnames(bin_mat) <- cluster_names
    return(bin_mat)
}

#' Calculate a "differential expression" test on read depth for clusters.
#'
#' @export
calc_cluster_depth_de_vs_all_cells <- function(sce_obj, clust_ids, fc_thresh=0.5,
                                         p_thresh=1E-4, method=c("mw", "ttest"),
                                         missing_clusts = FALSE) {
    if (is.character(clust_ids) && length(clust_ids == 1) &&
            clust_ids %in% colnames(SingleCellExperiment::colData(sce_obj))) {
        clust_ids <- sce_obj[[clust_ids]]
    }
    if (missing_clusts) {
        l <- length(levels(clust_ids))
        clusts <- sort(as.numeric(levels(clust_ids)))
    } else {
        l <- length(unique(clust_ids))
        clusts <- sort(as.numeric(unique(clust_ids)))
    }
    clust.mean <- t(apply(matrix(sce_obj$UMIs, nrow=1), 1,
                          function(z) {sapply(split(z, clust_ids), sum)}))
    
    logFC <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
    rownames(logFC) <- rownames(clust.mean)
    colnames(logFC) <- clusts
    for (i in 1:l) {
        logFC[, i] <- log2(clust.mean[, i] + 1) - log2(base::rowMeans(matrix(clust.mean[, -i], nrow = 1), na.rm = TRUE) + 1)
    }
    
    if (method == "mw") {
        pvals <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
        rownames(pvals) <- rownames(clust.mean)
        colnames(pvals) <- clusts
        for (i in 1:l) {
            f <- (clust_ids == clusts[i])
            pvals[,i] <- apply(matrix(sce_obj$UMIs, nrow=1),
                               1, function(x) {wilcox.test(x[f], x[!f], alternative="g")$p.value})
        }
    }
    else {
        pvals <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
        rownames(pvals) <- rownames(clust.mean)
        colnames(pvals) <- clusts
        for (i in 1:l) {
            f <- (clust_ids == i)
            temp <- genefilter::rowttests(matrix(sce_obj$UMIs, nrow=1), as.factor(f))
            pvals[, i] <- temp[, 3]
        }
    }
    
    gene.mat <- matrix(0, ncol=l, nrow=dim(clust.mean)[1])
    rownames(gene.mat) <- rownames(clust.mean)
    colnames(gene.mat) <- clusts
    for (i in 1:l) {
      f <- (abs(logFC[, i]) > fc_thresh) & (pvals[, i] < p_thresh)
      gene.mat[f, i] <- 1
    }

    summary <- data.frame(DE = gene.mat[1, ],
                          pval = pvals[1, ],
                          logFC = logFC[1, ])
    
    res <- list(clust_exp=clust.mean, deg = gene.mat,
                pval = pvals, logfc = logFC, summary = summary)
    return(res)
}
