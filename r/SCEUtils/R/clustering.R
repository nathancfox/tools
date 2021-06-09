#' Perform Louvain Clustering.
#'
#' Perform clustering on a cell x PCA matrix using the
#' Louvain Community Detection algorithm.
#'
#' @param pca_mat The cell x PCA matrix.
#' @param k The number of neighbors to use in the nearest
#'   neighbor graph.
#' @param process_communities If TRUE, the communities object
#'   returned by the igraph clustering functions will be processed
#'   into a custom list designed for Nathan's workflow.
#' @param name A string to be used for \code{name} if process_communities
#'   is TRUE.
#' @return If process_communities, a custom list containing 4 members.
#'   A vector of length n_cells containing the cluster assignments for
#'   the cells called "clusters". A vector of unique cluster names
#'   called "cluster_names". A string containing an optional name for
#'   this clustering result. A list of named properties, indicating the
#'   tunable parameters used for this particular clustering. If
#'   process_communities is \code{FALSE}, the igraph communities object
#'   is returned.
#' @export
cluster_louvain <- function(pca_mat, k = 100,
                            process_communities = TRUE,
                            name = "louvain") {
    nng <- cccd::nng(x = pca_mat, k = k, method = "Euclidean", mutual = TRUE)
    communities <- igraph::cluster_louvain(nng)
    if (process_communities) {
        return(process_igraph_communities(communities, name = name,
                                          properties = list(k = k)))
    } else {
        return(communities)
    }
}

#' Perform Walktrap Clustering.
#'
#' Perform clustering on a cell x PCA matrix using the
#' Walktrap Community Detection algorithm.
#'
#' @param pca_mat The cell x PCA matrix.
#' @param k The number of neighbors to use in the nearest
#'   neighbor graph.
#' @param mutual If TRUE, an undirected mutual nearest neighbors
#'   graph will be constructed and used. If FALSE, a directed
#'   nearest neighbors graph with no mutual constraint will be used.
#' @param steps Passed to the \code{steps} argument of the
#'   \code{igraph::cluster_walktrap} function. Should be the length
#'   of the random walks to perform.
#' @param process_communities If TRUE, the communities object
#'   returned by the igraph clustering functions will be processed
#'   into a custom list designed for Nathan's workflow.
#' @param name A string to be used for \code{name} if process_communities
#'   is TRUE.
#' @return If process_communities, a custom list containing 4 members.
#'   A vector of length n_cells containing the cluster assignments for
#'   the cells called "clusters". A vector of unique cluster names
#'   called "cluster_names". A string containing an optional name for
#'   this clustering result. A list of named properties, indicating the
#'   tunable parameters used for this particular clustering. If
#'   process_communities is \code{FALSE}, the igraph communities object
#'   is returned.
#' @export
cluster_walktrap <- function(pca_mat, k = 100, mutual = TRUE,
                             steps = 4, process_communities = TRUE,
                             name = "walktrap") {
    nng <- cccd::nng(x = pca_mat, k = k, method = "Euclidean", mutual = mutual)
    communities <- igraph::cluster_walktrap(nng, steps = steps)
    if (process_communities) {
        return(process_igraph_communities(communities, name = name,
                                          properties = list(k = k, mutual = mutual,
                                                            steps = steps)))
    } else {
        return(communities)
    }
}

#' Perform Infomap Clustering.
#'
#' Perform clustering on a cell x PCA matrix using the
#' Infomap Community Detection algorithm.
#'
#' @param pca_mat The cell x PCA matrix.
#' @param k The number of neighbors to use in the nearest
#'   neighbor graph.
#' @param mutual If TRUE, an undirected mutual nearest neighbors
#'   graph will be constructed and used. If FALSE, a directed
#'   nearest neighbors graph with no mutual constraint will be used.
#' @param n_trials Passed to the \code{nb.trials} argument of the
#'   \code{igraph::cluster_infomap} function. Should be the number
#'   of attempts to partition the network (any int >= 1).
#' @param process_communities If TRUE, the communities object
#'   returned by the igraph clustering functions will be processed
#'   into a custom list designed for Nathan's workflow.
#' @param name A string to be used for \code{name} if process_communities
#'   is TRUE.
#' @return If process_communities, a custom list containing 4 members.
#'   A vector of length n_cells containing the cluster assignments for
#'   the cells called "clusters". A vector of unique cluster names
#'   called "cluster_names". A string containing an optional name for
#'   this clustering result. A list of named properties, indicating the
#'   tunable parameters used for this particular clustering. If
#'   process_communities is \code{FALSE}, the igraph communities object
#'   is returned.
#' @export
cluster_infomap <- function(pca_mat, k = 100, mutual = TRUE,
                            n_trials = 10, process_communities = TRUE,
                            name = "infomap") {
    nng <- cccd::nng(x = as.matrix(pca_mat), k = k, method = "Euclidean", mutual = mutual)
    communities <- igraph::cluster_infomap(nng, nb.trials = n_trials)
    if (process_communities) {
        return(process_igraph_communities(communities, name = name,
                                          properties = list(k = k, mutual = mutual,
                                                            n_trials = n_trials)))
    } else {
        return(communities)
    }
}

#' Perform Leiden Clustering.
#'
#' Perform clustering on a cell x PCA matrix using the
#' Leiden Community Detection algorithm. Note that this
#' method is unlike the other clustering functions because
#' it has to interface with a helper python script. It is
#' much more fragile than the others, which are fully
#' implemented in R. This also means that it uses a vector
#' of cluster assignments, not an igraph communities object.
#'
#' @param pca_mat The cell x PCA matrix.
#' @param k The number of neighbors to use in the nearest
#'   neighbor graph.
#' @param mutual If TRUE, an undirected mutual nearest neighbors
#'   graph will be constructed and used. If FALSE, a directed
#'   nearest neighbors graph with no mutual constraint will be used.
#' @param process_communities If TRUE, the clusters returned by the
#'   helper python script will be processed into a custom list designed
#'   for Nathan's workflow.
#' @param name A string to be used for \code{name} if process_communities
#'   is TRUE.
#' @return If process_communities, a custom list containing 4 members.
#'   A vector of length n_cells containing the cluster assignments for
#'   the cells called "clusters". A vector of unique cluster names
#'   called "cluster_names". A string containing an optional name for
#'   this clustering result. A list of named properties, indicating the
#'   tunable parameters used for this particular clustering. If
#'   process_communities is \code{FALSE}, the vector of cluster
#'   assignments is returned.
#' @export
cluster_leiden <- function(pca_mat, k = 100, mutual = TRUE,
                           process_communities = TRUE, name = "leiden",
                           reindex = TRUE) {
    if (mutual) {
        nng <- cccd::nng(x = pca_mat, k = k, method = "Euclidean", mutual = )
    }
    timestamp <- as.character(as.numeric(Sys.time()))
    igraph::write_graph(nng, paste("/tmp/", timestamp,
                                   "_leiden_graph.gml", sep = ""),
                        "graphml")
    system(paste("/home/nfox/miniconda3/bin/python ",
                 "/home/nfox/toolbox/r_tools/SCEUtils/R/leiden.py ",
                 timestamp, sep = ""))  
    clusters <- read.table(paste("/tmp/", timestamp,
                                 "_leiden_clusters.csv", sep = ""),
                           header = FALSE, sep = ",")[[1]]
    if (reindex) {
        for (i in seq_along(clusters)) {
            clusters[i] <- as.numeric(clusters[i]) + 1
        }
    }
    if (process_communities) {
        clusters <- factor(clusters, levels = sort(as.numeric(unique(clusters))))
        cluster_names <- levels(clusters)
        return(list(clusters = clusters, cluster_names = cluster_names,
                    name = name, properties = list(k = k, mutual = mutual)))
    } else {
        return(clusters)
    }
}

#' Process an igraph Communities Object.
#'
#' Process an igraph communities object into a custom list containing
#' clustering results for Nathan's pipeline.
#'
#' @param comm The igraph communities object.
#' @param name The name attribute to be used for the clustering results object.
#' @param properties The list of clustering parameters to be stored in the properties
#'   attribute of the clustering results object.
#' @return A list with 4 members. clusters = the vector of cluster assignments
#'   for all cells. cluster_names = the vector of unique cluster names. 
#'   name = a string naming this object. properties = a list of named parameters
#'   intended to store the parameters used in this particular clustering.
#' @export
process_igraph_communities <- function(comm, name = NULL, properties = NULL) {
    clusters <- igraph::membership(comm)
    clusters <- factor(clusters, levels = sort(as.numeric(unique(clusters))))
    cluster_names <- levels(clusters)
    return(list(clusters = clusters, cluster_names = cluster_names,
                name = name, properties = properties))
}

#' Print a summary of a clustering results object.
#'
#' Prints a summary of a clustering results object produced
#' by \code{process_igraph_communities}.
#'
#' @param cluster_results A clustering results object produced
#'   by \code{process_igraph_communities}.
#' @param max_print If there are more clusters than this number,
#'   the individual clusters will not be printed.
#' @return Nothing. Just prints to stdout.
#' @export
print_clusters <- function(cluster_results, max_print = 50) {
    cat(cluster_results$name, "\n",
        paste(rep("=", nchar(cluster_results$name)), collapse = ""), "\n\n", sep = "")
    n_clusters <- length(cluster_results$cluster_names)
    cat(sprintf("%d Cells\n%d Clusters\n\n",
                length(cluster_results$clusters), n_clusters))
    if (n_clusters > max_print) {
        cat(sprintf("There are %d clusters! Too many to print!\n", n_clusters))
    } else {
        f_string <- paste("Cluster %0", floor(log10(n_clusters)) + 1, "d : %d\n", sep = "")
        for (i in 1:n_clusters) {
            cat(sprintf(f_string, as.numeric(cluster_results$cluster_names[i]),
                                  sum(cluster_results$clusters == cluster_results$cluster_names[i])))
        }
    }
}

#' Reindex cluster labels to 1:N.
#'
#' Reindexes a vector of cluster labels to the labels 1:N where N
#' is the number of unique clusters. Use to clean up clusters
#' that have been subset and skip labels. Must be used on numeric
#' cluster labels.
#'
#' @param clusters Vector of numeric cluster labels. Can be numeric,
#'   character, or factor.
#' @return A numeric vector of the same length as \code{clusters}
#'   containing the reindexed cluster labels.
#' @export
reindex_clusters <- function(clusters) {
    suppressWarnings(
        transformed_clusters <- factor(clusters,
        levels = sort(as.numeric(unique(as.character(clusters)))))
    )
    if (any(is.na(transformed_clusters))) {
        stop("non-numeric values in clusters!")
    }
    transformed_clusters <- as.numeric(transformed_clusters)
    return(transformed_clusters)
}

#' Calculate the Adjusted Rand Index for two clustering partitions.
#'
#' Calculate the Adjusted Rand Index for two clustering partitions,
#' returning a value that reflects the similarity of the clustering
#' partitions. The two vectors must be alternate clustering partitions
#' for the same entities.
#'
#' @param clusters_1 A vector of cluster labels indicating the first
#'   partition to compare.
#' @param clusters_2 A vector of cluster labels indicating the second
#'   partition to compare.
#' @return The Adjusted Rand Index of the two cluster partitions.
#' @export
calc_ari <- function(clusters_1, clusters_2) {
    if (length(clusters_1) != length(clusters_2)) {
        stop("cluster vectors must be the same length!")
    }
    clusters_1 <- reindex_clusters(clusters_1)
    clusters_2 <- reindex_clusters(clusters_2)
    ari <- igraph::compare(clusters_1, clusters_2, method = "adjusted.rand")
    return(ari)
}

#' Visualize a cluster dendrogram
#'
#' Plot a cluster dendrogram showing the hierarchical relationships
#' between the clusters in the given cell partition. Can evaluate
#' cluster relationships with euclidean distance or spearman
#' correlation in expression space.
#'
#' @param sce_obj The SingleCellExperiment object
#' @param partition The cell labels indicating the cluster. Must be a
#'   vector of length number_of_cells or a column name in colData.
#' @param genes The genes to use in computing the cell correlations.
#'   Must be a vector of genes that are all rownames in \code{sce_obj}.
#'   Default is all genes.
#' @param method Method of cluster comparison. Must be "euclidean" or
#'   "spearman", referring euclidean distance or spearman correlation.
#' @param title The title for the plot.
#' @export
hclust_dendro <- function(sce_obj, partition, genes = NULL, method = c("euclidean", "spearman"),
                          title = "Cluster Dendrogram") {
    if (is.character(partition) && length(partition) == 1) {
        if (!(partition %in% colnames(SingleCellExperiment::colData(sce_obj)))) {
            stop("partition must be a vector of length dim(sce_obj)[2] or a column name in colData!")
        } else {
            partition <- SingleCellExperiment::colData(sce_obj)[, partition]
        }
    }
    if (length(partition) != dim(sce_obj)[2]) {
        stop("partition must be a vector of length dim(sce_obj)[2] or a column name in colData!")
    }
    if (is.null(genes)) {
        genes <- 1:(dim(sce_obj)[1])
    } else if (!all(genes %in% rownames(sce_obj))) {
        stop("genes must all be rownames in sce_obj!")
    }
    partition_means <- sapply(split(colnames(sce_obj), partition), function(x) {
                              apply(SingleCellExperiment::logcounts(sce_obj)[genes, x], 1, mean)
                       })
    if (method == "euclidean") {
        dist <- dist(t(partition_means), diag = TRUE)
    } else if (method == "spearman") {
        dist <- as.dist(cor(partition_means, method = "spearman"))
    } else {
        stop("method must be \"euclidean\" or \"spearman\"!")
    }
    plot(hclust(dist), main = title)
}
