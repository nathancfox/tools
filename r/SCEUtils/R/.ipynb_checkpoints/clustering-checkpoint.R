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
    nng <- cccd::nng(x = pca_mat, k = k, method = "Euclidean", mutual = mutual)
    communities <- igraph::cluster_infomap(nng, nb.trials = n_trials)
    if (process_communities) {
        return(process_igraph_communities(communities, name = name,
                                          properties = list(k = k, mutual = mutual,
                                                            n_trials = n_trials)))
    } else {
        return(communities)
    }
}

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
