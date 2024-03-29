% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{hclust_dendro}
\alias{hclust_dendro}
\title{Visualize a cluster dendrogram}
\usage{
hclust_dendro(
  sce_obj,
  partition,
  genes = NULL,
  method = c("euclidean", "spearman"),
  title = "Cluster Dendrogram"
)
}
\arguments{
\item{sce_obj}{The SingleCellExperiment object}

\item{partition}{The cell labels indicating the cluster. Must be a
vector of length number_of_cells or a column name in colData.}

\item{genes}{The genes to use in computing the cell correlations.
Must be a vector of genes that are all rownames in \code{sce_obj}.
Default is all genes.}

\item{method}{Method of cluster comparison. Must be "euclidean" or
"spearman", referring euclidean distance or spearman correlation.}

\item{title}{The title for the plot.}
}
\description{
Plot a cluster dendrogram showing the hierarchical relationships
between the clusters in the given cell partition. Can evaluate
cluster relationships with euclidean distance or spearman
correlation in expression space.
}
