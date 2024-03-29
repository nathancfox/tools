% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dim_red.R
\name{run_pca}
\alias{run_pca}
\title{Runs PCA.}
\usage{
run_pca(sce_obj, hvg, n_pcs = 100, assay = "logcounts", full_results = TRUE)
}
\arguments{
\item{sce_obj}{The SingleCellExperiment object.}

\item{hvg}{The highly variable genes to include.}

\item{n_pcs}{The number of PCs to calculate.}

\item{assay}{The named assay to use from the SCE.}

\item{full_results}{If TRUE, the full results from the
\code{rpca} function will be returned. If FALSE, only the
cells x PCA matrix will be returned.}
}
\value{
Either the full results list or the PCA matrix.
}
\description{
Runs the \code{rsvd::rpca} function on a subset of the
expression data in an SingleCellExperiment object.
}
