% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_de_results}
\alias{plot_de_results}
\title{Plot a volcano plot or an MA plot of DE results}
\usage{
plot_de_results(
  de_res,
  plot = c("volcano", "ma", "auroc_fc"),
  ct_markers = NULL,
  correct = c("BH", "bonferroni", "none"),
  plot_title = "DE Results",
  alpha = NULL,
  mean_expression = NULL
)
}
\arguments{
\item{de_res}{The DE results. A data.frame from the CSV file written by \code{write_de()}.}

\item{plot}{Indicates if the plot should be a volcano or MA plot.}

\item{ct_markers}{An optional vector of genes that are known markers
and should be indicated as such.}

\item{correct}{Indicates the method for multiple hypothesis test correction.}

\item{plot_title}{The string used as the title for the plot.}

\item{alpha}{The significance cut off value. p-values below this
will be labeled significant. Only applicable in auroc_fc or MA plots.}

\item{mean_expression}{A vector of mean gene expression values for
the genes in \code{de_res}, to be plotted in the MA plot. Default
is the mean expression across the cells in the cluster being
tested, as reported in \code{de_res}.}
}
\description{
Designed to work with the output of functions in \code{diff_exp.R}
}
