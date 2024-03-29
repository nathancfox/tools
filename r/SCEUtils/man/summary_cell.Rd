% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{summary_cell}
\alias{summary_cell}
\title{Prints the str() of the colData from the SCE object.}
\usage{
summary_cell(sce_obj)
}
\arguments{
\item{sce_obj}{The SingleCellExperiment object.}
}
\description{
Gets the colData from the SCE object and transforms it from
an S4Vectors dataframe to a base dataframe. It is then fed
to \code{str()} to get a summary of the fields.
}
