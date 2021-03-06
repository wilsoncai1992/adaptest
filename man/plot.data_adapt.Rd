% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{plot.data_adapt}
\alias{plot.data_adapt}
\title{Plot method for data_adapt objects}
\usage{
\method{plot}{data_adapt}(x, ..., plot_type = c("biomarker",
  "adapt_param"))
}
\arguments{
\item{x}{(data_adapt) - object of class \code{data_adapt} as returned by
\code{adaptest}}

\item{...}{additional arguments passed to \code{plot} as necessary}

\item{plot_type}{character vector specifying which of the two types of plots
to generate: "biomarker" for a plot sorted average CV-rank, or "adapt_param"
for a plot sorted by q-values with labels corresponding to indices}
}
\value{
plot of model statistics
}
\description{
Customized plotting method for easily examining data-adaptive statistics
}
