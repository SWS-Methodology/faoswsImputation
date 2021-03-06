% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleEqually.R
\name{sampleEqually}
\alias{sampleEqually}
\title{Helper function for makeCvGroup}
\usage{
sampleEqually(n, k)
}
\arguments{
\item{n}{The total number of values to be sampled.}

\item{k}{The values to be sampled from (1:k).}
}
\value{
A vector of length n that contains values in 1:k.
}
\description{
This function is designed to ensure that creation of cross-validation
groups are well-balanced.  Suppose there are k groups and n observations.
If n is a multiple of k, each group is represented exactly n/k times.
Otherwise, each group is represented either floor(n/k) or floor(n/k)+1
times.
}

