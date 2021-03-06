% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logisticNlsIntercept.R
\name{logisticNlsIntercept}
\alias{logisticNlsIntercept}
\title{Helper function for defaultLogistic}
\usage{
logisticNlsIntercept(x)
}
\arguments{
\item{x}{The dependent variable.}
}
\value{
A numeric vector of the same length as x but with the model
estimates at each point in time.  Note: if the logistic model has it's
midpoint outside the range of the data, this function will return a vector
of NA's (as the original defaultLogistic function had this behavior to
prevent poor fitting).
}
\description{
This function fits a logistic model to the data via non-linear least
squares (?nls).  The function fit is:
x = A + B / (1 + exp(-C*(time-D)))
where time is the independent variable (1 to length(x)) and x is the
dependent variable.
}
\seealso{
Other logistic.functions: \code{\link{logisticGlm}},
  \code{\link{logisticNls}}
}

