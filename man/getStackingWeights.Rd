% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStackingWeights.R
\name{getStackingWeights}
\alias{getStackingWeights}
\title{Get Ensemble Weights via Stacking}
\usage{
getStackingWeights(data, fits, imputationParameters)
}
\arguments{
\item{data}{The data object containing the observations to impute.}

\item{fits}{A list of the fitted values.  These may be estimated via leave
one out cross-validation or directly.}

\item{imputationParameters}{A list of the parameters for the imputation
algorithms.  See defaultImputationParameters() for a starting point.}
}
\value{
A data.table containing the weight for each model within each byKey
group, as well as a few other (currently unused) statistics.
}
\description{
Weights used to construct the final ensemble from the individual models are
computed via stacking.
}
\details{
Traditional stacking proceeds as follows: For each individual byKey
(usually country) a set of weights is chosen for the ensemble.  The weights
are constrained to be positive and to sum to 1, and the final ensemble is
constructed by sum(w_i*model_i).  The weights should be chosen in a way
such that better models are given more importance.  Thus, the following
criteria is minimized:

sum(errorFunction(|y - w_i*model_i|))

i.e. the errorFunction applied to the difference between the observed
values and the ensemble estimate.  The errorFunction is typically just x^2,
but could be a more complex function.

However, this is roughly equivalent to regression, with a constraint added.
In some cases, however, our datasets will be so sparse that we won't be
able to perform this optimization (only four observations and 7 valid
models, for example, will not have a unique solution).  Thus, we instead
use a LASSO regression for computing the stacking weights.

Note: if errorType is not "loocv", then stacking will be problematic: much
higher weights will be given to flexible models (such as loess or splines)
without valid reason.
}

