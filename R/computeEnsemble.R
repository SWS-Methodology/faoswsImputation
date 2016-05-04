##' Function to combine the ensembles
##'
##' @param fits A list of fitted values from models.
##' @param weights A weight matrix giving the weights of each model for each
##' observation.
##' @param errors An error matrix giving the error of each model for each
##' observation.  This is used to create an estimate of variability of the
##' ensemble for each imputed value.
##'
##' @return A vector of length nrow(weights), where each value represents the
##' ensemble estimate for that observation.
##'
##' @export
##'

computeEnsemble = function(fits, weights, errors){

    ## Data quality checks
    ## Rearrange elements of fits if needed
    stopifnot(all(names(fits) %in% names(weights)))
    stopifnot(all(names(weights) %in% names(fits)))
    fits = fits[names(weights)]
    stopifnot(all(names(weights) == names(fits)))
    stopifnot(length(fits) == ncol(weights))
    if(!all(sapply(fits, length) == nrow(weights)))
        stop("Length of fits do not match nrow(weights)!")

    fitsMatrix = matrix(unlist(fits), ncol = length(fits))
    weightedFit = fitsMatrix * weights
    errorFit = errors * weights
    ensemble = data.table(
        fit = apply(weightedFit, 1, function(x) sum(x, na.rm = !all(is.na(x)))),
        variance = apply(errorFit, 1, sum, na.rm = TRUE)
    )

    ## Test whether any model give negative imputation issue #13
    modelMin = apply(fitsMatrix, 2, min, na.rm = TRUE)
    if(any(modelMin < 0)){
        negMod = which(modelMin < 0)
        stop("Imputation gave negative result")
    }
    ensemble
}
