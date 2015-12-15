##' The default moving average model.
##' 
##' This module is very simple: it uses a mean of the last 3 observed (non-NA) 
##' values to predict the next value.  If there aren't 3 previous values, the
##' number available are used.  If there aren't any previous values, the global
##' mean is imputed (imputing NA could cause issues with the ensemble weighting
##' process).
##' 
##' @param x A numeric vector to be imputed.
##'   
##' @examples
##' defaultMovingAverage(x = c(10, 10, 10, NA, 1, NA))
##' defaultMovingAverage(x = c(NA, 1:5, NA))
##' defaultMovingAverage(x = c(NA, 1, NA, 2, NA, 3, NA, 4, NA, 5, NA))
##' 
##' 
##' @export

defaultMovingAverage = function(x){

    ### Data Quality Checks
    stopifnot(is.numeric(x))
    stopifnot(length(x) > 1)
    lookback = 3
    
    ## If lookback > number of observations, we can't do a rolling mean
    if(sum(!is.na(x)) < lookback){
        return(rep(NA_real_, length(x)))
    }

    index = 1:length(x)
    meanEst = zoo::rollmean(x[!is.na(x)], na.rm = TRUE, k = lookback, align = "right")
    # rollmean won't work at the start (when there are fewer values than
    # lookback).  In those cases, use the first value.
    meanEst = c(rep(meanEst[1], lookback-1), meanEst)
    meanEst = data.frame(meanEst, index = index[!is.na(x)])
    fit = sapply(index, function(ind){
        usedIndex = which.max(meanEst$index[meanEst$index < ind])
        if(length(usedIndex) == 0){
            ## Global mean:
            return(mean(x, na.rm = TRUE))
        }
        return(meanEst[usedIndex, "meanEst"])
    })
    return(fit)
}
