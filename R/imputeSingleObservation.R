##' Make Cross-Validation Groups
##' 
##' Creates a vector of cross-validation groups to be used for leave-one-out
##' cross-validation in later models.
##' 
##' @param data A data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' 
##' @return A new data table with time series containing sinlge
##'     observation imputed.
##' 
##' @export
##' 

imputeSingleObservation = function(data, imputationParameters){
    dataCopy = copy(data)
    param = imputationParameters
    dataCopy[, `:=`("obsCount",
                    sum(!is.na(.SD[[param$imputationValueColumn]]))),
             by = c(param$byKey)]

    dataCopy[obsCount == 1,
             `:=`(c(param$imputationValueColumn,
                  param$imputationFlagColumn,
                  param$imputationMethodColumn),
                  list(na.omit(.SD[[param$imputationValueColumn]]),
                       replace(.SD[[param$imputationFlagColumn]],
                               which(.SD[[param$imputationFlagColumn]] ==
                                     param$missingFlag), param$imputationFlag),
                       replace(.SD[[param$imputationMethodColumn]],
                               which(.SD[[param$imputationFlagColumn]] ==
                                     param$missingFlag), param$newMethodFlag))),
             by = c(param$byKey)]
    dataCopy[, `:=`(obsCount, NULL)]
    dataCopy
}
             
    
