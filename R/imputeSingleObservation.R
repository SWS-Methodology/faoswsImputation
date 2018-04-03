##' This function imputes series where just one protected value is available. Please note that
##' also (M,-) flag combination is protected. 
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
  param = imputationParameters
  data[, `:=`("obsCount",
              sum(!is.na(.SD[[param$imputationValueColumn]]))),
       by = c(param$byKey)]
  data[obsCount == 1 & get(param$imputationFlagColumn)!="M" & get(param$imputationMethodColumn)!="-",  ## this is to exclude that (M,-) figures are overwritten
       `:=`(c(param$imputationValueColumn,
              param$imputationFlagColumn,
              param$imputationMethodColumn),
            list(as.numeric(na.omit(.SD[[param$imputationValueColumn]])),
                 replace(.SD[[param$imputationFlagColumn]],
                         which(.SD[[param$imputationFlagColumn]] ==
                                 param$missingFlag), param$imputationFlag),
                 replace(.SD[[param$imputationMethodColumn]],
                         which(.SD[[param$imputationFlagColumn]] ==
                                 param$missingFlag), param$newMethodFlag))),
       by = c(param$byKey)]
  data[, `:=`(obsCount, NULL)]
  data
}
