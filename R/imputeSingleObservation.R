imputeSingleObservation = function(data, imputationParameters){
    dataCopy = copy(data)
    param = imputationParameters$
    dataCopy[, `:=`(c("obsCount"),
                    sum(is.na(.SD[[param$imputationValueColumn]]))),
             by = c(param$byKey)]

    dataCopy[obsCount == 1,
             `:=`(.SD[[param$imputationValueColumn]],
                  na.omit(.SD[[param$imputationValueColumn]])),
             by = c(param$byKey)]
    dataCopy[, `:=`(obsCount, NULL)]
    dataCopy
}
             
    
