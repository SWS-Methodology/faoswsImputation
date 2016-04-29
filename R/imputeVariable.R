##' Function to impute production or yield
##' 
##' This is a wrapper of the ensemble imputation for the production domain.
##' 
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation 
##'   algorithms.  See defaultImputationParameters() for a starting point.
##'   
##' @return This function doesn't return any objects but modifies the underlying
##'   data.table that it was passed.
##'   
##' @export
##' 
##' @import data.table
##' 

imputeVariable = function(data, imputationParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)

    ## Define which columns should store the imputation and flags, and create
    ## those columns if they don't currently exist.
    if(imputationParameters$newImputationColumn == ""){
        newValueColumn = imputationParameters$imputationValueColumn
        newObsFlagColumn = imputationParameters$imputationFlagColumn
        newMethodFlagColumn = imputationParameters$imputationMethodColumn
    } else {
        newValueColumn = paste0("Value_",
                                imputationParameters$newImputationColumn)
        newObsFlagColumn = paste0("flagObservationStatus_",
                                imputationParameters$newImputationColumn)
        newMethodFlagColumn = paste0("flagMethod_",
                                imputationParameters$newImputationColumn)
    }
    newVarianceColumn = "ensembleVariance"

    ## NOTE (Michael): We impute variables which has only single
    ##                 observation with the only observed
    ##                 value. Although not a good practice but
    ##                 explaination is offered in issue 8.
    data = imputeSingleObservation(data)
    
    missingIndex = is.na(
        data[, get(imputationParameters$imputationValueColumn)])
    data[, c(newValueColumn, newVarianceColumn) := 
             ensembleImpute(data = data,
                            imputationParameters = imputationParameters)]
    imputedIndex = missingIndex & !is.na(data[[newValueColumn]])
    invisible(data[imputedIndex,
                   c(newObsFlagColumn, newMethodFlagColumn) :=
                       list(imputationParameters$imputationFlag,
                            imputationParameters$newMethodFlag)])
}
