##' Check Ensemble Model
##'
##' @param object An S4 object.
##' 
##' @return TRUE if there are no errors, otherwise the error messages.
##' 

checkEnsembleModel = function(object){
    errors = character()
    if(object@extrapolationRange < 0){
        msg = "extrapolationRange can't be negative!"
        errors = c(errors, msg)
    }
    if(!object@level %in% c("global", "local")){
        msg = "level must be one of global or local."
        errors = c(errors, msg)
    }
    modelArguments = names(as.list(args(object@model)))
    if(object@level == "global"){
        requiredColumns = c("data", "imputationParameters")
        missing = requiredColumns[!requiredColumns %in% modelArguments]
        if(length(missing) > 1){
            msg = paste("model missing required arguments:",
                        paste(missing, collapse=", "))
            errors = c(errors, msg)
        }
    } else {
        # modelArguments should be the one argument and "", so length == 2
        if(length(modelArguments) != 2){
            msg = "Model should only contain one argument"
            errors = c(errors, msg)
        }
    }
    if(length(errors) == 0)
        TRUE
    else
        errors
}

##' Ensemble Model Class
##'
##' \describe{
##'     \item{\code{model}:}{The function defining how the model is fit to
##'     the data.  The function should take one or two arguments: data and
##'     imputationParameters (see extendSimpleModel) if level == "country"
##'     but only data if level == "global".}
##'     \item{\code{extrapolationRange}:}{How many time steps outside of
##'     the data is this model valid for?  Should be a positive integer
##'     (or Inf).  Defaults to 0.}
##'     \item{\code{level}:}{The level at which this model is applied. 
##'     Currently, must be one of "global" or "local".  This
##'     defines if this model operates on each "local" subset of the data
##'     (defined by the byKey) or all data at once ("global").  Defaults to
##'     "local".
##'     }
##' }
##' 
##' @import methods
##' 
##' @export ensembleModel
##' 

ensembleModel = methods::setClass(Class = "ensembleModel",
    representation = methods::representation(model = "function",
                                    extrapolationRange = "numeric",
                                    level = "character"),
    methods::prototype(extrapolationRange = 0, level = "local"),
    validity = checkEnsembleModel,
    package = "faoswsImputation")