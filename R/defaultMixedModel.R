##'
##' This function imputes missing values with a linear mixed model.
##'
##' The default functionality of this model is to fit a linear mixed model to
##' the data.  time and intercept are assumed to be fixed effects, and the
##' random effects are specified by the byKey parameter of
##' imputationParameters.  So, for example, byKey may reference the variable
##' containing country data.  In that case, a model is fit which assumes a
##' linear relationship between production (or whatever dependent variable the
##' user has specified) and time.  However, the intercept and slope of this
##' fit varies from country to country, and so country is considered a random
##' effect.
##'
##' Moreover, the model fit is not a simple linear regression, but rather a
##' spline regression (using the bs function from the \pkg{splines} package).
##' The fit of this model will therefore depend on the number of degrees of
##' freedom of this spline model (and, if the degrees of freedom is 1, then the
##' simple linear regression model is used).
##'
##' @param data The data.table object containing the data.
##' @param df The number of degrees of freedom for the spline.
##' @param weights The weights for the observations.
##' @param modelFormula Formula specifying how the dependent variable (value)
##' depends on the other columns of data.  Should be a valid mixed model
##' formula, as it will be passed to lmer (R's mixed model function).
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##'
##' @return Returns a vector of the estimated/imputed values.  If a value
##' existed in the original data, then an NA is returned in that location.
##'
##' @export
##'
##' @importFrom lme4 lmer
##' @import splines
##'

defaultMixedModel = function(data, df = 1, weights = NULL, modelFormula = NULL,
                             imputationParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    uniqueByKey = data[!is.na(get(imputationParameters$imputationValueColumn)),
                       1, by = c(imputationParameters$byKey)]
    if(nrow(uniqueByKey) <= 1) # Mixed model invalid if only one level:
        return(rep(NA_real_, nrow(data)))
    if("byKey" %in% colnames(data))
        stop("defaultMixedModel assumes 'byKey' is not a column name of data")

    ## Make the key into one column by pasting all key columns together with
    ## a very unique symbol (:-: shouldn't show up in other keys on accident).
    byKey = apply(data[, imputationParameters$byKey, with = FALSE],
                  1, function(x) paste(x, collapse = " :-: "))
    data[, byKey := byKey]
    if(is.null(modelFormula)){
        modelFormula = as.formula(paste0(
            imputationParameters$imputationValueColumn,
            "~ -1 + (1 + splines::bs(", imputationParameters$yearValue,
            ", df = ", df, ", degree = 1)|byKey)"))
        model = try(
            lme4::lmer(formula = modelFormula,
                       data = data,
                       weights = weights,
                       REML = FALSE)
            )
    } else {
        model = try(
            lme4::lmer(formula = modelFormula, data = data,
                       weights = weights,
                       REML = FALSE)
            )
    }

    if(!inherits(model, "try-error")){

        data[, allMissing :=
                   all(is.na(get(imputationParameters$imputationValueColumn))),
             by = byKey]
        nonMissingIndex = which(!data$allMissing)
        ## Impute the data with lme.
        modelFit = rep(NA, NROW(data))
        modelFit[nonMissingIndex] =
            predict(model, newdata = data[nonMissingIndex, ],
                    allow.new.levels = FALSE)
        modelFit = pmax(modelFit, 0)
    } else {
        modelFit = rep(NA_real_, nrow(data))
    }

    if("byKey" %in% colnames(data))
        data[, `:=`(c("byKey", "allMissing"), list(NULL, NULL))]

    return(modelFit)
}
