##' Returns all default models
##'
##' This is a convenience function that returns a list of all the default
##' models.
##'
##' @return A list of all the default model functions.
##' 
##' @export
##' 

allDefaultModels = function(){
    return(list(defaultMean = ensembleModel(model = defaultMean,
                                            extrapolationRange = Inf,
                                            level = "local"),
                defaultLm = ensembleModel(model = defaultLm,
                                          extrapolationRange = Inf,
                                          level = "local"),
                defaultExp = ensembleModel(model = defaultExp,
                                           extrapolationRange = 2,
                                           level = "local"),
                defaultLogistic  = ensembleModel(model = defaultLogistic,
                                                 extrapolationRange = 1,
                                                 level = "local"),
                defaultLoess  = ensembleModel(model = defaultLoess,
                                              extrapolationRange = 1,
                                              level = "local"),
                defaultSpline  = ensembleModel(model = defaultSpline,
                                               extrapolationRange = 1,
                                               level = "local"),
                defaultArima  = ensembleModel(model = defaultArima,
                                              extrapolationRange = Inf,
                                              level = "local"),
                defaultMars  = ensembleModel(model = defaultMars,
                                             extrapolationRange = Inf,
                                             level = "local"),
                defaultNaive  = ensembleModel(model = defaultNaive,
                                              extrapolationRange = 0,
                                              level = "local"),
                defaultMixedModel = ensembleModel(model = defaultMixedModel,
                                                  extrapolationRange = Inf,
                                                  level = "global")))
}
