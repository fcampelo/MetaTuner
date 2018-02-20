#' Fit regression model to performance data
#'
#' Fits regression models to the performance data gathered by MetaTuner.
#' Returns the model fitted to the data, plus a number of models obtained by
#' perturbing the regression coefficients within certain error bounds.
#'
#' @param X data frame containing the performance data
#' @param Nmodels number of models to generate (1 original plus `Nmodels - 1`
#'                perturbations)
#' @param model.order order to use for the regression model. Defaults to 2
#' @param model.type type of model to fit. See [metatuner()] for details.
#' @param Yij.norm matrix of normalized performance values, returned by
#' [EvaluateConfigurations()].
#' @param ... other parameters to be passed down to specific fitting functions
#'
#' @return list containing information about all models fitted
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'

FitModels <- function(X,
                      Nmodels,
                      model.order = 3,
                      model.type = c("linear", "quantile", "lasso", "ridge"),
                      Yij.norm,
                      ...){

  ## ==============
  ## Error checking done in the calling routine
  ## ==============

  if ((model.type == "linear") || (model.type == "quantile")) {
    weights <- GetWeights(Yij.norm)
  }

  # ========== Fit model (original)
  if (model.type == "linear"){
    models <- FitLinearModels(X           = X,
                              Nmodels     = Nmodels,
                              model.order = model.order,
                              weights     = weights)
  } else if (model.type == "quantile"){
    models <- FitQuantileModels(X           = X,
                                Nmodels     = Nmodels,
                                model.order = model.order,
                                weights     = weights)
  } else if ((model.type == "lasso") || (model.type == "ridge")){
    models <- FitLassoRidgeModels(X           = X,
                                  Nmodels     = Nmodels,
                                  model.order = model.order,
                                  model.type  = model.type)
  }


  # ========== Return fitted model + disturbed models
  return(models)

}
