#' Fit regression model to performance data
#'
#' Fits regression models to the performance data gathered by MetaTuner.
#' Returns the model fitted to the data, plus a number of models obtained by
#' perturbing the regression coefficients within certain error bounds.
#'
#' @param X data frame containing the performance data
#' @param perturbed.models number of perturbed models to generate
#' @param model.order order to use for the regression model. Defaults to 2
#' @param type type of model to fit. `linear` for linear regression using OLS,
#'             `quantile` for quantile regression of the median
#' @param ... other parameters to be passed down to specific fitting functions
#'
#' @return list containing information about all models fitted
#'
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'
#' @export

FitModels <- function(X,
                      perturbed.models,
                      model.order = 2,
                      type = c("linear", "quantile"),
                      ...){

  # ========== Error checking
  type <- match.arg(type, c("linear", "quantile"))
  assertthat::assert_that(is.data.frame(X),
                          all(sapply(X, is.numeric)),
                          assertthat::is.count(perturbed.models),
                          assertthat::is.count(model.order),
                          )


  # ========== Fit model (original)
  if (type == "linear"){
    models <- FitLinearModel(X,
                             perturbed.models,
                             model.order)
  } else if (type == "quantile"){
    models <- FitQuantileModel(X,
                             perturbed.models,
                             model.order)
  }

}
