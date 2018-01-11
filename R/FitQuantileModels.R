#' Fit quantile regression models
#'
#' Fits quantile regression models to the performance data gathered by
#' MetaTuner.
#'
#' @inheritParams FitModels
#'
#' @return list containing information about all models fitted
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'
#' @export

FitQuantileModels <- function(X,
                            perturbed.models,
                            model.order){

  # Fit "original" model
  myformula <- paste0("perf ~ .^", model.order)
  mymodel   <- quantreg::rq(as.formula(myformula), data = X)
  mycoefs   <- summary(mymodel)$coefficients


  # Generate perturbed models
  all.models        <- vector(mode = "list", length = perturbed.models + 1)
  all.models[[1]]   <- mymodel
  names(all.models) <- c("original",
                         paste0("perturbed",
                                seq(perturbed.models)))

  for (i in (seq(perturbed.models) + 1)){
    newmodel      <- mymodel
    newcoefs      <- mycoefs
    coefnoise     <- (-1 + 2 * runif(nrow(newcoefs))) * mycoefs[, 2]
    newcoefs[, 1] <- mycoefs[, 1] + coefnoise
    newcoefs[1, 1] <- mycoefs[1, 1] # no need to disturb the intercept

    newmodel$coefficients <- newcoefs[, 1]
    all.models[[i]]       <- newmodel
  }
  return(all.models)
}
