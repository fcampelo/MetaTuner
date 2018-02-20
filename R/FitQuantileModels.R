#' Fit quantile regression models
#'
#' Fits quantile regression models to the performance data gathered by
#' MetaTuner.
#'
#' @inheritParams FitModels
#' @param weights vector of observation weights returned by [GetWeights()].
#'
#' @return list containing information about all models fitted
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'

FitQuantileModels <- function(X,
                            Nmodels,
                            model.order,
                            weights){

  # ========== Fit "original" model
  modelDF     <- Inf
  model.order <- model.order + 1
  myX         <- X[, -ncol(X)]
  while (modelDF >= nrow(X)){
    model.order <- model.order - 1
    myformula   <- do.call(stats::polym, c(myX,
                                           degree = model.order,
                                           raw    = TRUE))
    modelDF <- ncol(myformula) + 1
  }
  ff <- stats::as.formula(paste("perf ~ poly(",
                                paste0(names(myX), collapse = ", "),
                                ", degree = ", model.order, ", raw = TRUE)"))

  mymodel   <- quantreg::rq(ff, data = X, weights = weights)
  mycoefs   <- quantreg::summary.rq(mymodel, se = "ker")$coefficients


  # Generate perturbed models
  all.models            <- vector(mode = "list", length = Nmodels)
  all.models[[1]]$model <- mymodel
  all.models[[1]]$order <- model.order
  names(all.models)     <- c("original",
                             paste0("perturbed",
                                    seq(Nmodels - 1)))

  for (i in 2:Nmodels){
    newmodel       <- mymodel
    newcoefs       <- mycoefs
    coefnoise      <- (-1 + 2 * stats::runif(nrow(newcoefs))) * mycoefs[, 2]
    newcoefs[, 1]  <- mycoefs[, 1] + coefnoise
    newcoefs[1, 1] <- mycoefs[1, 1] # no need to disturb the intercept

    newmodel$coefficients <- newcoefs[, 1]
    all.models[[i]]$model <- newmodel
    all.models[[i]]$order <- model.order
  }
  return(all.models)
}
