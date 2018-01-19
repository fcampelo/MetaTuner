#' Fit linear regression models
#'
#' Fits linear regression models to the performance data gathered by MetaTuner.
#'
#' @inheritParams FitModels
#'
#' @return list containing information about all models fitted
#'
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'

FitLinearModels <- function(X,
                            Nmodels,
                            model.order){

  # ========== Fit "original" model
  modelDF     <- Inf
  model.order <- model.order + 1
  myX         <- X[, -ncol(X)]
  while (modelDF >= nrow(X)){
    model.order <- model.order - 1
    myformula <- do.call(polym, c(myX,
                                  degree = model.order,
                                  raw    = TRUE))
    modelDF <- ncol(myformula) + 1
  }
  ff <- as.formula(paste("perf ~ poly(",
                         paste0(names(myX),collapse=", "),
                         ", degree = ",model.order, ", raw = TRUE)"))

  mymodel   <- stats::lm(ff, data = X)
  mycoefs   <- stats::summary.lm(mymodel)$coefficients


  # ========== Generate perturbed models
  all.models            <- vector(mode = "list", length = Nmodels)
  all.models[[1]]$model <- mymodel
  all.models[[1]]$order <- model.order
  names(all.models)     <- c("original",
                             paste0("perturbed",
                                    seq(Nmodels - 1)))

  for (i in 2:Nmodels){
    newmodel      <- mymodel
    newcoefs      <- mycoefs
    coefnoise     <- (-1 + 2 * runif(nrow(newcoefs))) * mycoefs[, 2]
    newcoefs[, 1] <- mycoefs[, 1] + coefnoise
    newcoefs[1, 1] <- mycoefs[1, 1] # no need to disturb the intercept

    newmodel$coefficients <- newcoefs[, 1]
    all.models[[i]]$model <- newmodel
    all.models[[i]]$order <- model.order
  }
  return(all.models)
}
