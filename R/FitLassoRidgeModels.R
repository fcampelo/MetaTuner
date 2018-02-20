#' Fit lasso or ridge regression models
#'
#' Fits lasso or ridge regression models to the performance data gathered by MetaTuner.
#'
#' @inheritParams FitModels
#'
#' @return list containing information about all models fitted
#'
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'

FitLassoRidgeModels <- function(X,
                                Nmodels,
                                model.order,
                                model.type = c("lasso", "ridge")){

  # ========== Fit "original" model
  modelDF     <- Inf
  model.order <- model.order + 1
  myX         <- X[, -ncol(X)]
  while (modelDF >= nrow(X)){
    model.order <- model.order - 1
    myformula <- do.call(stats::polym, c(myX,
                                         degree = model.order,
                                         raw    = TRUE))
    modelDF <- ncol(myformula) + 1
  }

  model.type <- match.arg(model.type, c("lasso", "ridge"))
  alpha      <- as.integer(model.type == "lasso")

  invisible(utils::capture.output(mymodel <- hqreg::cv.hqreg(myformula,
                                                             X$perf,
                                                             alpha = alpha)))
  mycoefs <- stats::coef(mymodel, mymodel$lambda.min)

  # If lasso model generate all coefficients equal to zero
  if (all((mycoefs[-1] == 0)))
    stop("All coefficients are equal to zero when generating a Lasso/Ridge
            model. Try using more data points (increasing m0 / mi) or
            changing the type of regression model")



  # ========== Calculate uncertainty for coefficients of original model

  if (model.type=="lasso") {
      myformula <- as.matrix(myformula[, which(mycoefs[-1] != 0)])
      colnames(myformula) <- names(which(mycoefs[-1] != 0))
  }

  matcoefs <- matrix(nrow = nrow(X), ncol = ncol(myformula) + 1)

  # generate auxiliary models with M-1 data points
  for(i in 1:nrow(X)){
    # modelLOO             <- hqreg::hqreg(myformula[-i, ],
    #                                      X$perf[-i],
    #                                      alpha = alpha,
    #                                      nlambda = 2,
    #                                      lambda = c(0, 0))
    invisible(utils::capture.output(modelLOO <- hqreg::cv.hqreg(myformula[-i, ],
                                                                X$perf[-i],
                                                                alpha = alpha)))
    matcoefs[i, ] <- stats::coef(modelLOO, modelLOO$lambda.min)
    #apply(modelLOO$beta, FUN = mean, MARGIN = 1)
  }

  SEcoefs <- apply(matcoefs, FUN = stats::sd, MARGIN = 2)


  # ========== Generate perturbed models
  # Prepare all.models list
  all.models            <- vector(mode = "list", length = Nmodels)
  myL <- c(min(mymodel$lambda.min + 0.01, 1),
           mymodel$lambda.min,
           max(mymodel$lambda.min - 0.01, 0))
  all.models[[1]]$model <- hqreg::hqreg(myformula,
                                        X$perf,
                                        alpha = alpha,
                                        lambda = myL)
  all.models[[1]]$order <- model.order
  names(all.models)     <- c("original",
                             paste0("perturbed",
                                    seq(Nmodels - 1)))
  # Perturb model
  SEmat   <- matrix(SEcoefs,
                    nrow     = nrow(all.models[[1]]$model$beta),
                    ncol     = ncol(all.models[[1]]$model$beta),
                    byrow    = FALSE,
                    dimnames = dimnames(all.models[[1]]$model$beta))
  for (i in 2:Nmodels){
    newmodel      <- all.models[[1]]$model
    mynoise       <- matrix(stats::runif(length(SEmat)),
                            nrow = nrow(SEmat),
                            ncol = ncol(SEmat))
    newmodel$beta <- newmodel$beta + (-1 + 2 * mynoise) * SEmat

    # no need to disturb the intercept
    newmodel$beta[1, ] <- all.models[[1]]$model$beta[1, ]

    all.models[[i]]$model <- newmodel
    all.models[[i]]$order <- model.order
  }

  return(all.models)

}
