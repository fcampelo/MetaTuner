#' Calculate weights for regression
#'
#' Calculate weights to be used by Linear and Quantile regression models.
#'
#' @param X a matrix containing the (normalized) performance data of
#' configurations on instances.
#'
#' @return a vector of m values in the (0, 1) interval. Each value is associated
#' with a configuration, proportionally to the number of instances on which
#' it has been evaluated.
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'

GetWeights <- function(X){

  validperfs <- apply(X, function(X) {sum(!is.na(X))}, MARGIN = 2)
  weights    <- validperfs / max(validperfs)

  return(weights)
}
