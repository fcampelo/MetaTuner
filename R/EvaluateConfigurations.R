#' Evaluate candidate configurations on sets of instances
#'
#' Evaluates the candidate configurations listed on `configs.to.eval` on the
#' tuning instances listed in `instances.to.eval`. Notice that this routine does
#' not repeat evaluations - if a configuration `i` already has a performance
#' value for instance `j` that evaluation is skipped.
#'
#' @param instance.list list containing all available tuning instances.
#' @param instances.to.eval vector containing the indices of the instances to
#'                          be used.
#' @param config.list list containing all configurations sampled.
#' @param configs.to.eval vector containing the indices of the configurations to
#'                          be evaluated on the instances listed in
#'                          `instances.to.eval`.
#' @param target.runner name of function used for evaluating the configurations.
#'                      See Section `Target Runner` of [metatuner()]
#'                      documentation.
#'
#' @return Updated `config.list` containing performance evaluation of
#' `configs.to.eval` on `instances.to.eval`, as well as `nruns`, the number of
#' algorithm runs performed.
#'
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'
#' @export

EvaluateConfigurations <- function(instance.list,
                                   instances.to.eval = "all",
                                   config.list,
                                   configs.to.eval = "all",
                                   target.runner){

  # Error checking
  assertthat::assert_that(is.list(instance.list),
                          length(instance.list) > 0,
                          assertthat::is.count(N),
                          is.numeric(sampled.instances),
                          all(lapply(sampled.instances, assertthat::is.count)),
                          all(sampled.instances <= length(instance.list)))

  allindx <- seq_along(instance.list)
  allindx <- allindx[-sampled.instances]

  N <- min(N, length(allindx))

  instance.indx <- sample(allindx, size = N, replace = FALSE)

  return(instance.indx)
}
