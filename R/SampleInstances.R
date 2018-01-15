#' Sample instances (without replacement) from the set of available tuning
#' instances
#'
#' Returns a randomly sampled subset of instances, so that repetitions do not
#' occur. If the number of available instances in `instance.list` is smaller
#' than `N`, it will return a vector containing less than `N` indices (it can
#' also return an empty vector, if all instances have already been sampled)
#'
#' @param instance.list list containing all available tuning instances.
#' @param N number of instances to sample.
#' @param sampled.instances vector of indices of all instances that have
#'                          already been sampled.
#'
#' @return a vector (of size between zero and N) of instance indices
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br}),
#'         Athila Trindade (\email{rochaathila@@gmail.com})
#'

SampleInstances <- function(instance.list,
                            N,
                            sampled.instances = numeric(0)){

  ## ==============
  ## Error checking done in the calling routine
  ## ==============

  allindx <- seq_along(instance.list)
  if(length(sampled.instances) != 0) allindx <- allindx[-sampled.instances]

  N <- min(N, length(allindx))

  instance.indx <- sample(allindx, size = N, replace = FALSE)

  return(instance.indx)
}
