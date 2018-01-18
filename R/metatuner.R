#' Metaheuristics tuner
#'
#' Search for parameter configurations that are
#' expected to yield the best performance for a given optimizer on instances
#' belonging to the same problem class as those used in the tuning effort.
#' Currently the method only works for parameters that are box-constained,
#' continuous, and Real.
#'
#' @section Tunable Parameters:
#' The parameters to be tuned are passed to `metatuner` as a data frame
#' containing three columns:
#' - `name`, with a unique name for each parameter.
#' - `minx`, with the smallest allowed value for each parameter.
#' - `maxx`, with the largest allowed value for each parameter.
#'
#'
#' @section Tuning Instances:
#' The problem instances available for the tuning effort must be passed as
#' list vector `tuning.instances`, in which each position contains the following
#' fields:
#' - `FUN`, the name of the function that implements the instance. Must
#'          have a formal argument `X` (e.g., `myfun <- function(X){...}`),
#'          which can be either a numeric vector or a matrix of row vectors.
#' - `xmin`, the lower bound for the optimization variables
#' - `xmax`, the upper bound for the optimization variables
#' - `alias` (optional), an alias for the instance
#'
#'
#' @section Algorithm Runner:
#' The `algo.runner` parameter points to a function that receive the instance
#' configuration (i.e., one position of the list vector `tuning.instance`) and
#' a numeric vector with the values of the tunable parameters (e.g.,
#' `myalgo <- function(instance, params){...}`), and return a single scalar
#' quantifying the performance of the algorithm equipped with that configuration
#' for that particular instance.
#'
#' **IMPORTANT**: `metatuner` assumes one wants to _minimize_ the expected
#' output of `algo.runner`. When working with quality indicators that should be
#' maximize, please do not forget to multiply this output by `-1` in the
#' output of `algo.runner`.
#'
#'
#' @section Initial Sampling Methods:
#' `metatuner` generates the initial sampling of the space of configurations
#' using the method given in `initial.sampling`. Currently available methods
#' are:
#' - Latin hypercube sampling ("lhs"): a space-covering design based on Latin
#'   Hypercube Sampling (uses package `lhs`).
#' - Low-discrepancy sequences of points ("sobol"): a space-covering design
#'   using Sobol's low-discrepancy sequences of points (uses package
#'   `SobolSequence`).
#'
#'
#' @section Statistical Modeling:
#' Parameter `model.type` informs `metatuner` of the type of regression model to
#' fit. Currently implemented alternatives are:
#' - `linear` for linear regression using OLS.
#' - `quantile` for quantile regression (uses package `quantreg`).
#'
#' In all cases `metatuner` fits a polynomial model to the performance data
#' gathered from running the candidate configurations on the tuning instances.
#' Parameter `model.order` is used to inform the order of the polynomial model
#' to be fitted.
#'
#'
#' @section Optimization Methods:
#' `metatuner` uses [stats::constrOptim()] to optimize the predicted quality of
#' `algo.runner` with regards to its tunable parameters. Any method that does
#' not require an explicit function for returning gradient / Hessian information
#' can be used as `optimization.method`. Currently we recommend using
#' `Nelder-Mead`, but `SANN` is also a (usually slower) possibility.
#'
#' @param parameters data frame containing the parameter names and bound
#'        constraints. See Section *Tunable Parameters* for details.
#' @param tuning.instances list of instances available for tuning. See Section
#'        *Tuning Instances* for details.
#' @param algo.runner name of function used for evaluating the configurations.
#'        See Section *Algorithm Runner* for details.
#' @param m0 initial number of configurations to be generated. Defaults to
#'        `3 * nrow(parameters)`.
#' @param mi number of new configurations to generate at each iteration.
#'        Defaults to `5`.
#' @param initial.sampling type of method to be used in the generation of the
#'        initial sample. See Section *Initial Sampling Methods* for details.
#'        Defaults to "lhs".
#' @param ndigits integer with the number of digits to use for each parameter.
#'        Accepts a vector input, if different resolutions are desired for
#'        different parameters. Defaults to `3`.
#' @param elite.confs number of elite configurations to maintain at each
#'        iteration.
#' @param N0 initial number of instances to sample. Defaults to `5`.
#' @param Ni number of new instances to add at each iteration. Defaults to `1`.
#' @param summary.function name of function for aggregating the (scaled)
#'        output values of `algo.runner` into a single performance value. Usual
#'        values include "mean" and "median", but in principle any
#'        summarizing function can be used.
#' @param model.type Type of regression model to use. See Section
#'        *Statistical Models* for details.
#' @param model.order Order of the model to fit. Defaults to `3`.
#' @param optimization.method optimization method to use for estimating new
#'        configurations. See Section *Optimization Methods* for details.
#'        Defaults to "Nelder-Mead".
#' @param budget number of algorithm runs allocated for the tuning effort.
#'
#' @export

metatuner <- function(parameters,
                      tuning.instances,
                      algo.runner,
                      m0                  = 3 * nrow(parameters),
                      mi                  = 5,
                      initial.sampling    = "lhs",
                      ndigits             = 3,
                      elite.confs,
                      N0                  = 5,
                      Ni                  = 1,
                      summary.function    = "median",
                      model.type          = "quantile",
                      model.order         = 3,
                      optimization.method = "Nelder-Mead",
                      budget){

  # =========== Input standardization
  initial.sampling <- match.arg(initial.sampling, c("lhs", "sobol"))
  model.type       <- match.arg(model.type, c("linear", "quantile"))

  if(length(ndigits) == 1) {
    ndigits <- rep(ndigits, times = nrow(parameters))
  }


  # =========== Error checking
  SanityCheck(as.list(environment()))

  # =========== Prepare config.list structure
  config.list        <- vector(mode = "list", length = 2)
  names(config.list) <- c("A", "nruns")
  config.list$nruns  <- 0

  # =========== Generate initial sample of configurations
  config.list$A <- GenerateInitialSample(m0       = m0,
                                         dim      = nrow(parameters),
                                         method   = initial.sampling,
                                         ndigits  = ndigits)

  # =========== Sample instances
  Gamma.A <- SampleInstances(instance.list = tuning.instances,
                             N             = N0)

  # =========== Evaluate configurations in config.list on the sampled instances
  cat("\nEvaluate initial configurations on initial sample of instances")
  config.list <- EvaluateConfigurations(tuning.instances  = tuning.instances,
                                        instances.to.eval = Gamma.A,
                                        config.list       = config.list,
                                        configs.to.eval   = "all",
                                        algo.runner       = algo.runner,
                                        summary.function  = summary.function,
                                        parameters        = parameters)
  cat("\n-----")

  elite.list  <- seq(config.list$A)

  # =========== Iterative cycle:
  keep.running <- TRUE
  iteration.counter <- 0
  while(keep.running){
    iteration.counter <- iteration.counter + 1
    cat("\n Iteration ", iteration.counter)
    # Sample Ni additional instances
    Gamma.A <- c(Gamma.A,
                 SampleInstances(instance.list     = tuning.instances,
                                 sampled.instances = Gamma.A,
                                 N                 = Ni))

    # Evaluate elite configurations on the new instances
    cat("\nEvaluating elite configurations on new instances")
    config.list <- EvaluateConfigurations(tuning.instances  = tuning.instances,
                                          instances.to.eval = Gamma.A,
                                          config.list       = config.list,
                                          configs.to.eval   = elite.list,
                                          algo.runner       = algo.runner,
                                          parameters        = parameters)

    # Build statistical models
    models   <- FitModels(X           = config.list$config.perf,
                          Nmodels     = mi,
                          model.order = model.order,
                          model.type  = model.type)

    # Optimize models
    cat("\n-----")
    newconfs <- OptimizeModels(parameters          = parameters,
                               models              = models,
                               optimization.method = optimization.method,
                               ndigits             = ndigits)

    # Remove redundant configurations and add new ones to the archive
    newconfs <- FilterRepeatedConfigurations(newconfs, config.list)

    # Add new configurations to archive and evaluate on all instances so far.
    if (length(newconfs) > 0) {
      toEval        <- length(config.list$A) + seq(length(newconfs))
      config.list$A <- c(config.list$A, newconfs)

      cat("\n-----\nEvaluating new candidate configurations")
      config.list <- EvaluateConfigurations(tuning.instances  = tuning.instances,
                                            instances.to.eval = Gamma.A,
                                            config.list       = config.list,
                                            configs.to.eval   = toEval,
                                            algo.runner       = algo.runner,
                                            parameters        = parameters)
    } else cat("\n-----\nNo new candidates to evaluate")

    # Determine elite configurations
    elite.list <- order(config.list$config.perf$perf)[1:elite.confs]

    # Check stop criteria
    keep.running <- config.list$nruns < budget

    cat("\n-----\nTotal runs at the end of iteration ", iteration.counter,
        ": ", config.list$nruns, "\n-----")
  }

  # =========== Prepare return structures
  # don't forget config denormalization!

  # =========== Return output

}
