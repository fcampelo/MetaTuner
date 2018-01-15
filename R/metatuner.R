
metatuner <- function(algo.runner,
                      parameters,
                      tuning.instances,
                      initial.sampling.method,
                      m0, mi, elite.confs,
                      N0, Ni,
                      stopcrit,
                      summary.function,
                      budget,
                      parameter.resolution = 3){

  # =========== Error checking and initial definitions
  if(length(parameter.resolution) == 1) {
    parameter.resolution <- rep(parameter.resolution, times = nrow(parameters))
  }

  # To be included




  # =========== Prepare config.list structure
  config.list        <- vector(mode = "list", length = 2)
  names(config.list) <- c("A", "nruns")
  config.list$nruns  <- 0

  # =========== Generate initial sample of configurations
  config.list$A <- GenerateInitialSample(m0       = m0,
                                         dim      = nrow(parameters),
                                         method   = initial.sampling.method,
                                         ndigits  = parameter.resolution)

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
                          type        = model.type)

    # Optimize models
    cat("\n-----")
    newconfs <- OptimizeModels(parameters          = parameters,
                               models              = models,
                               optimization.method = optimization.method,
                               ndigits             = parameter.resolution)

    # Remove redundant configurations and add new ones to the archive
    newconfs <- FilterRepeatedConfigurations(newconfs, config.list)

    # Add new configurations to archive and evaluate on all instances so far.
    toEval        <- length(config.list$A) + seq(length(newconfs))
    config.list$A <- c(config.list$A, newconfs)

    cat("\n-----\nEvaluating new candidate configurations")
    config.list <- EvaluateConfigurations(tuning.instances  = tuning.instances,
                                          instances.to.eval = Gamma.A,
                                          config.list       = config.list,
                                          configs.to.eval   = toEval,
                                          algo.runner       = algo.runner,
                                          parameters        = parameters)

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
