
metatuner <- function(algo.runner,
                      parameters,
                      tuning.instances,
                      initial.sampling.method,
                      m0, mi,
                      N0, Ni,
                      stopcrit,
                      summary.function){

  # Error checking and initial definitions

  # ===========

  config.list <- vector(mode = "list", length = 2)
  names(config.list) <- c("A", "nruns")
  config.list$nruns <- 0
  config.list$A     <- GenerateInitialSample(m0       = m0,
                                             dim      = nrow(parameters),
                                             method   = initial.sampling.method)

  Gamma.A     <- SampleInstances(instance.list = tuning.instances,
                                 N             = N0)

  config.list <- EvaluateConfigurations(tuning.instances  = tuning.instances,
                                        instances.to.eval = Gamma.A,
                                        config.list       = config.list,
                                        configs.to.eval   = "all",
                                        algo.runner       = algo.runner,
                                        summary.function  = summary.function)

  elite.list  <- seq_along(config.list$A)
  while(keep_running(nruns)){
    # Sample Ni additional instances
    Gamma.A <- c(Gamma.A,
                 SampleInstances(instance.list     = tuning.instances,
                                 sampled.instances = Gamma.A,
                                 N                 = Ni))

    # Evaluate elite configurations on the new instances
    config.list <- EvaluateConfigurations(tuning.instances  = tuning.instances,
                                          instances.to.eval = Gamma.A,
                                          config.list       = config.list,
                                          configs.to.eval   = elite.list,
                                          algo.runner       = algo.runner)

    # Build statistical models
    models <- FitModels(X                = config.list$config.perf,
                        perturbed.models = mi,
                        model.order      = model.order,
                        type             = model.type)

    A.prime


  }

}
