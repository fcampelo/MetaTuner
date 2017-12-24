
metatuner <- function(algo.runner,
                      parameters,
                      tuning.instances,
                      initial.sampling.method,
                      m0, mi,
                      N0, Ni,
                      stopcrit){

  # Error checking and initial definitions

  # ===========

  A <- GenerateInitialSample(m0       = m0,
                             dim      = nrow(parameters),
                             method   = initial.sampling.method)

  Gamma.A <- SampleInstances(instance.list = tuning.instances,
                             N = N0)

  A <- EvaluateConfigurations(tuning.instances = tuning.instances,
                              instances.to.eval = Gamma.A,
                              config.list = A,
                              configs.to.eval = "all",
                              algo.runner = algo.runner)

  elite.list <- seq_along(A)
  while(keep_running(nruns)){
    Gamma.A <- c(Gamma.A,
                 SampleInstances(instance.list = tuning.instances,
                                 sampled.instances = Gamma.A,
                                 N = Ni))
    A <- EvaluateConfigurations(tuning.instances = tuning.instances,
                                instances.to.eval = Gamma.A,
                                config.list = A,
                                configs.to.eval = elite.list,
                                algo.runner = algo.runner)
    nruns      <- A$nruns
    A          <- A$config.list

  }

}
